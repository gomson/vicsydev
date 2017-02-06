# [vicsy/dev](https://github.com/codr4life/vicsydev) | consing Forth
posted Feb 5th 2017, 02:00 pm

### preramble
In a previous [post](https://github.com/codr4life/vicsydev/blob/master/lispy_forth.md), I presented the humble beginnings of Lifoo; a Lispy, Forth-like language implemented in Common Lisp. This post goes further into specific features and the reasoning behind them. I decided from the start that this was going to be a fresh take on Forth, in the spirit of Lisp; taking nothing for granted; and I ran into plenty of interesting design choices as a result.

### repl
If you wan't to play along with the examples, a basic REPL may be started by cloning the [repository](https://github.com/codr4life/lifoo), followed by loading and evaluating ```(lifoo:lifoo-repl)```.

```
Welcome to Lifoo,
press enter on empty line to eval expr,
exit ends session

Lifoo> 1 2 +

3

Lifoo> exit

NIL
```

### reader
One of the goals set early on in the design process was to reuse the Lisp reader as is for reading Lifoo code. Looking back, sticking with this choice throughout hard times was fundamental to achieving a seamless integration since it acted as a natural obstacle to deviating from Lisp in other ways.

### quoting
Lifoo treats all list literals as quoted. When evaluating a list literal, the parser treats items as code tokens. The price for convenience is not being able to evaluate items in list literals without mapping eval or building from scratch, but the approach fits like a glove with the simplicity of Forth and plays nice with Lisp.

```
Lifoo> (1 2 3)

(1 2 3)

Lifoo> (1 2 3) (2 *) map

(2 4 6)

Lifoo> ((1 2 +) (3 4 +) (5 6 +)) (eval) map

(3 7 11)
```

### symbols
Since Forth doesn't use a special call syntax; symbols in the token stream are interpreted as words, functions calls. Luckily, Common Lisp offers another kind of symbols in the form of keywords. In Lifoo; regular symbols are evaluated as words, while keywords are treated as symbols.

```
Lifoo> "lifoo" symbol

:LIFOO

Lifoo> :lifoo

:LIFOO
```

### comparisons
Lisp reserves common operators for use with numbers, ```+-*/<>``` and more; Lifoo follows this tradition but provides generic compare operations rather than mirroring the ```eq(ua)(l)``` puzzle from Lisp which is too complex for such a simple language.

```
Lifoo> 1 2 + 3 =

T

Lifoo> "abc" "def" neq?

T

Lifoo> "def" "abc" lt?

T

Lifoo> '(1 2 3) '(1 2 3 4) cmp

1
```

### definitions
I decided to deviate from the traditional Forth syntax for defining words, a decision driven by the choice of reader. Lifoo provides a ```define``` word that defines preceding code and symbol as a word. ```define``` can be called anywhere; it overwrites any previous definitions, and makes the specified one available for immediate use.

```
Lifoo> (drop drop 42) :+ define
       1 2 +

42

Lifoo> (+ 1 2) :foo define
       :foo word source

(+ 1 2)
```

### setf
The beauty of ```setf``` is that it untangles specifying a place from setting its value. If you still can't see it; imagine writing a generic function that can set indexes in arrays and replace tails of lists in any other language; then add fields in structs and keys in hash tables; ```setf``` allows you to pull tricks like that without missing a beat; and on top of that you can hook your own places into the protocol. Lifoo provides a comparable ```set``` word that sets values for any place that's hooked in.

```
Lifoo> #(1 2 3) 1 nth 4 set drop

#(1 4 3)

Lifoo> clear :bar var 42 set env

((:BAR . 42))
```

### del
One thing Python got right (despite missing the ```setf``` train) was providing an extendable protocol for deletion. Separating concerns into pieces of generic functionality like this is what enables exponential power gains. Lifoo provides a ```del``` word that works like ```set``` but deletes places instead.

```
Lifoo> (1 2 3) 1 nth del drop

(1 3)

Lifoo> "abc" 1 nth del drop

"ac"
```

### structs
A programming language doesn't get far without the ability to define new types from within the language. Lifoo provides a simple but effective interface to defstruct. Outside of Lifoo the struct is anonymous to not clash with existing Lisp definitions. Words are automatically generated for ```make-foo```, ```foo-p``` and fields with setters when the ```struct``` word is evaluated.

```
Lifoo> ((bar -1) baz) :foo struct
       nil make-foo foo?

T

Lifoo> (:bar 42) make-foo
       foo-bar

42

Lifoo> (:bar 42) make-foo
       foo-bar 43 set
       foo-bar

43


(define-lisp-word :struct ()
  (let ((fields (lifoo-pop))
        (name (lifoo-pop)))
    (define-lifoo-struct name fields)))


(defmacro define-lifoo-struct (name fields)
  "Defines struct NAME with FIELDS"
  `(progn
     (let ((lisp-name (gensym))
           (fs ,fields))
       (eval `(defstruct (,lisp-name)
                ,@fs))
       (define-lifoo-struct-fn
           (keyword! 'make- ,name) (symbol! 'make- lisp-name)
         (lifoo-pop))
       (define-lifoo-struct-fn
           (keyword! ,name '?) (symbol! lisp-name '-p)
         (list (lifoo-peek)))
       (dolist (f fs)
         (let ((fn (if (consp f) (first f) f)))
           (define-lifoo-struct-fn
               (keyword! ,name '- fn) (symbol! lisp-name '- fn)
             (list (lifoo-peek)) :set? t))))))

(defmacro define-lifoo-struct-fn (lifoo lisp args &key set?)
  "Defines word LIFOO that calls LISP with ARGS"
  (with-symbols (_fn _sfn)
    `(let ((,_fn (symbol-function ,lisp))
           (,_sfn (and ,set? (fdefinition (list 'setf ,lisp)))))
       
       (define-lisp-word ,lifoo ()
         (lifoo-push
          (apply ,_fn ,args)
          :set (when ,set?
                 (lambda (val)
                   (lifoo-pop)
                   (funcall ,_sfn val (lifoo-peek)))))))))
```

### macros
Once token streams come on silver plates for free, the macro implementation picture changes drastically. I ended up with what is essentially Lisp macros with a touch of Forth. Like Lisp, Lifoo macros operate on streams of tokens. But since Forth is post-fix; macros deal with previously parsed, rather than wrapped, tokens. Lifoo provides macro words that are called to translate the token stream when code is parsed. A token stream consists of pairs of tokens and generated code, and the result of a macro call replaces the token stream from that point on.

```
(defmacro define-macro-word (name (in &key exec)
                             &body body)
  "Defines new macro word NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define-macro (keyword! ',name)
                       (lambda (,in)
                         ,@body)
                       :exec (or ,exec *lifoo*)))
```

### always, throw & catch
One of the features that was waiting for macros to arrive was throwing and catching. ```always``` is implemented as a macro that wraps the entire token stream in ```unwind-protect```, catch pulls the same trick with ```handler-case```, and ```throw``` signals a condition. If a thrown value isn't caught, an error is reported.

```
Lifoo> :frisbee throw
       "skipped" print ln
       (:always) always
       (drop) catch

:ALWAYS

Lifoo> :up throw
       "skipped" print ln
       (:caught cons) catch

(:CAUGHT . :UP)


(define-lisp-word :throw ()
  (lifoo-throw (lifoo-pop)))


(define-macro-word :always (in)
  (list
   (cons :always `(unwind-protect
                       (progn
                         ,@(reverse (mapcar #'rest (rest in))))
                    (lifoo-eval ',(first (first in)))))))

(define-macro-word :catch (in)
  (list
   (cons :catch `(handler-case
                   (progn
                     ,@(reverse (mapcar #'rest (rest in))))
                 (lifoo-throw (c)
                   (lifoo-push (value c))
                   (lifoo-eval ',(first (first in))))))))
```

### multi-threading
All Lifoo code runs in a ```lifoo-exec``` object, the result of accessing a ```lifoo-exec``` from multiple threads at the same time is undefined. Spawning new threads clones the current exec and [channels](http://vicsydev.blogspot.de/2017/01/channels-in-common-lisp.html) are used for communication.

```
Lifoo> 1 chan 42 send recv

42

Lifoo> 0 chan (1 2 + send :done) 1 spawn swap 
       recv swap drop swap 
       wait cons

(:DONE . 3)
```

### performance
The only thing I can say for sure so far is that it's slower than Lisp, yet fast enough for my needs without even trying. And it should be; since most code is pre-compiled down to Lisp, which is plenty fast. The reason structs are slower is that defining a struct in Lisp with accessors is a complex operation. Evaluate ```(cl4l-test:run-suite '(:lifoo) :warmup 10 :reps 100)``` after loading to get an idea of the speed on your setup.

```
CL-USER> (cl4l-test:run-suite '(:lifoo) :warmup 10 :reps 100)
(lifoo abc)                    0.34
(lifoo array)                 0.396
(lifoo compare)               0.256
(lifoo env)                   0.028
(lifoo error)                 0.072
(lifoo flow)                  1.192
(lifoo io)                    0.004
(lifoo list)                  0.244
(lifoo log)                   0.008
(lifoo meta)                  0.084
(lifoo stack)                 0.016
(lifoo string)                0.248
(lifoo struct)                1.228
(lifoo thread)                0.176
(lifoo word)                  0.084
TOTAL                         4.376
NIL
```

There is much left to be said, but this post needs to end somewhere. You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
