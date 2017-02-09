# [vicsy/dev](https://github.com/codr4life/vicsydev) | consing Forth
posted Feb 6th 2017, 05:00 pm

### preramble
In a previous [post](https://github.com/codr4life/vicsydev/blob/master/lispy_forth.md), I presented the humble beginnings of [Lifoo](https://github.com/codr4life/lifoo); a Lispy, Forth-like language implemented in Common Lisp. This post goes further into specific features and the reasoning behind them. I decided from the start that this was going to be a fresh take on Forth, in the spirit of Lisp; taking nothing for granted; and I ran into plenty of interesting choices as a result.

### repl
If you wan't to play along with the examples, a basic REPL may be started by cloning the [repository](https://github.com/codr4life/lifoo), followed by loading and evaluating ```(lifoo:lifoo-repl)```.

```
CL-USER> (lifoo:lifoo-repl)
Welcome to Lifoo,
press enter on empty line to evaluate,
exit ends session

Lifoo> 1 2 +

3

Lifoo> exit

NIL
CL-USER>
```

### reader
One of the goals set early on in the design process was to reuse the Lisp reader for reading Lifoo code. Looking back, sticking with this choice was fundamental to achieving a seamless integration since it acted as a natural obstacle to deviating from the Lisp way.

```
Lifoo> "1 2 +" read

(1 2 +)

Lifoo> "1 2 +" read eval

3

Lifoo> (1 2 +) write

"1 2 +"

Lifoo> (1 2 +) write read

(1 2 +)

Lifoo> (1 2 +) write read eval

3
```

### quoting
Lifoo treats all list literals as quoted. When evaluating a list literal, the parser treats items as code tokens. The price for convenience is not being able to evaluate items in list literals without mapping eval or building from scratch, but the approach fits like a glove with the simplicity of Forth and plays nice with Lisp. ```@``` pre-compiles the preceding list containing Lifoo code down to a Lisp lambda.

```
Lifoo> (1 2 3)

(1 2 3)

Lifoo> (1 2 3) (2 *) map

(2 4 6)

Lifoo> (2 *)@

#<FUNCTION {1008CD673B}>

Lifoo> (1 2 3) (2 *)@ map

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
Lisp reserves common operators for use with numbers, ```+-*/<>``` and more; Lifoo follows this tradition but also provides generic compare operators that work for numbers as well as any other Lifoo values.

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

### setf
The beauty of ```setf``` is that it untangles specifying a place from setting its value. If you still can't see it; imagine writing a generic function that can set indexes in arrays and replace tails of lists in any other language; then add fields in structs and keys in hash tables; ```setf``` allows you to pull tricks like that without missing a beat; and on top of that you can hook your own places into the protocol. Lifoo provides a comparable ```set``` word that sets values for any stack cell that's hooked in.

```
Lifoo> #(1 2 3) 1 nth 4 set drop

#(1 4 3)

Lifoo> nil hash :foo 42 put list

((:FOO . 42))

Lifoo> clear :bar var 42 set env

((:BAR . 42))


(define-lisp-word :set (t) ()
  (let* ((val (lifoo-pop))
         (cell (lifoo-peek-cell))
         (set (lifoo-set cell)))
    (unless set
      (error "missing set: ~a" val))
    (when-let (trans (lifoo-trans :exec exec))
      (let ((prev (lifoo-val cell)))
        (push (lambda ()
                (funcall set prev)
                (setf (lifoo-val cell) val))
              (on-rollback trans))))
    (funcall set val)
    (setf (lifoo-val cell) val)))
```

### del
One thing Python got right was providing an extendable protocol for deletion. Separating concerns into pieces of generic functionality like that is what enables exponential power gains. Lifoo provides a ```del``` word that works like ```set``` but deletes places instead.

```
Lifoo> (1 2 3) 1 nth del

(1 3)

Lifoo> "abc" 1 nth del

"ac"

Lifoo> ((:foo . 1) (:bar . 2)) hash :foo get del drop list

((:BAR . 2))


(define-lisp-word :del (t) ()
  (let* ((cell (lifoo-peek-cell))
         (val (lifoo-val cell))
         (del (lifoo-del cell)))
    (unless del
      (error "missing del: ~a" val))
    (when-let (trans (lifoo-trans :exec exec))
      (push (lambda ()
              (funcall (lifoo-set cell) val))
            (on-rollback trans)))
    (funcall del)))
```

### definitions
I decided to deviate from the traditional Forth syntax for defining words, since neither the Lisp reader nor I approve of that level of cleverness. Lifoo provides a ```define``` word that defines preceding code and symbol as a word. ```define``` can be called anywhere; overwrites any previous bindings, and makes the new definition available for immediate use.

```
Lifoo> (lifoo-push (concatenate 'string
                             (lifoo-pop)
                             (lifoo-pop)))@@
       (string string) :+ define
       "def" "abc" + 1 2 + cons

(3 . "abcdef")
```

### packages
Lifoo provides an extensible, tag-based init protocol. Words may be tagged with several different packages, and importing any package imports the word. Besides support for ```init```; packages are second class, and the only way of defining one is from Lisp. the good news is that Lisp is right around the corner as long as you remembered to load the ```meta``` package; and the REPL loads all packages by default.

```
Lifoo> (define-lifoo-init (:foo :bar)
         (define-word :baz () () 39 +))@@
       eval
       (:foo :bar) init
       3 baz

42


(define-macro-word :@@ (in out)
  (cons (cons in
            `(lifoo-push (lambda ()
                           (lifoo-optimize)
                           ,(first (first out)))))
      (rest out)))
      
      
(defmacro define-lifoo-init (tags &body body)
  "Defines init for TAGS around BODY"
  `(setf (gethash ',tags *lifoo-init*)
         (lambda (exec)
           (with-lifoo (:exec exec)
             ,@body))))
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
           (keyword! 'make- ,name)
           ()
         (symbol! 'make- lisp-name)
         (lifoo-pop))
       (define-lifoo-struct-fn
           (keyword! ,name '?)
           (t)
         (symbol! lisp-name '-p)
         (list (lifoo-peek)))
       (dolist (f fs)
         (let ((fn (if (consp f) (first f) f)))
           (define-lifoo-struct-fn
               (keyword! ,name '- fn)
               (t)
             (symbol! lisp-name '- fn)
             (list (lifoo-peek))
             :set? t))))))

(defmacro define-lifoo-struct-fn (lifoo lifoo-args
                                  lisp lisp-args
                                  &key set?)
  "Defines word LIFOO that calls LISP with ARGS"
  (with-symbols (_fn _sfn)
    `(let ((,_fn (symbol-function ,lisp))
           (,_sfn (and ,set? (fdefinition (list 'setf ,lisp)))))
       
       (define-lisp-word ,lifoo ,lifoo-args ()
         (lifoo-push
          (apply ,_fn ,lisp-args)
          :set (when ,set?
                 (lambda (val)
                   (funcall ,_sfn val (lifoo-peek :offs 1)))))))))
```

### macros
Once token streams come on silver plates, it's really hard to resist the temptation of going all in with macros. What I ended up with is essentially Lisp macros with a touch of Forth. Like Lisp, Lifoo macros operate on streams of tokens. But since Forth is post-fix; macros deal with previously parsed, rather than wrapped, tokens. Lifoo provides macro words that are called to translate the token stream when code is parsed. A token stream consists of pairs of tokens and generated code, and the result of a macro call replaces the token stream from that point on. 

```
Lifoo> (number number) :+ word macro?

NIL

Lifoo> (+ 1 2) compile

(PROGN (LIFOO-CALL '+) (LIFOO-PUSH 1) (LIFOO-PUSH 2))

Lifoo> :@ macro macro?

T

Lifoo> (+ 1 2)@

#<FUNCTION {100677E90B}>

Lifoo> ((+ 1 2)@) compile

(PROGN
        (DO-LIFOO-CALL ([macro word: @])
          (LIFOO-PUSH
           (LAMBDA ()
             (LIFOO-OPTIMIZE)
             (LIFOO-CALL '+)
             (LIFOO-PUSH 1)
             (LIFOO-PUSH 2)))))


(define-macro-word :@ (in out)
  (cons (cons in
            `(lifoo-push (lambda ()
                           (lifoo-optimize)
                           ,@(lifoo-compile
                              (first (first out))))))
        (rest out)))


(defmacro define-macro-word (id (in out &key exec)
                             &body body)
  "Defines new macro word NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define (make-lifoo-word ,id
                                  :macro? t
                                  :source ',body
                                  :fn (lambda (,in ,out)
                                        ,@body))
                 :exec (or ,exec *lifoo*)))
```

There is much left to be said, but this post needs to end somewhere. You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
