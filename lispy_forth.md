# [vicsy/dev](https://github.com/codr4life/vicsydev) | a Lispy, embedded Forth
posted Feb 6th 2017, 05:00 pm

### preramble
I've been craving for a trivial, embedded scripting language that feels just right for a long, long time; something I can quickly drop into any project that needs scripting without too much ceremony. I'm aware of Lua & co., but that's still not the kind of trivial I'm aiming for. And since I'm mostly slinging Lisp these days, it should be possible to leverage some of it's power for a tighter integration.

### forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. While widely popular in embedded hardware circles, it's unfortunately mostly forgotten outside of that niche. Unfortunate because it's one of the most trivial languages to implement, and a solid foundation for embedded and/or domain specific languages. 

### lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. Besides being based on stacks and words, Lifoo is very much Lisp in Forth clothes; to the point where it reuses the Lisp reader to read Lifoo code.

```
Lifoo> 1 2 +

3

Lifoo> (1 2 +)

(1 2 +)

Lifoo> (1 2 +) eval

3

Lifoo> (1 2 +) compile

(PROGN (LIFOO-PUSH 1) (LIFOO-PUSH 2) (LIFOO-CALL '+))

Lifoo> (1 2 +) compile link

#<FUNCTION {1003F99E8B}>

Lifoo> (1 2 +) compile link call

3

Lifoo> (1 2 3) (2 *) map

(2 4 6)

Lifoo> ((1 2 +) (3 4 +) (5 6 +)) (eval) map

(3 7 11)

Lifoo> #(1 2 3) 1 nth 4 set drop

#(1 4 3)

Lifoo> "abc" 1 nth del

"ac"

Lifoo> "~a+~a=~a" (1 2 3) format

"1+2=3"

Lifoo> ((bar -1) baz) :foo struct
       nil make-foo foo?

T

Lifoo> (:frisbee throw
        "fail" error)
       catch

:FRISBEE

Lifoo> 0 chan 
       (1 2 + send :done)@ 1 spawn swap 
       recv swap drop swap 
       wait cons

(:DONE . 3)

Lifoo> (lifoo-push (concatenate 'string
                             (lifoo-pop)
                             (lifoo-pop)))@@
       (string string) :+ define
       "def" "abc" + 1 2 + cons

(3 . "abcdef")
```

### words
Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, modular set of built-in words. Words can be defined in either Lisp or Lifoo; functions for defining, looking up and un-defining words are also provided. 

```
(define-word :array (hash-table) ()
  list array)

(define-lisp-word :cmp (nil)
  (let ((lhs (lifoo-pop))
        (rhs (lifoo-pop)))
    (lifoo-push (compare lhs rhs))))

(define-macro-word :@ (in out)
  (cons (cons in
            `(lifoo-push (lambda ()
                           (lifoo-optimize)
                           ,@(lifoo-compile
                              (first (first out))))))
        (rest out))))
```

### repl
Lifoo provides a basic REPL, the implementation is provided as an example of how to integrate Lifoo into Lisp code.

```
LIFOO> (lifoo:lifoo-repl)
Welcome to Lifoo,
press enter on empty line to evaluate,
exit ends session

Lifoo> "hello Lifoo!" print ln

hello Lifoo!
NIL

Lifoo>


(defun lifoo-repl (&key (exec (lifoo-init t :exec (make-lifoo)))
                        (in *standard-input*)
                        (out *standard-output*))
  "Starts a REPL for EXEC with input from IN and output to OUT,
   using PROMPT"

  (format out "Welcome to Lifoo,~%")
  (format out "press enter on empty line to evaluate,~%")
  (format out "exit ends session~%")
  
  (with-lifoo (:exec exec)
    (tagbody
     start
       (format out "~%Lifoo> ")
       (let ((expr (with-output-to-string (expr)
                      (let ((line))
                        (do-while
                            ((not
                              (string= "" (setf line
                                                (read-line in
                                                           nil)))))
                          (when (string= "exit" line) (go end))
                          (terpri expr)
                          (princ line expr))))))
         (with-input-from-string (in expr)
           (restart-case
               (progn
                 (lifoo-reset)
                 (lifoo-eval (lifoo-read :in in) :throw? nil)
                 (write (lifoo-pop) :stream out)
                 (terpri out))
             (ignore ()
               :report "Ignore error and continue.")))
         (go start))
     end)))
```

### tracing
Stack tracing takes on a whole new meaning in Forth, Lifoo offers integrated tracing to help untangle messy stacks.

```
Lifoo> (number number) :+ word trace

NIL

Lifoo> "Every :+ entry and exit is traced from here" log

NIL

Lifoo> 1 2 +

3

Lifoo> untrace

NIL

Lifoo> "Nothing is traced from here" log

NIL

Lifoo> 3 4 +

7

Lifoo> print-trace

LOG   Nothing is traced from here
EXIT  + #(3)
ENTER + #(1 2)
LOG   Every :+ entry and exit is traced from here
NIL
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
