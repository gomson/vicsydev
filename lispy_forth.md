# [vicsy/dev](https://github.com/codr4life/vicsydev) | a Lispy, embedded Forth
posted Feb 4th 2017, 02:10 am

### preramble
I've been craving for a trivial, embedded scripting language that feels just right for a long, long time; something I can quickly drop into any project that needs scripting without too much ceremony. I'm aware of Lua & co., but that's still not the kind of trivial I'm aiming for. And since I'm mostly slinging Lisp these days, it should be possible to leverage some of it's powers for a tighter integration.

### forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. While widely popular in embedded hardware circles, it's unfortunately mostly forgotten outside of that niche. Unfortunate because it's one of the most trivial languages to implement, and a solid foundation for embedded and/or domain specific languages. 

### lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. Besides being based on stacks and words, Lifoo is very much Lisp in Forth clothes; to the point where it reuses the Lisp reader to read Lifoo code. Lifoo comes with a macro called DO-LIFOO to simplify executing code inline; separate functions for parsing, evaluating and compiling expressions are also provided. 

```
Lifoo> 1 2 +

3

Lifoo> (1 2 3) (2 *) map

(2 4 6)

Lifoo> ((1 2 +) (3 4 +) (5 6 +)) (eval) map

(3 7 11)

Lifoo> (drop drop 42) :+ define
       1 2 +

42

Lifoo> #(1 2 3) 1 nth 4 set drop

#(1 4 3)

Lifoo> "abc" 1 nth del drop

"ac"

Lifoo> ((bar -1) baz) :foo struct
       nil make-foo foo?

T

Lifoo> :frisbee throw
       "skipped" print ln
       (:always) always
       (drop) catch

:ALWAYS

Lifoo> 0 chan (1 2 + send :done) 1 spawn swap 
       recv swap drop swap 
       wait cons

(:DONE . 3)
```

### words
Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, modular set of built-in words. Words can be defined in either Lisp or Lifoo, and the goal is to gradually migrate as much functionality as possible to pure Lifoo. Functions for defining, looking up and un-defining words are also provided. 

```
(define-word :eq? () cmp 0 =)

(define-lisp-word :cmp ()
  (let ((lhs (lifoo-pop))
        (rhs (lifoo-pop)))
    (lifoo-push (compare lhs rhs))))

(define-macro-word :catch (in)
(list
  (cons :catch `(handler-case
                   (progn
                     ,@(reverse (mapcar #'rest (rest in))))
                 (lifoo-throw (c)
                   (lifoo-push (value c))
                   (lifoo-eval ',(first (first in))))))))
```

### implementation

```
```

### repl
Writing a basic REPL is trivial given above implementation.

```
LIFOO> (lifoo:lifoo-repl)
Welcome to Lifoo,
press enter on empty line to eval expr,
exit ends session

Lifoo> "hello Lifoo!" print ln

hello Lifoo!
NIL
Lifoo>

(defun lifoo-repl (&key (exec (lifoo-init t :exec (make-lifoo)))
                        (in *standard-input*)
                        (prompt "Lifoo>")
                        (out *standard-output*))
  "Starts a REPL for EXEC with input from IN and output to OUT,
   using PROMPT"

  (format out "Welcome to Lifoo,~%")
  (format out "press enter on empty line to eval expr,~%")
  (format out "exit ends session~%")
  
  (with-lifoo (:exec exec :env t)
    (tagbody
     start
       (format out "~%~a " prompt)
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
                 (lifoo-eval (lifoo-read :in in))
                 (write (lifoo-pop) :stream out))
             (ignore ()
               :report "Ignore error and continue.")))
         (go start))
     end)))
```

### tracing
Stack tracing takes on a whole new meaning in Forth, Lifoo offers integrated tracing to help untangle messy stacks.

```
Lifoo> :+ trace

NIL

Lifoo> "Every :+ entry and exit is traced from here" log

NIL

Lifoo> 1 2 +

3

Lifoo> t untrace

NIL

Lifoo> "Nothing is traced from here" log

NIL

Lifoo> 3 4 +

7

Lifoo> print-log

LOG   Nothing is traced from here
EXIT  + #(3)
ENTER + #(1 2)
LOG   Every :+ entry and exit is traced from here
NIL

Lifoo>
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
