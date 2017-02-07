# [vicsy/dev](https://github.com/codr4life/vicsydev) | a Lispy, embedded Forth
posted Feb 6th 2017, 05:00 pm

### preramble
I've been craving for a trivial, embedded scripting language that feels just right for a long, long time; something I can quickly drop into any project that needs scripting without too much ceremony. I'm aware of Lua & co., but that's still not the kind of trivial I'm aiming for. And since I'm mostly slinging Lisp these days, it should be possible to leverage some of it's powers for a tighter integration.

### forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. While widely popular in embedded hardware circles, it's unfortunately mostly forgotten outside of that niche. Unfortunate because it's one of the most trivial languages to implement, and a solid foundation for embedded and/or domain specific languages. 

### lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. Besides being based on stacks and words, Lifoo is very much Lisp in Forth clothes; to the point where it reuses the Lisp reader to read Lifoo code. Lifoo comes with a macro called DO-LIFOO to simplify executing code inline; separate functions for parsing, evaluating and compiling expressions are also provided. 

```
Lifoo> 1 2 +

3

Lifoo> (1 2 +)

(1 2 +)

Lifoo> (1 2 +) eval

3

Lifoo> (1 2 +) compile

(PROGN (LIFOO-PUSH 1) (LIFOO-PUSH 2) (LIFOO-CALL '+))

Lifoo> (1 2 +) inline

#<FUNCTION {1003F99E8B}>

Lifoo> (1 2 +) inline eval

3

Lifoo> (1 2 3) (2 *) map

(2 4 6)

Lifoo> ((1 2 +) (3 4 +) (5 6 +)) (eval) map

(3 7 11)

Lifoo> #(1 2 3) 1 nth 4 set drop

#(1 4 3)

Lifoo> "abc" 1 nth del drop

"ac"

Lifoo> "~a+~a=~a" (1 2 3) format

"1+2=3"

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

Lifoo> (drop drop 42) :+ define
       1 2 +

42
```

### words
Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, modular set of built-in words. Words can be defined in either Lisp or Lifoo; functions for defining, looking up and un-defining words are also provided. 

```
(define-word :eq? (nil) cmp 0 =)

(define-lisp-word :cmp (nil)
  (let ((lhs (lifoo-pop))
        (rhs (lifoo-pop)))
    (lifoo-push (compare lhs rhs))))

(define-macro-word :always (in out)
  (list
    (cons in `(unwind-protect
                (progn
                  ,@(reverse (mapcar #'rest (rest out))))
             (lifoo-eval ',(first (first out)))))))
```

### implementation
The following definitions have been hand-picked for their illustrative value, the full implementation of Lifoo currently weighs in at around 400 lines excluding word definitions.

```
(defmacro define-word (name ((&rest args) &key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(lifoo-define ',name
                 (make-lifoo-word :id ,(keyword! name)
                                  :source ',body
                                  :args ',args)
                 :exec (or ,exec *lifoo*)))

(defmacro define-lisp-word (id ((&rest args) &key exec speed)
                            &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,id
                 (make-lifoo-word
                  :id ,id
                  :source ',body
                  :fn (lambda ()
                        ,(lifoo-optimize :speed speed)
                        ,@body)
                  :args ',args)
                 :exec (or ,exec *lifoo*)))

(defmacro define-macro-word (id (in out &key exec)
                             &body body)
  "Defines new macro word NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,id (make-lifoo-word
                      :id ,id
                      :macro? t
                      :source ',body
                      :fn (lambda (,in ,out)
                            ,@body))
                 :exec (or ,exec *lifoo*)))

(defmacro do-lifoo ((&key (env t) exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo* (make-lifoo))
                :env ,env)
     (lifoo-eval ',body)
     (lifoo-pop)))

(defun lifoo-eval (expr &key (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (handler-case
        (eval `(progn ,@(lifoo-compile expr)))
      (lifoo-throw (c)
        (lifoo-error "thrown value not caught: ~a" (value c))))))

(defun lifoo-compile (forms &key (exec *lifoo*))
  "Parses EXPR and returns code for EXEC"
  (labels ((compile-forms (in out)
             (if in
                 (let ((f (first in)))
                   (compile-forms (rest in)
                                  (lifoo-compile-form f out)))
                 (mapcar #'rest (nreverse out)))))
    (with-lifoo (:exec exec)
      (compile-forms (list! forms) nil))))

(defun lifoo-compile-form (f in)
  "Compiles form F for token stream IN and returns new stream"
  (cond
    ((simple-vector-p f)
     (let ((len (length f)))
       (cons (cons f `(lifoo-push
                          ,(make-array
                            len
                            :adjustable t
                            :fill-pointer len
                            :initial-contents f)))
             in)))
    
    ((consp f)
     (if (consp (rest f))
         (cons (cons f `(lifoo-push ',(copy-list f))) in)
         (cons (cons f `(lifoo-push ',(cons (first f) (rest f))))
               in)))
    ((null f)
     (cons (cons f `(lifoo-push nil)) in))
    ((eq f t)
     (cons (cons f `(lifoo-push t)) in))
    ((or (and (symbolp f) (not (keywordp f)))
         (lifoo-word-p f))
     (lifoo-expand f in))
    (t
     (cons (cons f `(lifoo-push ,f)) in))))

(defun lifoo-expand (in out)
  "Expands IN into OUT and returns new token stream"
  (let ((word (lifoo-word in)))
    (if word
        (if (macro? word)
            (let ((exp (funcall (fn word) in out)))
              (cons (cons (first (first exp)) 
                          `(do-lifoo-call ((lifoo-word ',in))
                               ,(rest (first exp))))
               (rest exp)))
            (progn
              (when (args word) (lifoo-compile-args word out))
              (cons (cons in `(lifoo-call ',in)) out)))
        (cons (cons in `(lifoo-call ',in))
              out))))

(defun lifoo-call (word &key (exec *lifoo*))
  "Calls WORD in EXEC"

  (unless (lifoo-word-p word)
    (let ((id word))
      (unless (setf word (lifoo-word id))
        (error "missing word: ~a" id)))) 

  (do-lifoo-call (word :exec exec)
    (funcall (lifoo-compile-word word))))

(defun lifoo-compile-word (word &key (exec *lifoo*) speed)
  "Makes sure word is compiled and returns function"
  (or (fn word)
      (setf (fn word)
            (eval `(lambda ()
                     ,(lifoo-optimize :speed speed)
                     ,@(lifoo-compile (source word)
                                      :exec exec))))))
```

### repl
Writing a basic REPL is trivial given above implementation.

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
  
  (with-lifoo (:exec exec :env t)
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
                 (lifoo-eval (lifoo-read :in in))
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
