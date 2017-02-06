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

Lifoo> (1 2 +) eval

3

Lifoo> (1 2 +) compile

(PROGN (LIFOO-PUSH 1) (LIFOO-PUSH 2) (LIFOO-CALL '+))

Lifoo> (1 2 +) compile lisp eval

3
```

### words
Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, modular set of built-in words. Words can be defined in either Lisp or Lifoo; functions for defining, looking up and un-defining words are also provided. 

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
The following definitions have been hand-picked for their illustrative value, the full implementation of Lifoo currently weighs in at around 400 lines excluding word definitions.

```
(defmacro define-macro-word (name (in &key exec)
                             &body body)
  "Defines new macro word NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define-macro (keyword! ',name)
                       (lambda (,in)
                         ,@body)
                       :exec (or ,exec *lifoo*)))

(defmacro define-lisp-word (id (&key exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,id
                 (make-lifoo-word :id ,id
                                  :source ',body
                                  :fn (lambda () ,@body))
                 :exec (or ,exec *lifoo*)))

(defmacro define-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(lifoo-define ',name
                 (make-lifoo-word :id ,(keyword! name)
                                  :source ',body)
                 :exec (or ,exec *lifoo*)))

(defmacro do-lifoo ((&key (env t) exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo* (make-lifoo))
                :env ,env)
     (lifoo-eval ',body)
     (lifoo-pop)))

(defstruct (lifoo-word (:conc-name))
  id
  trace?
  source fn)

(defstruct (lifoo-cell (:conc-name lifoo-))
  val set del)

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  envs logs
  (stack (make-array 3 :adjustable t :fill-pointer 0))
  (macro-words (make-hash-table :test 'eq))
  (words (make-hash-table :test 'eq)))

(defun lifoo-read (&key (in *standard-input*))
  "Reads Lifoo code from IN until end of file"
  (let ((eof? (gensym)) (more?) (expr))
    (do-while ((not (eq (setf more? (read in nil eof?)) eof?)))
      (push more? expr))
    (nreverse expr)))

(defun lifoo-parse (expr &key (exec *lifoo*))
  "Parses EXPR and returns code for EXEC"
  (labels
      ((parse (fs acc)
         (if fs
             (let ((f (first fs)))
               (parse
                (rest fs)
                (cond
                  ((simple-vector-p f)
                   (let ((len (length f)))
                     (cons (cons f `(lifoo-push
                                     ,(make-array
                                       len
                                       :adjustable t
                                       :fill-pointer len
                                       :initial-contents f)))
                           acc)))
                  ((consp f)
                   (if (consp (rest f))
                       (cons (cons f `(lifoo-push ',(copy-list f)))
                             acc)
                       (cons (cons f `(lifoo-push
                                       ',(cons (first f)
                                               (rest f))))
                             acc)))
                  ((null f)
                   (cons (cons f `(lifoo-push nil)) acc))
                  ((eq f t)
                   (cons (cons f `(lifoo-push t)) acc))
                  ((and (symbolp f) (not (keywordp f)))
                   (let* ((id (keyword! f))
                          (mw (lifoo-macro-word id)))
                     (if mw
                         (funcall mw acc)
                         (cons (cons f `(lifoo-call ,id)) acc))))
                  ((lifoo-word-p f)
                   (cons (cons f `(lifoo-call ,f)) acc))
                  (t
                   (cons (cons f `(lifoo-push ,f)) acc)))))
             (mapcar #'rest (nreverse acc)))))
    (with-lifoo (:exec exec)
      (parse (list! expr) nil))))
      
(defun lifoo-eval (expr &key (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (handler-case
        (eval `(progn ,@(lifoo-parse expr)))
      (lifoo-throw (c)
        (lifoo-error "thrown value not caught: ~a" (value c))))))

(defun lifoo-compile (word &key (exec *lifoo*))
  "Returns compiled function for WORD"
  (or (fn word)
      (setf (fn word)
            (eval `(lambda ()
                     ,@(lifoo-parse (source word) :exec exec))))))

(defun lifoo-call (word &key (exec *lifoo*))
  "Calls WORD in EXEC"

  (unless (lifoo-word-p word)
    (let ((id word))
      (unless (setf word (lifoo-word id))
        (error "missing word: ~a" id)))) 

  (when (trace? word)
    (push (list :enter (id word) (clone (stack exec)))
          (logs exec)))

  (with-lifoo (:exec exec)
    (handler-case
        (progn 
          (funcall (lifoo-compile word))

          (when (trace? word)
            (push (list :exit (id word) (clone (stack exec)))
                  (logs exec))))
      (lifoo-break ()
        (when (trace? word)
          (push (list :break (id word) (clone (stack exec)))
                (logs exec)))))))

(defun lifoo-push-cell (cell &key (exec *lifoo*))
  "Pushes CELL onto EXEC stack"  
  (vector-push-extend cell (stack exec))
  cell)

(defun lifoo-push (val &key (exec *lifoo*) set del)
  "Pushes VAL onto EXEC stack"  
  (lifoo-push-cell (make-lifoo-cell :val val :set set :del del)
                   :exec exec)
  val)

(defun lifoo-pop-cell (&key (exec *lifoo*))
  "Pops cell from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (vector-pop (stack exec))))

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops value from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (lifoo-val (lifoo-pop-cell :exec exec))))
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
