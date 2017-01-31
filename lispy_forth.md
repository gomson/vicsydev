# [vicsy/dev](https://github.com/codr4life/vicsydev) | a Lispy, embedded Forth
posted Jan 31th 2017, 10:10 am

### preramble
I've been craving for a trivial, embedded scripting language that feels just right for a long, long time; something I can quickly drop into any project that needs scripting without too much ceremony. I'm aware of Lua, but that's still not the kind of trivial I'm aiming for. And since I'm mostly slinging Lisp these days, it should be possible to leverage some of it's powers for a tighter integration.

### forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. While widely popular in embedded hardware circles, it's unfortunately mostly forgotten outside of that niche. Unfortunate because Forth makes a perfect foundation for domain specific languages.

### lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. Besides being based on stacks and words, Lifoo is very much Lisp in Forth clothes; to the point where it reuses the Lisp reader to read Lifoo code. Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, optional set of built in words as a foundation to build on. Words can be defined in either Lisp or Lifoo, the goal is to gradually migrate as much functionality as possible to pure Lifoo. Besides macros; functions for defining, looking up and un-defining words are also provided. 

```
    ;; Define binary ops
    (define-lisp-ops () + - * / = /= < > cons)
    

    ;; *** stack management ***
    
    ;; Pushes stack on stack
    (define-lisp-word :stack ()
      (lifoo-push (stack *lifoo*)))
    
    ;; Drops $1 from stack
    (define-lisp-word :drop ()
      (lifoo-pop))

    ;; Swaps $1 and $2
    (define-lisp-word :swap ()
      (push (lifoo-pop) (rest (stack *lifoo*))))
    
    ;; Pushes $1 on stack
    (define-lisp-word :dup ()
      (lifoo-push (first (stack *lifoo*))))

    ;; *** compiler ***
    
    ;; Replaces $1 with result of evaluating it
    (define-lisp-word :eval ()
      (lifoo-push (lifoo-eval (lifoo-pop))))
    
    ;; Replaces $1 with result of compiling it
    (define-lisp-word :compile ()
      (lifoo-push (lifoo-compile (lifoo-pop))))
    

    ;; *** introspection ***
    
    ;; Replaces $1 with symbolic representation
    (define-lisp-word :symbol ()
      (lifoo-push (keyword! (lifoo-pop))))

    ;; Replaces $1 with the word it represents
    (define-lisp-word :word ()
      (let ((fn (lifoo-word (lifoo-pop))))
        (lifoo-push fn)))

    ;; Replaces $1 with T if NIL, otherwise NIL
    (define-lisp-word :nil? ()
      (lifoo-push (null (lifoo-eval (lifoo-pop)))))

    ;; *** generic comparisons ***
    
    ;; Replaces $1 and $2 with result of comparing $2 to $1
    (define-lisp-word :cmp ()
      (let ((rhs (lifoo-pop))
            (lhs (lifoo-pop)))
        (lifoo-push (compare lhs rhs))))


    ;; *** lists ***

    ;; Clears and pushes stack
    (define-lisp-word :list ()
      (let ((lst (stack *lifoo*)))
        (setf (stack *lifoo*) nil)
        (lifoo-push (nreverse lst))))

    ;; Replaces $1 with first element of list
    (define-lisp-word :first ()
      (lifoo-push (first (lifoo-pop))))

    ;; Replaces $1 with rest of list
    (define-lisp-word :rest ()
      (lifoo-push (rest (lifoo-pop))))

    ;; Pops item from list in $1 and pushes it on stack
    (define-lisp-word :pop ()
      (let ((it (pop (first (stack *lifoo*)))))
        (lifoo-push it)))

    ;; Pops $1 from stack and pushes it on list in $2
    (define-lisp-word :push ()
      (let ((it (lifoo-pop)))
        (push it (first (stack *lifoo*)))))

    ;; Replaces $1 with reversed list
    (define-lisp-word :reverse ()
      (lifoo-push (reverse (lifoo-pop))))

    ;; Replaces $1 and $2 with results of mapping $1 over $2
    (define-lisp-word :map ()
      (let ((fn (lifoo-compile (lifoo-pop)))
            (lst (lifoo-pop)))
        (lifoo-push (mapcar (lambda (it)
                              (lifoo-push it)
                              (funcall fn)
                              (lifoo-pop))
                            lst))))


    ;; *** strings ***

    ;; Replaces $1 with string representation
    (define-lisp-word :string ()
      (let ((val (lifoo-pop)))
        (lifoo-push (if (listp val)
                        (apply #'string! val)
                        (string! val)))))

    ;; Replaces $1 (arguments) and $2 (format) with formatted
    ;; output
    (define-lisp-word :format ()
      (let ((args (lifoo-pop))
            (fmt (lifoo-pop)))
        (lifoo-push (apply #'format nil fmt args))))


    ;; *** printing ***

    ;; Prints line ending
    (define-lisp-word :ln ()
      (terpri))

    ;; Pops and prints $1
    (define-lisp-word :print ()
      (princ (lifoo-pop)))
    

    ;; *** branching ***
    
    ;; Replaces $1 and $2 with results of evaluating $2 if $1,
    ;; otherwise NIL
    (define-lisp-word :when ()
      (let ((cnd (lifoo-pop))
            (res (lifoo-pop)))
        (lifoo-eval cnd)
        (if (lifoo-pop)
            (lifoo-eval res)
            (lifoo-push nil))))


    ;; *** loops ***
    
    ;; Pops and repeats body in $2 x $1, pushing indexes on stack
    (define-lisp-word :times ()
      (let ((reps (lifoo-pop))
            (body (lifoo-parse (lifoo-pop))))
        (dotimes (i reps)
          (lifoo-push i)
          (eval `(progn ,@body)))))


    ;; *** threads ***

    ;; Sleeps for $1 seconds
    (define-lisp-word :sleep ()
      (sleep (lifoo-pop)))


    ;; *** tracing ***
    
    ;; Enables stack tracing and clears trace
    (define-lisp-word :trace ()
      (setf (tracing? *lifoo*) t)
      (setf (traces *lifoo*) nil))

    ;; Disables stack tracing and prints trace
    (define-lisp-word :untrace ()
      (dolist (st (traces *lifoo*))
        (format t "~a~%" st))
      (setf (tracing? *lifoo*) nil))

    
    ;; *** derived generic comparisons ***

    (define-word :eq? () cmp 0 =)
    (define-word :neq? () cmp 0 /=)
    (define-word :lt? () cmp -1 =)
    (define-word :gt? () cmp 1 =)
    (define-word :lte? () cmp 1 <)
    (define-word :gte? () cmp -1 >)


    ;; *** derived branching ***

    ;; Replaces $1 and $2 with results of evaluating $2 if $1
    ;; is NIL, otherwise NIL
    (define-word :unless () nil? when)
```

### use
Lifoo comes with a macro called do-lifoo to make it easy to execute code inline; separate functions for parsing, evaluating and compiling expressions are also provided.

```
(define-test (:lifoo :stack)
  (assert (= 1 (do-lifoo ()
                 1 dup drop)))
  (assert (= 2 (do-lifoo ()
                 1 2 swap drop))))

(define-test (:lifoo :branch)
  (assert (eq :ok (do-lifoo ()
                    :ok (1 2 <) when)))
  (assert (eq :ok (do-lifoo ()
                    :ok (1 2 =) unless))))

(define-test (:lifoo :loop)
  (assert (equal '(2 1 0) (do-lifoo ()
                            list (push) 3 times))))

(define-test (:lifoo :cmp)
  (assert (do-lifoo ()
            "abc" "abc" eq?))
  (assert (not (do-lifoo ()
                 "abc" "abcd" eq?)))
  (assert (do-lifoo ()
            "abc" "abcd" neq?))
  (assert (do-lifoo ()
            "abc" "def" lt?))
  (assert (not (do-lifoo ()
                 "abc" "def" gt?))))

(define-test (:lifoo :lists)
  (assert (equal '(1 . 2) (do-lifoo ()
                            1 2 cons)))
  (assert (equal '(1 . 2) (do-lifoo ()
                            (1 . 2))))
  (assert (equal '(1 2 3) (do-lifoo ()
                            1 2 3 list)))
  (assert (= 2 (do-lifoo ()
                 (1 2 3) rest first)))
  (assert (= 2 (do-lifoo ()
                 (1 2 3) pop drop pop)))
  (assert (equal '(1 2 3) (do-lifoo ()
                            (1) 2 push 3 push reverse)))
  (assert (equal '(2 4 6) (do-lifoo ()
                            (1 2 3) (2 *) map))))

(define-test (:lifoo :print)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo ()
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :string)
  (assert (string= "123ABC" (do-lifoo () (1 2 3 abc) string)))
  (assert (string= "1+2=3"
                   (do-lifoo () "~a+~a=~a" (1 2 3) format))))

(define-test (:lifoo :symbol)
  (assert (eq :lifoo (do-lifoo ()
                       "lifoo" symbol))))

(define-test (:lifoo :eval)
  (assert (equal '(1 2 +) (do-lifoo ()
                            (1 2 +))))
  (assert (= 3 (do-lifoo ()
                 (1 2 +) eval))))

(define-test (:lifoo :compile)
  (assert (= 3 (do-lifoo ()
                 (1 2 +) compile eval))))

(define-test (:lifoo :word)
  (assert (= 3 (do-lifoo ()
                 1 2 "+" word eval))))
```

### implementation

```
(defvar *lifoo* nil)

(defmacro define-lisp-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name (lambda () ,@body))))

(defmacro define-lisp-ops ((&key exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op) ()
                      (let ((,_rhs (lifoo-pop))
                            (,_lhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (lifoo-compile '(,@body)))))

(defmacro do-lifoo ((&key exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo*
                          (lifoo-init :exec (make-lifoo))))
     (lifoo-eval '(,@body))
     (lifoo-pop)))

(defmacro with-lifoo ((&key exec) &body body)
  `(let ((*lifoo* (or ,exec (lifoo-init :exec (make-lifoo)))))
     ,@body))

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  stack traces tracing?
  (words (make-hash-table :test 'eq)))

(defun lifoo-parse (expr &key (exec *lifoo*))
  "Parses EXPR and returns code compiled for EXEC"
  (labels
      ((rec (ex acc)
         (if ex
             (let ((e (first ex)))
               (cond
                 ((consp e)
                  (rec (rest ex)
                       (cons `(lifoo-push '(,@e)) acc)))
                 ((null e)
                  (rec (rest ex)
                       (cons `(lifoo-push nil) acc)))
                 ((eq e t)
                  (rec (rest ex) (cons `(lifoo-push t) acc)))
                 ((keywordp e)
                  (rec (rest ex) (cons `(lifoo-push ,e) acc)))
                 ((symbolp e)
                  (rec (rest ex)
                       (cons `(funcall ,(lifoo-word e)) acc)))
                 ((functionp e)
                  (rec (rest ex)
                       (cons `(funcall ,e) acc)))
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,e) acc)))))
             (nreverse acc))))
    (with-lifoo (:exec exec)
      (rec (list! expr) nil))))

(defun lifoo-read (&key (in *standard-input*))
  (let ((more?) (expr))
    (do-while ((setf more? (read in nil)))
      (push more? expr))
    (nreverse expr)))

(defun lifoo-eval (expr &key (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (let ((pe (lifoo-parse expr :exec exec)))
    (eval `(progn ,@pe))))

(defun lifoo-compile (expr &key (exec *lifoo*))
  "Returns result of parsing and compiling EXPR in EXEC"  
  (eval `(lambda ()
           ,@(lifoo-parse expr :exec exec))))

(defun lifoo-define (id fn &key (exec *lifoo*))
  "Defines word named ID in EXEC as FN"  
  (setf (gethash (keyword! id) (words exec)) fn))

(defun lifoo-undefine (id &key (exec *lifoo*))
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (words exec)))

(defun lifoo-word (id &key (exec *lifoo*))
  "Returns word named ID from EXEC or signals error if missing"  
  (let ((word (gethash (keyword! id) (words exec))))
    (unless word (error "missing word: ~a" id))
    (when (tracing? exec)
      (push (format nil "WORD ~a" id)
            (traces exec)))
    word))

(defun lifoo-push (expr &key (exec *lifoo*))
  "Pushes EXPR onto EXEC's stack"  
  (push expr (stack exec))
  (when (tracing? exec)
    (push (format nil "PUSH ~a~%~a" expr (stack exec))
          (traces exec)))
  expr)

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops EXPR from EXEC's stack"
  (when (stack exec)
    (let ((val (pop (stack exec))))
      (when (tracing? exec)
        (push (format nil "POP  ~a~%~a" val (stack exec))
              (traces exec)))
      val)))

(defun lifoo-stack (&key (exec *lifoo*))
  (stack exec))

(defun lifoo-trace (&key (exec *lifoo*))
  (setf (tracing? exec) t)
  (setf (traces exec) nil))

(defun lifoo-untrace (&key (exec *lifoo*))
  (setf (tracing? exec) nil)
  (traces exec))

(defun lifoo-words (&key (exec *lifoo*))
  (words exec))
```

### repl
A simple repl is provided for playing around freestyle.

```
CL-USER> (lifoo:lifoo-repl)

Lifoo> "hello Lifoo!" print ln
hello Lifoo!
NIL

Lifoo> 
```

### tracing
Stack tracing takes on a whole new meaning in Forth, Lifoo offers integrated tracing to help untangle messy stacks.

```
Lifoo> trace
NIL

Lifoo> 1 2 +
3

Lifoo> untrace
WORD UNTRACE
POP  3
NIL
PUSH 3
(3)
POP  1
NIL
POP  2
(1)
PUSH 2
(2 1)
PUSH 1
(1)
WORD +
NIL

Lifoo> 
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
