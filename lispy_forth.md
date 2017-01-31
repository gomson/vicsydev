# [vicsy/dev](https://github.com/codr4life/vicsydev) | Lispy Forth
posted Jan 30th 2017, 02:00 pm

### preramble
I've been craving for a trivial, embedded scripting language that feels just right for a long, long time; something I can quickly drop into any project that needs scripting without too much ceremony. I'm aware about Lua, but that's still not the kind of trivial I'm aiming for. And since I'm mostly slinging Lisp these days, it should be possible to leverage some of it's powers for a tighter integration.

### forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. It's very popular in embedded circles, but outside of that bubble it's unfortunately largely forgotten these days.

### lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. Besides being based on stacks and words, Lifoo is very much Lisp in Forth clothes; to the point where it reuses the Lisp reader to read Lifoo code. 

### built-in words
Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, optional set of built in words as a foundation to build on. Words can be defined in either Lisp or Lifoo, the goal is to gradually migrate as much functionality as possible to pure Lifoo. Besides macros; functions for defining, looking up and un-defining words are also provided. Like Lisp, Lifoo reserves =, < & > for numbers; but unlike Lisp it also offers more convenient generic comparisons based on [cl4l](https://github.com/codr4life/cl4l/blob/master/compare.lisp).

```lisp
  ;; Define binary ops
  (define-lisp-ops (exec) + - * / = /= < > cons)

  ;; Replaces $1 and $2 with result of comparing $2 to $1
  (define-lisp-word :cmp (exec)
    (let ((rhs (lifoo-pop exec))
          (lhs (lifoo-pop exec)))
      (lifoo-push exec (compare lhs rhs))))

  ;; Derived comparison ops
  (define-word :eq? (exec) cmp 0 =)
  (define-word :neq? (exec) cmp 0 /=)
  (define-word :lt? (exec) cmp -1 =)
  (define-word :gt? (exec) cmp 1 =)
  (define-word :lte? (exec) cmp 1 <)
  (define-word :gte? (exec) cmp -1 >)

  ;; Drops $1 from stack
  (define-lisp-word :drop (exec)
    (lifoo-pop exec))

  ;; Pushes $1 on stack
  (define-lisp-word :dup (exec)
    (lifoo-push exec (first (lifoo-stack exec))))
  
  ;; Replaces $1 with result of evaluating it
  (define-lisp-word :eval (exec)
    (lifoo-push exec
                (lifoo-eval exec
                            (lifoo-pop exec))))

  ;; Replaces $1 with result of compiling it
  (define-lisp-word :compile (exec)
    (lifoo-push exec
                (lifoo-compile exec
                               (lifoo-pop exec))))

  ;; Replaces $1 with first element of list
  (define-lisp-word :first (exec)
    (lifoo-push exec (first (lifoo-pop exec))))

  ;; Replaces $1 (arguments) and $2 (format) with formatted output
  (define-lisp-word :format (exec)
    (let ((args (lifoo-pop exec))
          (fmt (lifoo-pop exec)))
      (lifoo-push exec (apply #'format nil fmt args))))

  ;; Clears and pushes stack
  (define-lisp-word :list (exec)
    (let ((lst (lifoo-stack exec)))
      (setf (lifoo-stack exec) nil)
      (lifoo-push exec (nreverse lst))))

  ;; Prints line ending
  (define-lisp-word :ln (exec)
    (terpri))

  ;; Replaces $1 and $2 with results of mapping $1 over $2
  (define-lisp-word :map (exec)
    (let ((fn (lifoo-compile exec (lifoo-pop exec)))
          (lst (lifoo-pop exec)))
      (lifoo-push exec (mapcar (lambda (it)
                                 (lifoo-push exec it)
                                 (funcall fn)
                                 (lifoo-pop exec))
                               lst))))

  ;; Replaces $1 with T if NIL, otherwise NIL
  (define-lisp-word :nil? (exec)
    (lifoo-push exec
                (null (lifoo-eval exec
                                  (lifoo-pop exec)))))

  ;; Pops and prints $1
  (define-lisp-word :print (exec)
    (princ (lifoo-pop exec)))

  ;; Replaces $1 with rest of list
  (define-lisp-word :rest (exec)
    (lifoo-push exec
                (rest (lifoo-pop exec))))

  ;; Replaces $1 with string representation
  (define-lisp-word :string (exec)
    (let ((val (lifoo-pop exec)))
      (lifoo-push exec (if (listp val)
                           (apply #'string! val)
                           (string! val)))))

  ;; Replaces $1 with results of converting it to a symbol
  (define-lisp-word :symbol (exec)
    (lifoo-push exec (keyword! (lifoo-pop exec))))
  
  ;; Swaps $1 and $2
  (define-lisp-word :swap (exec)
    (push (lifoo-pop exec)
          (rest (lifoo-stack exec))))

  ;; Replaces $1 and $2 with results of evaluating $2 if $1,
  ;; otherwise NIL
  (define-lisp-word :when (exec)
    (let ((cnd (lifoo-pop exec))
          (res (lifoo-pop exec)))
      (lifoo-eval exec cnd)
      (if (lifoo-pop exec)
          (lifoo-eval exec res)
          (lifoo-push exec nil))))

  ;; Replaces $1 and $2 with results of evaluating $2 if $1 is NIL,
  ;; otherwise NIL
  (define-word :unless (exec) nil? when)

  ;; Replaces $1 with the word it represents
  (define-lisp-word :word (exec)
    (let ((fn (lifoo-word exec (lifoo-pop exec))))
      (lifoo-push exec fn)))
```

### use
Lifoo comes with a macro called do-lifoo to make it easy to execute code inline; separate functions for parsing, evaluating and compiling expressions are also provided.

```lisp
(define-test (:lifoo :stack)
  (assert (= 1 (do-lifoo () 1 dup drop)))
  (assert (= 2 (do-lifoo () 1 2 swap drop))))

(define-test (:lifoo :branch)
  (assert (eq :ok (do-lifoo () :ok (1 2 <) when)))
  (assert (eq :ok (do-lifoo () :ok (1 2 =) unless))))

(define-test (:lifoo :cmp)
  (assert (do-lifoo () "abc" "abc" eq?))
  (assert (not (do-lifoo () "abc" "abcd" eq?)))
  (assert (do-lifoo () "abc" "abcd" neq?))
  (assert (do-lifoo () "abc" "def" lt?))
  (assert (not (do-lifoo () "abc" "def" gt?))))

(define-test (:lifoo :lists)
  (assert (equal '(1 . 2) (do-lifoo () 1 2 cons)))
  (assert (equal '(1 . 2) (do-lifoo () (1 . 2))))
  (assert (equal '(1 2 3) (do-lifoo () 1 2 3 list)))
  (assert (= 2 (do-lifoo () (1 2 3) rest first)))
  (assert (equal '(2 4 6) (do-lifoo () (1 2 3) (2 *) map))))

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
  (assert (eq :lifoo (do-lifoo () "lifoo" symbol))))

(define-test (:lifoo :eval)
  (assert (equal '(1 2 +) (do-lifoo () (1 2 +))))
  (assert (= 3 (do-lifoo () (1 2 +) eval))))

(define-test (:lifoo :compile)
  (assert (= 3 (do-lifoo () (1 2 +) compile eval))))

(define-test (:lifoo :word)
  (assert (= 3 (do-lifoo () 1 2 "+" word eval))))
```

### implementation

```lisp
(defmacro define-lisp-word (name (exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,exec ',name (lambda () ,@body)))

(defmacro define-lisp-ops ((exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(progn
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,op (exec)
                      (let ((,_rhs (lifoo-pop ,exec))
                            (,_lhs (lifoo-pop ,exec)))
                        (lifoo-push ,exec (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(lifoo-define ,exec ',name (lifoo-compile ,exec '(,@body))))

(defmacro do-lifoo ((&key exec) &body body)
  "Runs BODY in EXEC"
  (with-symbols (_exec)
    `(let ((,_exec (or ,exec (lifoo-init (make-lifoo)))))
       (lifoo-eval ,_exec '(,@body))
       (lifoo-pop ,_exec))))

(defstruct (lifoo)
  stack (words (make-hash-table :test 'eq)))

(defun lifoo-parse (exec expr)
  "Parses EXPR and returns code compiled for EXEC"
  (labels
      ((rec (ex acc)
         (if ex
             (let ((e (first ex)))
               (cond
                 ((consp e)
                  (rec (rest ex)
                       (cons `(lifoo-push ,exec '(,@e))
                             acc)))
                 ((null e)
                  (rec (rest ex) (cons `(lifoo-push ,exec nil)
                                       acc)))
                 ((eq e t)
                  (rec (rest ex) (cons `(lifoo-push ,exec t)
                                       acc)))
                 ((keywordp e)
                  (rec (rest ex) (cons `(lifoo-push ,exec ,e)
                                       acc)))
                 ((symbolp e)
                  (rec (rest ex)
                       (cons `(funcall ,(lifoo-word exec e))
                             acc)))
                 ((functionp e)
                  (rec (rest ex)
                       (cons `(funcall ,e) acc)))
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,exec ,e)
                                       acc)))))
             (nreverse acc))))
    (rec (list! expr) nil)))

(defun lifoo-read (&key (in *standard-input*))
  (let ((more?) (expr))
    (do-while ((setf more? (read in nil)))
      (push more? expr))
    (nreverse expr)))

(defun lifoo-eval (exec expr)
  "Returns result of parsing and evaluating EXPR in EXEC"
  (let ((pe (lifoo-parse exec expr)))
    (eval `(progn ,@pe))))

(defun lifoo-compile (exec expr)
  "Returns result of parsing and compiling EXPR in EXEC"  
  (eval `(lambda ()
           ,@(lifoo-parse exec expr))))

(defun lifoo-define (exec id fn)
  "Defines word named ID in EXEC as FN"  
  (setf (gethash (keyword! id) (lifoo-words exec)) fn))

(defun lifoo-undefine (exec id)
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (lifoo-words exec)))

(defun lifoo-word (exec id)
  "Returns word named ID from EXEC or signals error if missing"  
  (let ((word (gethash (keyword! id) (lifoo-words exec))))
    (unless word (error "missing word: ~a" id))
    word))

(defun lifoo-push (exec expr)
  "Pushes EXPR onto EXEC's stack"  
  (push expr (lifoo-stack exec))
  expr)

(defun lifoo-pop (exec)
  "Pops EXPR from EXEC's stack"  
  (pop (lifoo-stack exec)))
```

### repl
A simple repl is provided for playing around freestyle.

```lisp
CL-USER> (lifoo:lifoo-repl)

lifoo> "hello Lifoo!" print ln
hello Lifoo!
NIL

lifoo> 
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
