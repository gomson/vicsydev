# [vicsy/dev](https://github.com/codr4life/vicsydev) | a Lispy, embedded Forth
posted Jan 31th 2017, 10:10 am

### preramble
I've been craving for a trivial, embedded scripting language that feels just right for a long, long time; something I can quickly drop into any project that needs scripting without too much ceremony. I'm aware of Lua & co., but that's still not the kind of trivial I'm aiming for. And since I'm mostly slinging Lisp these days, it should be possible to leverage some of it's powers for a tighter integration.

### forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. While widely popular in embedded hardware circles, it's unfortunately mostly forgotten outside of that niche. Unfortunate because Forth makes a perfect foundation for domain specific languages.

### lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. Besides being based on stacks and words, Lifoo is very much Lisp in Forth clothes; to the point where it reuses the Lisp reader to read Lifoo code. Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, modular set of built-in words. Words can be defined in either Lisp or Lifoo, and the goal is to gradually migrate as much functionality as possible to pure Lifoo. Besides macros; functions for defining, looking up and un-defining words are also provided. 

```
(defmacro define-lifoo-init (name &body body)
  "Defines NAME-init around BODY with exec"
  `(defun ,(symbol! 'lifoo- name) (&key (exec *lifoo*)) 
     (with-lifoo (:exec exec) ,@body)
     exec))

(define-lifoo-init init-comparisons
  ;; Pops $rhs and $lhs,
  ;; and pushes result of comparing $lhs to $rhs
  (define-lisp-word :cmp ()
    (let ((rhs (lifoo-pop))
          (lhs (lifoo-pop)))
      (lifoo-push (compare lhs rhs))))
  
  (define-word :eq? () cmp 0 =)
  (define-word :neq? () cmp 0 /=)
  (define-word :lt? () cmp -1 =)
  (define-word :gt? () cmp 1 =)
  (define-word :lte? () cmp 1 <)
  (define-word :gte? () cmp -1 >))

(define-lifoo-init init-env
  ;; Pushes copy of env as alist
  (define-lisp-word :env (:copy-env? nil)
    (lifoo-push (copy-list *lifoo-env*)))

  ;; Pops $var and returns value
  (define-lisp-word :get (:copy-env? nil)
    (lifoo-push (lifoo-get (lifoo-pop))))

  ;; Pops $val and $var,
  ;; sets $var's value to $val and pushes $val
  (define-lisp-word :set (:copy-env? nil)
    (let ((val (lifoo-pop))
          (var (lifoo-pop)))
      (lifoo-set var val)
      (lifoo-push val)))

  (define-lisp-word :rem (:copy-env? nil)
    (let* ((var (lifoo-pop))
           (val (lifoo-get var)))
      (lifoo-rem var)
      (lifoo-push val))))

(define-lifoo-init init-flow
  ;; Pops $cnd and $res;
  ;; and pushes $res if $cnd, otherwise NIL
  (define-lisp-word :when ()
    (let ((cnd (lifoo-pop))
          (res (lifoo-pop)))
      (lifoo-eval cnd)
      (if (lifoo-pop)
          (lifoo-eval res)
          (lifoo-push nil))))

  ;; Pops $cnd and $res;
  ;; and pushes $res unless $cnd, otherwise NIL
  (define-word :unless () nil? when)

  ;; Pops $reps and $body;
  ;; and repeats $body $reps times,
  ;; pushing indexes before evaluating body
  (define-lisp-word :times ()
    (let ((reps (lifoo-pop))
          (body (lifoo-parse (lifoo-pop))))
      (dotimes (i reps)
        (lifoo-push i)
        (eval `(progn ,@body)))))

  ;; Pops $body and loops until $body pushes nil 
  (define-lisp-word :while ()
    (let ((body (lifoo-parse (lifoo-pop))) (res))
      (do-while ((progn
                   (eval `(progn ,@body))
                   (setf res (lifoo-top)))
                 res)
        (lifoo-pop)))))

(define-lifoo-init init-io
  ;; Pops $val and prints it
  (define-lisp-word :print ()
    (princ (lifoo-pop)))

  ;; Prints line ending
  (define-lisp-word :ln ()
    (terpri)))

(define-lifoo-init init-lists
  (define-lisp-ops () cons)

  ;; Clears stack and pushes previous contents as list
  (define-lisp-word :list ()
    (let ((lst (stack *lifoo*)))
      (setf (stack *lifoo*) nil)
      (lifoo-push (nreverse lst))))

  ;; Pops $list and pushes first element
  (define-lisp-word :first ()
    (lifoo-push (first (lifoo-pop))))

  ;; Pops $list and pushes rest
  (define-lisp-word :rest ()
    (lifoo-push (rest (lifoo-pop))))

  ;; Pops item from list in $1 and pushes it
  (define-lisp-word :pop ()
    (let ((it (pop (first (stack *lifoo*)))))
      (lifoo-push it)))

  ;; Pops $it and pushes it on list in $1
  (define-lisp-word :push ()
    (let ((it (lifoo-pop)))
      (push it (first (stack *lifoo*)))))

  ;; Pops $list and pushes reversed list
  (define-lisp-word :reverse ()
    (lifoo-push (reverse (lifoo-pop))))

  ;; Pops $fn and $lst,
  ;; and pushes result of mapping $fn over $lst
  (define-lisp-word :map ()
    (let ((fn (lifoo-compile (lifoo-pop)))
          (lst (lifoo-pop)))
      (lifoo-push (mapcar (lambda (it)
                            (lifoo-push it)
                            (funcall fn)
                            (lifoo-pop))
                          lst)))))

(define-lifoo-init init-meta
  ;; Pops $val and pushes its symbolic representation
  (define-lisp-word :symbol ()
    (lifoo-push (keyword! (lifoo-pop))))

  ;; Pops $val and pushes the word it represents
  (define-lisp-word :word ()
    (let ((fn (lifoo-word (lifoo-pop))))
      (lifoo-push fn)))

  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? ()
    (lifoo-push (null (lifoo-eval (lifoo-pop)))))

  ;; Pops $expr and pushes function that evaluates $expr as Lisp
  (define-lisp-word :lisp ()
    (lifoo-push (eval `(lambda () ,(lifoo-pop)))))
  
  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval ()
    (lifoo-push (lifoo-eval (lifoo-pop))))
  
  ;; Pops $expr and pushes compiled word
  (define-lisp-word :compile ()
    (lifoo-push (lifoo-compile (lifoo-pop))))

  ;; Enables tracing and clears trace
  (define-lisp-word :trace ()
    (setf (tracing? *lifoo*) t)
    (setf (traces *lifoo*) nil))

  ;; Disables tracing and prints trace
  (define-lisp-word :untrace ()
    (dolist (st (reverse (traces *lifoo*)))
      (format t "~a~%" st))
    (setf (tracing? *lifoo*) nil))

  ;; Pops $msg and signals error
  (define-lisp-word :error ()
    (signal 'lifoo-error :message (lifoo-eval (lifoo-pop))))

  ;; Pops $cnd and signals error if NIL
  (define-lisp-word :assert ()
    (let* ((cnd (lifoo-pop))
           (ok? (progn (lifoo-eval cnd) (lifoo-pop))))
      (unless ok?
        (signal 'lifoo-assert
                :message (format nil "assert failed: ~a" cnd))))))

(define-lifoo-init init-numbers
  (define-lisp-ops () + - * / = /= < > cons)

  (define-lisp-word :inc ()
    (incf (first (stack *lifoo*)))))

(define-lifoo-init init-stack
  ;; Pushes stack on stack as list
  (define-lisp-word :stack ()
    (lifoo-push (copy-list (stack *lifoo*))))
  
  ;; Pops stack
  (define-lisp-word :drop ()
    (lifoo-pop))

  ;; Swaps $1 and $2
  (define-lisp-word :swap ()
    (push (lifoo-pop) (rest (stack *lifoo*))))
  
  ;; Pushes $1 on stack
  (define-lisp-word :dup ()
    (lifoo-push (first (stack *lifoo*)))))

(define-lifoo-init init-strings
  ;; Pops $val and pushes string representation
  (define-lisp-word :string ()
    (let ((val (lifoo-pop)))
      (lifoo-push (if (listp val)
                      (apply #'string! val)
                      (string! val)))))

  ;; Pops $args and $fmt,
  ;; and pushes formatted output
  (define-lisp-word :format ()
    (let ((args (lifoo-pop))
          (fmt (lifoo-pop)))
      (lifoo-push (apply #'format nil fmt args)))))

(define-lifoo-init init-threads
  ;; Yields processor and re-schedules thread
  (define-lisp-word :yield ()
    (thread-yield))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :sleep ()
    (sleep (lifoo-pop)))

  ;; Pops $expr;
  ;; evaluates it in new thread/exec,
  ;; and pushes thread
  (define-lisp-word :thread ()
    (let* ((expr (lifoo-pop))
           (exec (make-lifoo :stack (clone (stack *lifoo*))
                             :words (clone (words *lifoo*))))
           (thread (make-thread (lambda ()
                                  (lifoo-eval expr
                                              :exec exec)))))
      (lifoo-push thread)))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :join-thread ()
    (lifoo-push (join-thread (lifoo-pop))))

  ;; Pops $buf-len and pushes new channel
  (define-lisp-word :chan ()
    (lifoo-push (make-chan :max-length (lifoo-pop))))

  ;; Pops $msg and puts on channel in $1
  (define-lisp-word :chan-put ()
    (let ((msg (lifoo-pop)))
      (chan-put (first (stack *lifoo*)) msg)))

  ;; Gets and pushes message from channel in $1
  (define-lisp-word :chan-get ()
    (let ((msg (chan-get (first (stack *lifoo*)))))
      (lifoo-push msg))))
```

### use
Lifoo comes with a macro called do-lifoo to make it easy to execute code inline; separate functions for parsing, evaluating and compiling expressions are also provided.

```
(defmacro lifoo-assert (res &body body)
  "Asserts that evaluating BODY pushes value equal to RES 
   according to COMPARE"
  `(assert (zerop (compare ,res (do-lifoo () ,@body)))))

(define-test (:lifoo :meta)
  (with-lifoo ()
    (lifoo-assert t
      nil nil?)

    (lifoo-assert :lifoo
      "lifoo" symbol)

    (lifoo-assert 3
      1 2 "+" word eval)
    
    (lifoo-assert '(1 2 +)
      (1 2 +))
    
    (lifoo-assert 3
      (1 2 +) eval)

    (lifoo-assert 3
      (1 2 +) compile eval)
    
    (lifoo-assert 42
      42 (lifoo-pop) lisp eval)
    
    (assert (eq
             :failed
             (handler-case (do-lifoo ()
                             :any-message error)    
               (lifoo-error (e)
                 (assert (eq :any-message (lifoo-error e)))
                 :failed))))

    (assert (eq
             :failed
             (handler-case (do-lifoo ()
                             (1 2 =) assert)    
               (lifoo-assert () :failed))))))

(define-test (:lifoo :stack)
  (with-lifoo ()
    (lifoo-assert '(3 2 1)
      1 2 3 stack)

    ;; Make sure that stack is left intact
    (assert (equal '(3 2 1) (lifoo-stack)))
    
    (lifoo-assert 1
      1 dup drop)
    
    (lifoo-assert 2
      1 2 swap drop)))

(define-test (:lifoo :flow)
  (with-lifoo ()
    (lifoo-assert :ok
      :ok (2 1 <) when)
    
    (lifoo-assert :ok
      :ok (1 2 =) unless)
    
    (lifoo-assert 3
      0 (inc dup 3 >) while drop)
    
    (lifoo-assert '(2 1 0)
      list (push) 3 times)))

(define-test (:lifoo :strings)
  (with-lifoo ()
    (lifoo-assert "123ABC"
      (1 2 3 abc) string)
    
    (lifoo-assert "1+2=3"
      "~a+~a=~a" (1 2 3) format)))

(define-test (:lifoo :lists)
  (with-lifoo ()
    (lifoo-assert '(2 . 1)
      1 2 cons)
    
    (lifoo-assert '(1 . 2)
      (1 . 2))
    
    (lifoo-assert '(1 2 3)
      1 2 3 list)
    
    (lifoo-assert 2
      (1 2 3) rest first)
    
    (lifoo-assert 2
      (1 2 3) pop drop pop)
    
    (lifoo-assert '(1 2 3)
      (1) 2 push 3 push reverse)
    
    (lifoo-assert '(2 4 6)
      (1 2 3) (2 *) map)))

(define-test (:lifoo :comparisons)
  (with-lifoo ()
    (lifoo-assert t
      "abc" "abc" eq?)
    
    (lifoo-assert nil
      "abc" "abcd" eq?)
    
    (lifoo-assert t
      "abc" "abcd" neq?)
    
    (lifoo-assert t
      "abc" "def" lt?)
    
    (lifoo-assert nil
      "abc" "def" gt?)))

(define-test (:lifoo :env)
  (with-lifoo ()
    (lifoo-assert 42
      :foo 42 set drop :foo get)
    
    (lifoo-assert '((:bar . 7) (:foo . 42))
      :foo 42 set :bar 7 set env)
    
    (lifoo-assert '(nil . 42)
      :foo dup 42 set drop dup rem swap get cons)))

(define-test (:lifoo :io)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo ()
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :threads)
  (with-lifoo ()
    (lifoo-assert 42
      1 chan 42 chan-put chan-get)
    
    (lifoo-assert '(:done . 3)
      0 chan (1 2 + chan-put :done) thread swap 
      chan-get swap drop swap 
      join-thread cons)))
```

### implementation

```
(defvar *lifoo* nil)
(defvar *lifoo-env* nil)

(defmacro define-lisp-word (name
                            (&key (copy-env? t) exec)
                            &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name (lambda ()
                            ,(if copy-env?
                                 `(with-lifoo-env ()
                                    ,@body)
                                 `(progn ,@body))))))

(defmacro define-lisp-ops ((&key exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op)
                        (:copy-env? nil)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (&key (copy-env? t) exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (lifoo-compile '(,@body)
                                  :copy-env? ,copy-env?))))

(defmacro do-lifoo ((&key exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo*
                          (lifoo-init :exec (make-lifoo))))
     (lifoo-eval '(,@body))
     (lifoo-pop)))

(defmacro with-lifoo ((&key exec) &body body)
  "Runs body with *LIFOO* bound to EXEC or new"
  `(let ((*lifoo* (or ,exec (lifoo-init :exec (make-lifoo)))))
     ,@body))

(defmacro with-lifoo-env ((&key env) &body body)
  "Runs body with *LIFOO-ENV* bound to ENV or copy"
  `(let ((*lifoo-env* (or ,env (copy-list *lifoo-env*))))
     ,@body))

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  stack traces tracing?
  (words (make-hash-table :test 'eq)))

(define-condition lifoo-error (error)
  ((message :initarg :message :reader lifoo-error)))

(define-condition lifoo-assert (lifoo-error) ())

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
                  (let ((found? (or (lifoo-word e)
                                    (error "missing word: ~a" e))))
                    (rec (rest ex)
                         (cons
                          `(progn
                             (when (tracing? ,exec)
                               (push (format nil "CALL ~a" ',e)
                                     (traces ,exec)))
                             (funcall ,found?))
                          acc))))
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

(defun lifoo-eval (expr &key (copy-env? t) (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (let ((pe (lifoo-parse expr)))
      (eval (if copy-env?
                `(with-lifoo-env ()
                   ,@pe)
                `(progn ,@pe))))))

(defun lifoo-compile (expr &key (copy-env? t) (exec *lifoo*))
  "Returns result of parsing and compiling EXPR in EXEC"  
  (eval (if copy-env?
            `(lambda ()
               (with-lifoo-env ()
                 ,@(lifoo-parse expr :exec exec)))
            `(lambda ()
               ,@(lifoo-parse expr :exec exec)))))

(defun lifoo-define (id fn &key (exec *lifoo*))
  "Defines word named ID in EXEC as FN"  
  (setf (gethash (keyword! id) (words exec)) fn))

(defun lifoo-undefine (id &key (exec *lifoo*))
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (words exec)))

(defun lifoo-word (id &key (exec *lifoo*))
  "Returns word named ID from EXEC or NIL if missing"  
  (gethash (keyword! id) (words exec)))

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

(defun lifoo-top (&key (exec *lifoo*))
  "Returns top of EXEC's stack"
  (first (stack exec)))

(defun lifoo-stack (&key (exec *lifoo*))
  "Returns current stack for EXEC"
  (stack exec))

(defun lifoo-trace (&key (exec *lifoo*))
  "Enables tracing for EXEC"
  (setf (tracing? exec) t)
  (setf (traces exec) nil))

(defun lifoo-untrace (&key (exec *lifoo*))
  "Disables tracing for EXEC"
  (setf (tracing? exec) nil)
  (traces exec))

(defun lifoo-get (var)
  "Returns value of VAR in EXEC"
  (rest (assoc var *lifoo-env* :test #'eq))) 

(defun lifoo-set (var val)
  "Sets value of VAR in EXEC to VAL"
  (let ((found? (assoc var *lifoo-env* :test #'eq)))
    (if found?
        (rplacd found? val)
        (setf *lifoo-env* (acons var val *lifoo-env*))))
  val)

(defun lifoo-rem (var)
  "Returns value of VAR in EXEC"
  (setf *lifoo-env*
        (delete var *lifoo-env* :key #'first :test #'eq))) 
```

### repl
Building a basic REPL is trivial given above implementation.

```
(defun lifoo-repl (&key (exec (lifoo-init :exec (make-lifoo)))
                        (in *standard-input*)
                        (prompt "Lifoo>")
                        (out *standard-output*))
  "Starts a REPL for EXEC"
  (with-lifoo (:exec exec)
    (with-lifoo-env ()
      (tagbody
       start
         (format out "~%~a " prompt)
         (when-let (line (read-line in nil))
           (unless (string= "" line)
             (with-input-from-string (in line)
               (lifoo-eval (lifoo-read :in in) :copy-env? nil)
               (format out "~a~%" (lifoo-pop)))
             (go start)))))))

CL-USER> (lifoo:lifoo-repl)

Lifoo> "hello Lifoo!" print ln
hello Lifoo!
NIL

Lifoo> 
```

### tracing
Stack tracing takes on a whole new meaning in Forth, Lifoo offers integrated tracing to help untangle messy stacks.

```
CL-USER> (lifoo:lifoo-repl)

Lifoo> trace
NIL

Lifoo> 1 2 +
3

Lifoo> untrace
PUSH 1
(1)
PUSH 2
(2 1)
CALL +
POP  2
(1)
POP  1
NIL
PUSH 3
(3)
POP  3
NIL
CALL UNTRACE
NIL

Lifoo> 
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
