# [vicsy/dev](https://github.com/codr4life/vicsydev) | a Lispy, embedded Forth
posted Feb 4th 2017, 02:10 am

### preramble
I've been craving for a trivial, embedded scripting language that feels just right for a long, long time; something I can quickly drop into any project that needs scripting without too much ceremony. I'm aware of Lua & co., but that's still not the kind of trivial I'm aiming for. And since I'm mostly slinging Lisp these days, it should be possible to leverage some of it's powers for a tighter integration.

### forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. While widely popular in embedded hardware circles, it's unfortunately mostly forgotten outside of that niche. Unfortunate because it's one of the most trivial languages to implement, and a solid foundation for embedded and/or domain specific languages. 

### lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. Besides being based on stacks and words, Lifoo is very much Lisp in Forth clothes; to the point where it reuses the Lisp reader to read Lifoo code. Lifoo comes with a macro called DO-LIFOO to simplify executing code inline; separate functions for parsing, evaluating and compiling expressions are also provided. 

```
(defmacro lifoo-asseq (res &body body)
  "Asserts that evaluating BODY after stack reset pushes value 
   that compares equal to RES"
  `(asseq ,res (do-lifoo () reset ,@body)))

;; Loads words from :string module and returns uppercase string
(lifoo-asseq "LIFOO"
  :string init
  "lifoo" upper)

;; Loads words from all modules
(lifoo-init t)

;; Returns :true if 1 = 1
(lifoo-asseq :true
  :false :true (1 1 =) cond)

;; Increases value while condition is true
(lifoo-asseq 3
  0 (inc dup 3 >) while)

;; Maps lambda over sequence
(lifoo-asseq #(2 4 6)
  #(1 2 3) (2 *) map)

;; Removes all chars matching block 
(lifoo-asseq "bcdbr"
  "abacadabra" (#\a eq?) filter)

;; Sets variable;
;; opens new environment and sets new value,
;; and closes environment and returns value

(lifoo-asseq 42
  :foo 42 set 
  begin :foo 43 set end 
  :foo get)

;; Throws value, catches it and returns (:caught . value)
(lifoo-asseq '(:caught . :frisbee)
  :frisbee throw (:caught cons) catch)

;; Backs up and restores stack to/from current environment
(lifoo-asseq #(1 2)
  1 2 backup 
  3 4 restore 
  stack)
      
;; Redefines :+ to drop arguments and return fixed value
(lifoo-asseq 42
  (drop drop 42) :+ define
  1 2 +)
      
;; Defines word and returns first element from it's source
(lifoo-asseq '+
  (+ 1 2) :foo define
  :foo word source first)

;; Runs Lisp code inline that modifies the stack
(lifoo-asseq 43
  42
  (lifoo-push (1+ (lifoo-pop))) 
  lisp eval)

;; Creates an unbuffered channel;
;; starts a new thread that puts value in channel;
;; gets value from channel in original thread;
;; joins thread and returns result from thread
;; consed to value from channel

(lifoo-asseq '(:done . 3)
  0 chan (1 2 + chan-put :done) thread swap 
  chan-get swap drop swap 
  join-thread cons)))
```

### words
Forth likes to call functions words, and Lifoo keeps with that tradition. Lifoo comes with a modest but growing, modular set of built-in words. Words can be defined in either Lisp or Lifoo, and the goal is to gradually migrate as much functionality as possible to pure Lifoo. Functions for defining, looking up and un-defining words are also provided. 

```
;; Pops $rhs and $lhs,
;; and pushes T if they compare equal

(define-word :eq? () cmp 0 =)

;; Pops $rhs and $lhs,
;; and pushes result of comparing $lhs to $rhs

(define-lisp-word :cmp ()
  (let ((rhs (lifoo-pop))
        (lhs (lifoo-pop)))
    (lifoo-push (compare lhs rhs))))

;; Wraps parsed forms in handler-case with previous
;; form as handler
(define-macro-word :catch (f fs)
  (list
    (cons f `(handler-case
                 (progn
                   ,@(reverse (mapcar #'rest (rest fs))))
               (lifoo-throw (c)
                 (lifoo-push (value c))
                 (lifoo-eval ',(first (first fs))))))))
```

### implementation

```
(defvar *lifoo* nil)
(defvar *lifoo-init* (make-hash-table :test 'equal))

(defmacro define-init (tags &body body)
  "Defines init for TAGS around BODY"
  `(setf (gethash ',tags *lifoo-init*)
         (lambda (exec)
           (with-lifoo (:exec exec)
             ,@body))))

(defmacro define-macro-word (name (f-arg fs-arg &key exec)
                             &body body)
  "Defines new macro word NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define-macro (keyword! ',name)
                       (lambda (,f-arg ,fs-arg)
                         ,@body)
                       :exec (or ,exec *lifoo*)))

(defmacro define-lisp-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  (let ((id (keyword! name)))
    `(lifoo-define ,id
                   (make-lifoo-word :id ,id
                                    :source ',body
                                    :fn (lambda () ,@body))
                   :exec (or ,exec *lifoo*))))

(defmacro define-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(lifoo-define ',name
                 (make-lifoo-word :id ,(keyword! name)
                                  :source ',body)
                 :exec (or ,exec *lifoo*)))

(defmacro define-binary-words ((&key exec) &rest forms)
  "Defines new words in EXEC for FORMS"
  (with-symbols (_lhs _rhs)
    `(progn
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op) (:exec ,exec)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 forms))))

(defmacro do-lifoo ((&key (env t) exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo* (make-lifoo))
                :env ,env)
     (lifoo-eval ',body)
     (lifoo-pop)))

(defmacro with-lifoo ((&key env exec) &body body)
  "Runs body with *LIFOO* bound to EXEC or new; environment
   is bound to ENV if not NIL, or copy of current if T"
  `(let ((*lifoo* (or ,exec (make-lifoo))))
     (when ,env (lifoo-begin :env ,env))
     (unwind-protect (progn ,@body)
       (when ,env (lifoo-end)))))

(defstruct (lifoo-word (:conc-name))
  id
  trace?
  source fn)

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  envs logs
  (stack (make-array 3 :adjustable t :fill-pointer 0))
  (macro-words (make-hash-table :test 'eq))
  (words (make-hash-table :test 'eq)))

(define-condition lifoo-break (condition) ()) 

(define-condition lifoo-error (simple-error) ()) 

(define-condition lifoo-throw (condition)
  ((value :initarg :value :reader value)))

(defun lifoo-break ()
  "Signals break"
  (signal 'lifoo-break))

(defun lifoo-error (fmt &rest args)
  "Signals error with message from FMT and ARGS"
  (error 'lifoo-error :format-control fmt :format-arguments args))

(defun lifoo-throw (val)
  "Throws VAL"
  (signal 'lifoo-throw :value val))

(defun lifoo-init (tags &key (exec *lifoo*))
  "Runs all inits matching tags in EXEC"
  (let ((cnt 0))
    (do-hash-table (ts fn *lifoo-init*)
      (when (or (eq t tags)
                (null (set-difference ts tags)))
        (funcall fn exec)
        (incf cnt)))
    (assert (not (zerop cnt))))
  exec)

(defun lifoo-read (&key (in *standard-input*))
  "Reads Lifoo code from IN until end of file"
  (let ((eof? (gensym)) (more?) (expr))
    (do-while ((not (eq (setf more? (read in nil eof?)) eof?)))
      (push more? expr))
    (nreverse expr)))

(defun lifoo-parse (expr &key (exec *lifoo*))
  "Parses EXPR and returns code compiled for EXEC"
  (labels
      ((parse (fs acc)
         (if fs
             (let ((f (first fs)))
               (parse
                (rest fs)
                (cond
                  ((or (arrayp f)
                       (characterp f)
                       (keywordp f)
                       (numberp f)
                       (stringp f))
                   (cons (cons f `(lifoo-push ,f)) acc))
                  ((consp f)
                   (cons (cons f `(lifoo-push ',f)) acc))
                  ((null f)
                   (cons (cons f `(lifoo-push nil)) acc))
                  ((eq f t)
                   (cons (cons f `(lifoo-push t)) acc))
                  ((symbolp f)
                   (let* ((id (keyword! f))
                          (mw (lifoo-macro-word id)))
                     (if mw
                         (funcall mw f acc)
                         (cons (cons f `(lifoo-call ,id)) acc))))
                  ((lifoo-word-p f)
                   (cons (cons f `(lifoo-call ,f)) acc))
                  (t
                   (error "invalid form: ~a" f)))))
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

(defun lifoo-macro-word (id &key (exec *lifoo*))
  "Returns macro word for ID from EXEC, or NIL if missing"
  (gethash (keyword! id) (macro-words exec)))

(defun lifoo-define-macro (id word &key (exec *lifoo*))
  "Defines ID as macro WORD in EXEC"
  (setf (gethash (keyword! id) (macro-words exec)) word))

(defun lifoo-define (id word &key (exec *lifoo*))
  "Defines ID as WORD in EXEC"
  (setf (gethash (keyword! id) (words exec)) word))

(defun lifoo-undefine (word &key (exec *lifoo*))
  "Undefines word for ID in EXEC"
  (remhash (words exec) (if (lifoo-word-p word)
                            (id word)
                            (keyword! word))))

(defun lifoo-word (id &key (exec *lifoo*))
  "Returns word for ID from EXEC, or NIL if missing"
  (if (lifoo-word-p id)
      id
      (gethash (keyword! id) (words exec))))

(defun lifoo-push (val &key (exec *lifoo*))
  "Pushes VAL onto EXEC stack"  
  (vector-push-extend val (stack exec))
  val)

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops and returns value from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (let ((val (vector-pop (stack exec))))
      val)))

(defun lifoo-peek (&key (exec *lifoo*))
  "Returns top of EXEC stack"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (unless (zerop fp)
      (aref stack (1- fp)))))

(defun (setf lifoo-peek) (val &key (exec *lifoo*))
  "Replaces top of EXEC stack with VAL"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (assert (not (zerop fp)))
    (setf (aref stack (1- fp)) val)))

(defun lifoo-trace? (word)
  "Returns T if WORD is traced, otherwise NIL"
  (trace? word))

(defun (setf lifoo-trace?) (on? word)
  "Enables/disables trace for WORD"
  (setf (trace? word) on?))

(defun lifoo-log (msg &key (exec *lifoo*))
  "Logs MSG in EXEC"
  (push (list :log msg) (logs exec)))

(defun lifoo-dump-log (&key (exec *lifoo*))
  "Returns logs from EXEC"
  (let ((log(logs exec)))
    (setf (logs exec) nil)
    log))

(defun lifoo-print-log (log &key (out *standard-output*))
  "Prints log to OUT"
  (dolist (e log)
    (apply #'format out
           (ecase (first e)
             (:break "BREAK ~a ~a~%")
             (:enter "ENTER ~a ~a~%")
             (:exit  "EXIT  ~a ~a~%")
             (:log   "LOG   ~a~%"))
           (rest e))))

(defun lifoo-stack (&key (exec *lifoo*))
  "Returns stack for EXEC"
  (stack exec))

(defun lifoo-begin (&key (env t) (exec *lifoo*))
  "Opens ENV or new environment if T in EXEC"
  (push (if (eq t env) (copy-list (lifoo-env)) env)
        (envs exec)))

(defun lifoo-end (&key (exec *lifoo*))
  "Closes current environment in EXEC"
  (pop (envs exec)))

(defun lifoo-env (&key (exec *lifoo*))
  "Returns current environment"
  (first (envs exec)))

(defun (setf lifoo-env) (env &key (exec *lifoo*))
  "Replaces current environment"
  (rplaca (envs exec) env))

(defun lifoo-get (var)
  "Returns value of VAR in EXEC"
  (rest (assoc var (lifoo-env) :test #'eq))) 

(defun (setf lifoo-get) (val var)
  "Sets value of VAR in EXEC to VAL"
  (push (cons var val) (lifoo-env))
  val)

(defun lifoo-del (var)
  "Deletes VAR from EXEC and returns value"
  (setf (lifoo-env)
        (delete var (lifoo-env) :key #'first :test #'eq))) 
```

### repl
Writing a basic REPL is trivial given above implementation.

```
(defun lifoo-repl (&key (exec (lifoo-init :exec (make-lifoo)))
                        (in *standard-input*)
                        (prompt "Lifoo>")
                        (out *standard-output*))
  "Starts a REPL for EXEC with input from IN and output to OUT,
   using PROMPT"
  (with-lifoo (:exec exec :env t)
    (tagbody
       start
         (format out "~%~a " prompt)
         (when-let (line (read-line in nil))
           (unless (string= "" line)
             (with-input-from-string (in line)
               (restart-case
                   (progn
                     (lifoo-eval (lifoo-read :in in))
                     (format out "~a~%" (lifoo-pop)))
                 (ignore ()
                   :report "Ignore error and continue.")))
             (go start))))))

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
