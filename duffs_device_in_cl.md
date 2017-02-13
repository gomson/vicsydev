# [vicsy/dev](https://github.com/codr4life/vicsydev) | Duff's Device in Common Lisp
posted Feb 13th 2017, 01:00 am

### preramble
It's a shame that many programming languages fail to provide wholehearted support for green threads, aka. cooperative scheduling; and that fewer still leave enough rope to roll your own. Even Common Lisp fails the test, as it neither provides green threads nor the power needed to implement them. 

### cooperative scheduling
One popular urban myth in Common Lisp circles is that green threads bring nothing to the table since it's preemptive threads are fast enough; which misses the point that cooperative scheduling is a useful, complementary approach to structuring software; as long as performance is at least comparable; and as an added bonus, the performance profile is more consistent and predictable than for preemptive threads.

### Duff's Device
In C, it's popular to build green threads on top of ```switch``` interleaved with user code; known as Duff's Device. The approach unfortunately doesn't play well with the compiler and requires serious hacks to be usable. Implementing the same idea in Lisp takes some imagination, as there is nothing comparable to ```switch```; ```tag-body``` needs jump labels in clear text; and ```case``` doesn't allow fall-through, which is crucial. Branching around each statement is a possibility, Common Lisp compilers are pretty hard core when it comes to optimisation, which means it's possible to get away with it; but modifying unknown Lisp code on statement level is a [tar pit](http://quickdocs.org/cl-cont/api).

### Forth
Writing code Forth style allows painless statement level translation because of the simple, linear syntax. That's one of the reasons I chose Forth as the basis of [Lifoo](https://github.com/codr4life/lifoo), a new Forth-based language fused with Common Lisp that I'm working on. By standing on the shoulders of [Lifoo](https://github.com/codr4life/lifoo), we finally get enough leverage to implement the idea in a reasonable amount of reasonable code.

```
Lifoo> 41 1 (1 task-yield +) task 
       run run
       done? swap result swap drop cons

(42 . T)

Lifoo> 42 1 (1 task-yield +) task source

(LET* ((#:G11886 (LIFOO-VAR *TASK*)) (#:G11887 (TASK-LINE #:G11886)))
         (DECLARE (IGNORABLE #:G11886 #:G11887))
         (WHEN (AND (< #:G11887 1) (NOT (TASK-YIELDING? #:G11886)))
           (SETF (TASK-LINE #:G11886) 1)
           (LIFOO-PUSH 2))
         (WHEN (AND (< #:G11887 2) (NOT (TASK-YIELDING? #:G11886)))
           (SETF (TASK-LINE #:G11886) 2)
           (LIFOO-CALL [word: task-yield nil]))
         (WHEN (AND (< #:G11887 3) (NOT (TASK-YIELDING? #:G11886)))
           (SETF (TASK-LINE #:G11886) 3)
           (IF (FUNCALL (TYPE-CHECKER [word: + (number number)])
                        (STACK *LIFOO*))
               (LIFOO-CALL [word: + (number number)])
               (LIFOO-CALL '+))))

Lifoo> :foo 1 () task queue 
       :bar 1 () task queue tasks

([task: G12549 @0 (BAR) done? NIL] 
 [task: G12548 @0 (FOO) done? NIL])

Lifoo> 38 
       1 (inc task-yield inc) task queue
       1 (inc task-yield inc) task queue
       finish-tasks drop

42


(defparameter *task* (gensym))
(defparameter *tasks* (gensym))

(define-lifoo-init (:task)
  ;; Yields processor to other tasks
  (define-lisp-word :task-yield () ()
    (let ((task (lifoo-var *task*)))
      (assert task)
      (setf (task-yielding? task) t)))

  ;; Pops $code and $num-args,
  ;; and pushes new task
  (define-macro-word :task (in out)
    (declare (ignore in))
    (let* ((fst (first out))
           (source (first fst))
           (code (lifoo-compile-task source))
           (fn (eval `(lambda ()
                        ,(cl4l-optimize)
                        ,code))))
      (cons (cons (first fst)
                  `(lifoo-push (lifoo-task ',code ,fn
                                           (lifoo-pop))))
            (rest out))))

  ;; Pops $task and pushes to run queue
  (define-lisp-word :queue (lifoo-task) ()
    (push (lifoo-pop) (lifoo-var *tasks*)))

  ;; Pushes current run queue
  (define-lisp-word :tasks () ()
    (lifoo-push-expr (lifoo-var *tasks*)))
  
  ;; Pops $task and pushes its source
  (define-lisp-word :source (lifoo-task) ()
    (let ((task (lifoo-pop)))
      (lifoo-push (task-source task))))

  ;; Pops $task and pushes its stack
  (define-lisp-word :task-stack (lifoo-task) ()
    (lifoo-push (nreverse (map 'list
                               #'lifoo-val
                               (task-stack (lifoo-pop))))))

  ;; Runs $1 until next yield or done?
  (define-lisp-word :run (lifoo-task)
      ()
    (lifoo-run-task (lifoo-peek)))

  ;; Runs all tasks that are not done? once
  (define-lisp-word :run-tasks () ()
    (lifoo-push (lifoo-run-tasks)))

  ;; Runs tasks until all are done?
  (define-lisp-word :finish-tasks () (:speed 1)
    (let ((tot 0))
      (do-while ((multiple-value-bind (more? cnt) (lifoo-run-tasks)
                   (incf tot cnt)
                   (> more? 0))))
      (lifoo-push tot)))
  
  ;; Pushes T if $1 is done?,
  ;; NIL otherwise
  (define-lisp-word :done? (lifoo-task) ()
    (lifoo-push (task-done? (lifoo-peek))))

  ;; Pushes result ($1) of $1
  (define-lisp-word :result (lifoo-task) (:speed 1)
    (lifoo-push (lifoo-task-result (lifoo-peek)))))

(defstruct (lifoo-task (:conc-name task-))
  (id (gensym)) stack source fn (line 0 :type fixnum)
  (done? nil :type boolean) (yielding? nil :type boolean))

(define-fn lifoo-compile-task (source &key (exec *lifoo*))
  (:speed 1)
  "Returns compiled lambda for CODE in EXEC"
  (let ((_task (gensym)) (_line (gensym)) (line 0))
    (declare (type fixnum line))
    (let ((code
            (mapcar (lambda (f)
                      (incf line)
                      `(when (and (< ,_line ,line)
                                  (not (task-yielding? ,_task)))
                         (setf (task-line ,_task) ,line)
                         ,f))
                    (lifoo-compile source :exec exec))))
      `(let* ((,_task (lifoo-var *task*))
              (,_line (task-line ,_task)))
         (declare (ignorable ,_task ,_line))
         ,@code))))

(define-fn lifoo-task (src fn num-args &key (exec *lifoo*))
  (:speed 1)
  "Returns new task for CODE with NUM-ARGS arguments in EXEC"
  (let* ((stack (stack exec))
         (len (length stack))
         (stack2 (if (= len num-args)
                     stack
                     (subseq stack (- len num-args))))
         (task-stack (make-array num-args
                                 :adjustable t
                                 :fill-pointer num-args
                                 :initial-contents stack2)))
    (make-lifoo-task :source src
                     :fn fn
                     :stack task-stack)))

(define-fn lifoo-task-result (task) ()
  "Returns current top of stack for TASK"
  (let ((stack (task-stack task)))
    (unless (zerop (fill-pointer stack))
      (lifoo-val (aref stack (1- (length stack)))))))

(define-fn lifoo-run-task (task &key (exec *lifoo*))
  (:speed 1)
  "Runs TASK once in EXEC"
  (assert (not (task-done? task)))
  (let ((prev (stack exec)))
    (setf (lifoo-var *task* :exec exec) task)
    (setf (stack exec) (task-stack task))
    (unwind-protect
         (funcall (task-fn task))
      (setf (stack exec) prev)
      (setf (lifoo-var *task* :exec exec) nil)))
  (if (task-yielding? task)
      (setf (task-yielding? task) nil)
      (setf (task-done? task) t)))

(define-fn lifoo-run-tasks (&key (exec *lifoo*)) (:speed 1)
  "Runs all tasks in EXEC that are not DONE? once"
  (let ((rem 0) (cnt 0))
    (labels ((rec (in out)
               (if in
                   (let ((task (first in)))
                     (unless (task-done? task)
                       (lifoo-run-task task :exec exec)
                       (incf cnt))
                     (rec (rest in)
                          (if (task-done? task)
                              out
                              (progn
                                (incf rem)
                                (cons task out)))))
                   (nreverse out))))
      (setf (lifoo-var *tasks* :exec exec)
            (rec (lifoo-var *tasks*) nil)))
    (values rem cnt)))
```

### semantics
This implementation has the added quirk of only allowing yields from task scope, yields from nested scopes are dealt with as soon as the stack unwinds.

```
Lifoo> 40 1 ((task-yield inc)@ call 1 +) task 
       run swap
41

Lifoo> 40 1 ((task-yield inc)@ call 1 +) task 
       run run result swap drop cons

(42 . 41)
```

### performance
As of right now, cooperative tasks are around 10x faster than preemptive threads in [Lifoo](https://github.com/codr4life/lifoo); but there is plenty more low hanging fruit left in the task code path. ```cl4l:*cl4l-speed*``` may be set to a value between 1 and 3 to optimize most of the code involved in one go.

```
(lifoo task perf)              0.94
(lifoo task spawn perf)       11.44
TOTAL                         12.38


(define-test (:lifoo :task :perf)
  (lifoo-asseq 0
    0
    1 (dec task-yield
       dec task-yield
       dec task-yield) task drop
    1 (inc task-yield
       inc task-yield
       inc task-yield) task drop
    finish-tasks drop))

(define-test (:lifoo :task :spawn :perf)
  (lifoo-asseq '(0 . 0)
    0 semaphore
    0 semaphore
    2 (signal swap wait swap
       signal swap wait swap
       signal swap wait)@ spawn
    swing
    2 (signal swap wait swap
       signal swap wait swap
       signal swap wait)@ spawn
    wait drop 2 pick wait
    drop count swap drop swap count swap drop cons))
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
