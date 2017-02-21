# [vicsy/dev](https://github.com/codr4life/vicsydev) | Faster Green Threads with Duff's Device
posted Feb 17th 2017, 01:00am

### preramble
It's a shame that many programming languages fail to provide wholehearted support for green threads, and that fewer still leave enough rope to roll your own. Cooperative scheduling is a useful, complementary approach to structuring software. It's often faster, sometimes much faster; and comes with a more consistent and predictable performance profile than preemptive threads. Every implementation of green threads in Common Lisp that I've come across unfortunately uses [cl-cont](http://quickdocs.org/cl-cont/api), which is too slow and messes too much with the code. 

### Duff's Device
In C circles, it's common practice to build green threads on top of ```switch``` interleaved with user code; known as Duff's Device. Implementing the same idea in Lisp takes some imagination; ```tagbody``` needs jump labels in clear text, and ```case``` doesn't fall through. The not entirely obvious solution is to put a ```case``` inside a ```tagbody``` to do the actual jumping. 

```
CL4L-TASK> (run-task (define-task ()
                       (format t "before~%")
                       (yield 42)
                       (format t "after~%")))
before
42

CL4L-TASK> (macroexpand-1 
             '(define-task ()
                 (format t "before~%")
                 (yield 42)
                 (format t "after~%")))
                 
(MAKE-TASK :ID (GENSYM) :FN
         (LAMBDA (NEXT)
           (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0)))
           (SETF (TASK-RESULT *TASK*) NIL)
           (TAGBODY
             (ECASE NEXT 
               (G7722 (GO G7722)) 
               (G7720 (GO G7720)))
            G7720
             (FORMAT T "before~%")
             (SETF (TASK-NEXT *TASK*) 'G7722
                   (TASK-RESULT *TASK*) 42)
             (GO G7721)
            G7722
             (FORMAT T "after~%")
             (SETF (TASK-DONE? *TASK*) T)
            G7721))
         :NEXT 'G7720)
```


### implementation

```
(defvar *task*)

(defstruct (task (:constructor mk-task))
  id fn next result done?)

(defmacro define-task ((&key (id '(gensym)) speed) &body body)
  "Returns new task with ID around BODY"
  (let* ((start-tag (symbol! (gensym)))
         (yield-tag (symbol! (gensym)))
         (tags (list start-tag))
         (bdy (mapcan (lambda (f)
                        (if (eq (first f) 'yield)
                            (let ((tag (symbol! (gensym))))
                              (push tag tags)
                              `((setf
                                 (task-next *task*) ',tag
                                 (task-result *task*) ,(first
                                                        (rest f)))
                                (go ,yield-tag)
                                ,tag))
                            `(,f)))
                      body)))
    `(make-task
       :id ,id
       :fn (lambda (next)
             ,(cl4l-optimize :speed speed)
             (setf (task-result *task*) nil)
             (tagbody
                (ecase next
                  ,@(mapcar (lambda (tag)
                              `(,tag (go ,tag)))
                     tags))
                ,start-tag
                ,@bdy
                (setf (task-done? *task*) t)
                ,yield-tag))
       :next ',start-tag)))

(defun run-task (self)
  "Runs SELF until next yield"
  (assert (not (task-done? self)))
  (let ((*task* self))
    (funcall (task-fn self) (task-next self)))
  (task-result self))

(defun finish-task (self)
  " Runs SELF until done"
  (do-while ((not (task-done? self)))
    (run-task self)))

(defun run-tasks (lst)
  "Runs all tasks in lst that are not done, 
   returns number of remaining tasks"
  (let ((rem 0))
    (dolist (task lst)
      (unless (task-done? task)
        (run-task task)
        (unless (task-done? task)
          (incf rem))))
    rem))

(defun finish-tasks (lst)
  "Runs tasks in LST until all are done"
  (do-while ((> (run-tasks lst) 0))))
```

### performance
Included below are the first benchmark results, tasks compared to preemptive threads with semaphores to force alternate execution. The massive performance boost more than makes up for the inconvenience of only supporting yields from task scope. ```cl4l:*cl4l-speed*``` may be set to a value between 1 and 3 to optimize most of the code involved in one go.

```
CL4L-TASK> (cl4l-test:run-suite '(:cl4l :task :perf) :reps 10000)
(cl4l task perf)              0.012
(cl4l task perf preemtive)    17.75
TOTAL                         17.76


(defparameter *perf-reps* 3)

(define-test (:cl4l :task :perf)
  (let ((cnt1 0) (cnt2 0))
    (declare (type fixnum cnt1)
             (type fixnum cnt2)
             (type fixnum *perf-reps*))
    (let* ((tasks (list
                   (define-task ()
                     start
                     (yield)
                     (when (< (incf cnt1) *perf-reps*)
                       (go start)))
                   (define-task ()
                     start
                     (yield)
                     (when (< (incf cnt2) *perf-reps*)
                       (go start))))))
      (finish-tasks tasks))))

(define-test (:cl4l :task :perf :preemtive)
  (let* ((sem1 (make-semaphore))
         (sem2 (make-semaphore))
         (thread1 (make-thread (lambda ()
                                 (define-body ()
                                     (dotimes (_ *perf-reps*)
                                       (semaphore-signal sem2)
                                       (semaphore-wait sem1))))))
         (thread2 (make-thread (lambda ()
                                 (define-body ()
                                   (dotimes (_ *perf-reps*)
                                     (semaphore-signal sem1)
                                     (semaphore-wait sem2)))))))
    (join-thread thread1)
    (join-thread thread2)))
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/cl4l).

peace, out
