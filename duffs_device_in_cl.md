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
As of right now, cooperative tasks are around 10x faster than preemptive threads in Lifoo; but there is plenty more low hanging fruit left in the task code path. ```cl4l:*cl4l-speed*``` may be set to a value between 1 and 3 to optimize most of the code involved in one go.

```
(lifoo task perf)              0.94
(lifoo task spawn perf)       11.44
TOTAL                         12.38


(define-test (:lifoo :task :perf)
  (lifoo-asseq 0
    0
    1 (dec task-yield
       dec task-yield
       dec) task drop
    1 (inc task-yield
       inc task-yield
       inc) task drop
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
