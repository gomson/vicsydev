# [vicsy/dev](https://github.com/codr4life/vicsydev) | Duff's Device in Common Lisp
posted Feb 16th 2017, 9:00 pm

### preramble
It's a shame that many programming languages fail to provide wholehearted support for green threads, aka. cooperative scheduling; and that fewer still leave enough rope to roll your own. 

### cooperative scheduling
One popular urban myth in Common Lisp circles is that green threads bring nothing to the table since it's preemptive threads are fast enough; which misses the point that cooperative scheduling is a useful, complementary approach to structuring software; as long as performance is at least comparable; and as an added bonus, the performance profile is more consistent and predictable than for preemptive threads.

### Duff's Device
In C, it's popular to build green threads on top of ```switch``` interleaved with user code; known as Duff's Device. Implementing the same idea in Lisp takes some imagination; ```tag-body``` needs jump labels in clear text, and ```case``` doesn't fall through. The not entirely obvious solution is to put a ```case``` inside a ```tagbody``` to do the actual jumping. 

### Forth
While the approach taken here is perfectly doable in straight Common Lisp, I'll leave that implementation as an exercise for someone else; the rest of this guided tour will use [Lifoo](https://github.com/codr4life/lifoo), an embedded Forth that compiles to Lisp.

```
Lifoo> 41 1 (1 task-yield +) task 
       run run
       done? swap result swap drop cons

(42 . T)

Lifoo> 42 1 (1 task-yield +) task source

(TAGBODY
  (ECASE (TASK-NEXT (LIFOO-VAR *TASK*))
    (G1690 (GO G1690))
    (G8619 (GO G8619))
    (G1691 (GO G1691)))
G1690
  (LIFOO-PUSH 1)
  (PROGN (SETF (TASK-NEXT (LIFOO-VAR *TASK*)) 'G8619) (GO G1691))
G8619
  (IF (FUNCALL (TYPE-CHECKER [word: + (number number)]) 
               (STACK *LIFOO*))
    (LIFOO-CALL [word: + (number number)])
    (LIFOO-CALL '+))
  (SETF (TASK-DONE? (LIFOO-VAR *TASK*)) T)
G1691)
```

### performance
As of right now, cooperative tasks are around 10x faster than preemptive threads. ```cl4l:*cl4l-speed*``` may be set to a value between 1 and 3 to optimize most of the code involved in one go.

```
LIFOO> (cl4l-test:run-suite '(:lifoo :task :perf) :reps 100)
(lifoo task perf)             1.012
(lifoo task spawn perf)       13.23
TOTAL                         14.24


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
