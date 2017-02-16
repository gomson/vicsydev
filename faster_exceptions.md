# [vicsy/dev](https://github.com/codr4life/vicsydev) | Faster exceptions with Lispy Forth
posted Feb 16th 2017, 9:00 pm

### preramble
I've often dreamt of being able to implement green threads, exceptions and more from user code without pulling my hair out in the process. There are so many ideas I would like to play around with when it comes to flow control, so much room for exploration. One of the ideas I've been playing around with is implementing throw and catch using regular branching. Unfortunately, most languages fail to even make this possible; and fewer still make it easy. This post describes an implementation of this idea in [Lispy Forth](https://github.com/codr4life/lifoo).

```
CL-USER> (lifoo:lifoo-repl)
Welcome to Lifoo,
press enter on empty line to evaluate,
exit ends session

Lifoo> (:frisbee throw :fail) catch

:FRISBEE

Lifoo> ((:frisbee throw :fail) catch) compile

(PROGN
  (TAGBODY
    (LIFOO-PUSH :FRISBEE)
    (PROGN 
      (SETF (LIFOO-VAR *THROWING*) (LIFOO-POP)) 
      (GO G2715))
    (LIFOO-PUSH :FAIL)
  G2715
    (LIFOO-PUSH (LIFOO-VAR *THROWING*))
    (SETF (LIFOO-VAR *THROWING*) NIL)))


(define-macro-word :throw (in out)
  (cons (cons in
              `(progn
                 (setf (lifoo-var *throwing*) (lifoo-pop))
                 (go ,*catch*)))
        out))

(define-macro-word :catch (in out)
  (let* ((code (lifoo-compile (first (first out)))))
    (cons (cons in `(tagbody
                       ,@code
                       ,*catch*
                      (lifoo-push (lifoo-var *throwing*))
                      (setf (lifoo-var *throwing*) nil)))
          (rest out))))
```

### performance
Unfortunately, embedded languages are seldom as fast as their hosts; beating raw Lisp conditions in [Lifoo](https://github.com/codr4life/lifoo) is not happening any time soon. But since [Lifoo](https://github.com/codr4life/lifoo) provides a bridge to Lisp conditions, called signals in [Lifoo](https://github.com/codr4life/lifoo)-speak; it's still possible to compare the different approaches, all else being mostly equal. 

```
CL-USER> (cl4l-test:run-suite '(:lifoo :throw :perf) :reps 10000)
(lifoo throw perf)            8.576
(lifoo throw perf lisp)       0.072
(lifoo throw perf sig)        24.12
TOTAL                         32.77


(define-test (:lifoo :throw :perf)
  (lifoo-asseq :ok
    (:ok throw
     :fail)
    catch))

(define-test (:lifoo :throw :perf :signal)
  (lifoo-asseq "ok"
    ("ok" error
     :fail)@
    handle error-message))

(define-test (:lifoo :throw :perf :lisp)
  (dotimes (_ *reps*)
    (handler-case
        (progn
          (error "message")
          (assert nil))
      (error ()))))


(defmacro lifoo-asseq (res &body body)
  "Asserts that evaluating BODY pushes value that compares equal 
   to RES"
  `(asseq ,res
     (let* ((compiled 
              (lifoo-compile '(reset clear ,@body)))
            (fn (eval `(lambda ()
                         ,(lifoo-optimize)
                         ,@compiled))))
       (dotimes (_ *reps*)
         (funcall fn))
       (lifoo-pop))))
```

### conclusion
So there you have it; jump tables seem to offer a faster approach than full exceptions for short blocks of code. This also gives a hint of the worst case performance ratio between [Lifoo](https://github.com/codr4life/lifoo) and Lisp right now; around 400x slower.

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
