# [vicsy/dev](https://github.com/codr4life/vicsydev) | Faster exceptions without the ceremony
posted Feb 11th 2017, 02:00 am

### preramble
I've often dreamt of being able to implement green threads, exceptions and more from user code without pulling my hair out in the process. There are so many ideas I would like to play around with when it comes to flow control, so much room for exploration. One of the ideas I've been playing around with is implementing throw and catch using jump tables. Unfortunately, most languages fail to even make this possible; and fewer still make it easy. This post describes an implementation of this idea in [Lispy Forth](https://github.com/codr4life/lifoo).

### Lisp
Even Common Lisp, the supposed king of customisation; fails the test by making general purpose code translation too difficult. Wrapping code around forms is fine; but as soon as the need to transform unknown code on statement level arises, it turns into a [tar pit](http://quickdocs.org/cl-cont/api). 

### [Lifoo](https://github.com/codr4life/lifoo)
One advantage of Forth syntax is that it's linear nature makes it possible to pull these kinds of tricks. Using a [Lispy Forth](https://github.com/codr4life/lifoo) with macro support turns possible into trivial.

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
          (WHEN (LIFOO-VAR *LIFOO-THROWING*) (GO #:G78818))
          (LIFOO-CALL [word: throw (t)])
          (WHEN (LIFOO-VAR *LIFOO-THROWING*) (GO #:G78818))
          (LIFOO-PUSH :FAIL)
         #:G78818
          (LIFOO-PUSH (LIFOO-VAR *LIFOO-THROWING*))
          (SETF (LIFOO-VAR *LIFOO-THROWING*) NIL)))
          
          
(define-lisp-word :throw (t) ()
  (setf (lifoo-var *lifoo-throwing*) (lifoo-pop)))

(define-macro-word :catch (in out)
  (let* ((catch-tag (gensym))
         (in-code (lifoo-compile (first (first out))))
         (out-code (apply #'join
                          `(when (lifoo-var *lifoo-throwing*)
                             (go ,catch-tag))
                          in-code)))
    (cons (cons in `(tagbody
                       ,@out-code
                       ,catch-tag
                       (lifoo-push (lifoo-var *lifoo-throwing*))
                       (setf (lifoo-var *lifoo-throwing*) nil)))
          (rest out))))
```

### semantics
This implementation has the added feature (or bug, depending on perspective) of requiring thrown values to be caught and thrown again inside nested code blocks to get "normal" exception semantics; if not, the code will continue running as usual until it hits a catch scope.

```
Lifoo> (((:up throw "fail" error) catch throw)@
        (:always)@ always) 
       catch cons

(:UP . :ALWAYS)
```

### performance
Unfortunately, embedded languages are seldom as fast as their hosts; beating raw Lisp conditions in [Lifoo](https://github.com/codr4life/lifoo) is not happening any time soon. But since [Lifoo](https://github.com/codr4life/lifoo) provides a bridge to Lisp conditions, called signals in [Lifoo](https://github.com/codr4life/lifoo)-speak; it's still possible to compare the different approaches, all else being mostly equal. 

```
LIFOO> (cl4l-test:run-suite '(:lifoo :throw :perf) :warmup 10 
                                                   :reps 10000)
(lifoo throw perf)            15.52
(lifoo throw perf lisp)       0.068
(lifoo throw perf signal)     16.38
TOTAL                         31.97


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
So there you have it; wrapping code in jump tables with checks between statements seems comparable to exceptions for short blocks; I know I'll sleep better. This also gives a hint of the worst case performance ratio between Lifoo and Lisp right now; around three hundred times slower.

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
