# [vicsy/dev](https://github.com/codr4life/vicsydev) | Faster Exceptions in Common Lisp
posted Feb 17th 2017, 03:00 am

### preramble
One of the ideas I've been playing around with is implementing faster exceptions using goto. Unfortunately, most languages fail to even make this possible; and fewer still make it easy. This post describes an implementation of the idea in Common Lisp and compares it's performance to regular conditions.

### use
The ```with-catch``` macro provides a ```throw-value``` macro within it's body and returns caught value or NIL.

```
CL-USER> (cl4l-utils:with-catch 
             (format t "before~%")
             (cl4l-utils:throw-value 42)
             (format t "after~%"))
before
42
```

### implementation
Implementation is trivial since it delegates all heavy lifting to ```tagbody``` and ```macrolet```.

```
(defmacro with-catch (&body body)
  "Executes BODY while catching thrown values,
   returns caught value or NIL"
  (let ((_catch (symbol! (gensym))) (_thrown (gensym)))
    `(macrolet ((throw-value (val)
                  `(progn
                     (setf ,',_thrown ,val)
                     (go ,',_catch))))
       (let ((,_thrown))
         (tagbody
            ,@body
            ,_catch)
         ,_thrown))))
```

### performance
Finally we arrive at the point where we get some numbers to go with the intuition, throwing and catching is around 25 times faster than signalling and handling conditions on my setup.

```
CL-USER> (cl4l-test:run-suite '(:cl4l :with-catch :perf) 
                              :reps 100000000)

(cl4l with-catch perf)        1.136
(cl4l with-catch perf cond)   27.66
TOTAL                          28.8


(define-test (:cl4l :with-catch :perf :cond)
  (handler-case
      (progn
          (error "message")
          (assert nil))
    (error ())))

(define-test (:cl4l :with-catch :perf)
  (with-catch
    (throw-value "message")
    (assert nil)))
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/cl4l).

peace, out
