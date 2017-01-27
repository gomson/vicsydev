# [vicsy/dev](https://github.com/codr4life/vicsydev) | implementing defer()
posted Jan 27th 2017, 2:03 pm

### preramble
Golang's designers had good reasons to choose 'defer' as the one true mechanism for executing code on scope exit. The strategies for scoping resources (WITH-macros, exceptions and others) break down as soon as allocation and/or clean up depend on code in the scope. Implementing a comparable mechanism in other languages is perfectly doable, this post describes an implementation in Common Lisp that adds the capability to defer to named/outer scopes.

### usage

```lisp
(define-test (:defer)
  (assert (string= "hello world"
                   (with-output-to-string (out)
                     (with-defer outer
                       (with-defer nil
                         (defer (format out "hello"))
                         (defer-outer (format out "world")))
                       (format out " "))))))
```

### implementation

```lisp
(defmacro with-defer (name &body body)
  (let ((_name (or name (gensym))))
    `(macrolet ((,(symbol! 'defer- _name) (&body forms)
                  (let ((n ',_name))
                    `(push (lambda () ,@forms) ,n)))
                (defer (&body forms)
                  `(,(symbol! 'defer- ',_name) ,@forms)))
       (let ((,_name))
         (unwind-protect (progn,@body)
           (dolist (fn ,_name)
             (funcall fn)))))))
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of this idea and more <a href="https://github.com/codr4life/cl4l">here</a>.

peace, out
