# [vicsy/dev](https://github.com/codr4life/vicsydev) | implementing defer
posted Jan 27th 2017, 4:20 pm

### preramble
Golang's designers had good reasons to choose defer() for executing code on scope exit. The more common strategies based on code blocks (exceptions etc.) turn into suffering as soon as allocation and/or clean up depend on protected code. Implementing a comparable mechanism in other languages is perfectly doable, this post describes and compares implementations in C and Common Lisp that adds the possibility of deferring to named/outer scopes.

### example
C
```c
bool called = false;
    
{
  C4WITH_DEFER(basic) { called = true; };
  assert(!called);
}
    
assert(called);
called = false;
    
C4WITH_DEFER(outer) {
  C4WITH_DEFER(inner) {
    C4DO_DEFER(outer, { called = true; });
  }
      
  assert(!called);
}
    
assert(called);
```

Lisp
```lisp
(let ((called? nil))
  (with-defer nil
    (do-defer (setf called? t))
    (assert (null called?)))
  (assert called?))
  
(let ((called? nil))
  (with-defer outer
    (with-defer inner
      (do-defer-outer (setf called? t)))
    (assert (null called?)))
  (assert called?))
```

### implementation

The C version depends on [dynamic arrays](https://github.com/codr4life/libc4life#dynamic-arrays) to keep track off actions and cleanup attributes for triggering, and uses a couple of [custom macros](https://github.com/codr4life/libc4life/blob/master/src/c4life/utils.h) for symbol generation.

```c
#define _C4WITH_DEFER(label, _dyna, _free)	              \
  struct c4dyna _dyna;					                  \
  void _free() {					                      \
    C4DO_DYNA(&_dyna, _fn) { (*(c4defer_fnt **)_fn)(); }  \
    c4dyna_free(&_dyna);				                  \
  }							                              \
							                              \
  for (struct c4dyna *label				                  \
	 __attribute__((cleanup(_free))) =		              \
	   c4dyna_init(&_dyna, sizeof(c4defer_fnt *));        \
       label;						                      \
       label = NULL)					                  \
    
#define C4WITH_DEFER(label)				            \
  _C4WITH_DEFER(label, C4GSYM(dyna), C4GSYM(free))  \

#define C4DO_DEFER(label, code)					            \
  *((c4defer_fnt **)c4dyna_push(label)) = C4FN(code, void)	\

typedef void (c4defer_fnt)();
```

While the Lisp version is shorter and more independent thanks to more powerful macros and standard library; it get's away with only using a [custom macro](https://github.com/codr4life/cl4l/blob/master/utils.lisp) for symbol generation.

```lisp
(defmacro with-defer (name &body body)
  (let* ((_name (or name (gensym)))
         (_macro-name (symbol! 'do-defer- _name)))
    `(macrolet ((,_macro-name (&body forms)
                  (let ((n ',_name))
                    `(push (lambda () ,@forms) ,n)))
                (do-defer (&body forms)
                  `(,',_macro-name ,@forms)))
       (let ((,_name))
         (unwind-protect (progn,@body)
           (dolist (fn ,_name)
             (funcall fn)))))))
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of this idea and more <a href="https://github.com/codr4life/cl4l">here</a>.

peace, out
