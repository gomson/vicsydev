# [vicsy/dev](https://github.com/codr4life/vicsydev) | implementing defer()
posted Jan 27th 2017, 2:03 pm

### preramble
Golang's designers had good reasons to choose 'defer' as the one true mechanism for executing code on scope exit. The strategies for scoping resources (WITH-macros, exceptions and others) break down as soon as allocation and/or clean up depend on protected code. Implementing a comparable mechanism in other languages is perfectly doable, this post describes and compares implementations in C and Common Lisp that add the capability to defer to named/outer scopes.

### example

```c
bool called = false;
    
{
  C4DEFER({ called = true; });
  assert(!called);
}
    
assert(called);

called = false;
    
C4DEFER_SCOPE(outer) {
  C4DEFER_SCOPE(inner) {
    C4DEFER_TO(outer, { called = true; });
  }
      
  assert(!called);
}
    
assert(called);
```

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
#define _C4DEFER(code, _def)					              \
  void _def() code;					                          \
  bool C4SYMS(_def, _trigger) __attribute__((cleanup(_def)))  \

#define C4DEFER(code)		   \
  _C4DEFER(code, C4GSYM(def))  \

#define _C4DEFER_SCOPE(label, _dyna, _free)	              \
  struct c4dyna _dyna;					                  \
  void _free() {					                      \
    C4DO_DYNA(&_dyna, _fn) { (*(c4defer_fnt **)_fn)(); }  \
    c4dyna_free(&_dyna);				                  \
  }							                              \
							                              \
  for (struct c4dyna *label				                  \
	 __attribute__((cleanup(_free))) =		              \
	 c4dyna_init(&_dyna, sizeof(c4defer_fnt *));	      \
       label;						                      \
       label = NULL)					                  \
    
#define C4DEFER_SCOPE(label)				         \
  _C4DEFER_SCOPE(label, C4GSYM(dyna), C4GSYM(free))	 \

#define C4DEFER_TO(label, code)					            \
  *((c4defer_fnt **)c4dyna_push(label)) = C4FN(code, void)	\

typedef void (c4defer_fnt)();
```

While the lisp version is significantly shorter and clearer thanks to more powerful macros and standard library, it get's away with only using a [custom macro](https://github.com/codr4life/cl4l/blob/master/utils.lisp) for symbol generation.

```lisp
(defmacro with-defer (name &body body)
  (let ((_name (or name (gensym))))
    `(macrolet ((,(symbol! 'do-defer- _name) (&body forms)
                  (let ((n ',_name))
                    `(push (lambda () ,@forms) ,n)))
                (do-defer (&body forms)
                  `(,(symbol! 'defer- ',_name) ,@forms)))
       (let ((,_name))
         (unwind-protect (progn,@body)
           (dolist (fn ,_name)
             (funcall fn)))))))
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of this idea and more <a href="https://github.com/codr4life/cl4l">here</a>.

peace, out
