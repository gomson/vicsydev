# [vicsy/dev](https://github.com/codr4life/vicsydev) | generic methods
posted Jan 27th 2017, 6:01 pm

### preramble
Generic methods is a superior strategy for dealing with dynamic dispatch compared to common alternatives. Instance methods and interfaces both miss the point by not offering enough granularity and adding too much hierarchy, leading to brittle code that's painful to evolve. The approach is doable in any generically talented language with user defined types, but most commonly known from Common Lisp. 

### inheritance
At first blush, generic methods may seem to offer only minor advantages over class inheritance; but dealing with one method at a time is a lot less complex than inheriting type hierarchies with multiple methods. There is nothing wrong sub typing or dynamic dispatch as concepts; what causes all the pain is not separating concerns.

### monkey patching
A popular approach in Ruby is to inject code into already defined classes, commonly known as Monkey Patching. The reason it's popular is that this kind of leverage is very useful; and the reason it causes so much pain is the same as above, it mixes up sub typing with dynamic dispatch. One way to think of generic methods is as monkey patching done right, with controlled scope and granularity.

```lisp
(defstruct (foo))

(defstruct (bar (:include foo)))

;; FOOBAR doesn't depend on polymorphism,
(defgeneric foobar (self)
  (:method ((self foo))
    (format t "foo.foobar~%")))

;; and the decision to drop it for a specialized implementation
;; can be made locally per method.
(defmethod foobar ((self bar))
  (format t "bar.foobar~%"))

;; BARFOO uses polymorphism to invoke the inherited method,
(defgeneric barfoo (self)
  (:method ((self foo))
    (format t "foo.barfoo~%")))

;; but still offers the possibility of customizing behaviour
;; locally 
(defmethod foobar ((self bar))
  (format t "bar.barfoo~%")
  (call-next-method))
```

### multiple dispatch
Besides beating classes and interfaces at their own game, generic methods provide the ability to dispatch on multiple values. Multiple dispatch allows writing code that focuses on behaviour rather than hierarchies. This allows significant savings when implementing more complex behaviours by not requiring callers to specify any types statically.

```lisp
(defgeneric complex-behaviour (x y)
  (:method (x y)
    "Default implementation")
  (:method ((x foo) (y foo))
    "Called for FOO / FOO")
  (:method ((x foo) y)
    "Called for FOO / any type")
  (:method ((x bar) (y bar))
    "Called for BAR / BAR")
  (:method ((x bar) y)
    "Called for BAR / any type")
  (:method ((x foo) (y bar))
    "Called for FOO / BAR")
  (:method ((x bar) (y foo))
    "Called for BAR / FOO"))
```

### fixtures
Common Lisp additionally provides the ability to easily define method fixtures to be run before, after and around the main method. Implementing the same strategy manually requires making sure that fixtures are only called from the leaves of the method tree, which interacts badly with overriding methods.

```lisp
(defmethod complex-behaviour :after (x y)
  "Always called after the chosen main method for any types")
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of this idea and more <a href="https://github.com/codr4life/cl4l">here</a>.

peace, out
