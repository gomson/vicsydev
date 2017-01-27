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

(defmethod complex-behaviour :after (x y)
  "Always called after the chosen main method for any types")
