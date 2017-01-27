# [vicsy/dev](https://github.com/codr4life/vicsydev) | lispy tests
posted Jan 27th, 2017 06:07 am

### preramble
I prefer my tests simple and flexible, designed around the application rather than imposed on it. These days, I usually start with basic functions and assertions; and add what I need when I need it. The holy grail is a reusable library that allows the same progressive approach without getting in the way, while helping out with more elaborate tasks like test discovery, grouping and benchmarking. This post describes the humble beginnings of that library in Common Lisp.

### tags
Each test carries a unique set of tags in it's definition. When running tests, a set of tags may be specified to only trigger tests matching all specified tags. An optional set of tags to skip may also be specified; tests matching any specified tags are skipped, which allows excluding non relevant tests within the triggered set. The approach relies on creativity with a touch of discipline when specifying tags; but the process is gradual and non-intrusive, Just add whatever tags needed to accomplish what needs to be done and to take advantage of any patterns that emerge. 

```lisp
(define-test (:foo :bar :baz)
  (assert nil))

(cl4l-test:run-suite '(:foo :bar :baz))

(cl4l-test:run-suite '(:foo :bar) :skip '(:baz))
```

### benchmarks
Premature optimisation is the root of many problems, but so is not having any idea about the performance of your code. Unfortunately, I've found that most test frameworks don't even bother with benchmarks, and the ones that do require too much ceremony. There are no rational reasons for separating benchmarks and tests, the framework described here comes with the ability to run any set of tests as a benchmark with specified number of warmups and repetitions.

```lisp
> (cl4l-test:run-suite nil)
(index clone)                   0.0
(index diff)                    0.0
(index join)                    0.0
(index match)                   0.0
(index merge)                   0.0
(index multi)                   0.0
(index perf)                    0.0
(index perf built-in)         0.012
(index perf table)              0.0
(index rec)                     0.0
(index stream)                  0.0
(index trans)                   0.0
TOTAL 0.012
```

```lisp
> (cl4l-test:run-suite '(:index :perf) :warmup 10 :reps 100)
(index perf)                  0.072
(index perf built-in)           1.0
(index perf table)             0.08
TOTAL 1.152
```

### fixtures
I prefer my fixtures to wrap around tests, to allow using block statements for allocating and releasing resources. Like tests, fixtures are uniquely identified by a set of tags. When running tests, fixtures matching skipped tags will not run.

```lisp
(define-fixture (:foo)
  (let ((foo ...))
    (call-next-fixture)))
```

### implementation
Included below is the complete implementation of all functionality described here, it's trivial enough to clone and own if you feel like tweaking it. Besides a few custom [utilities](https://github.com/codr4life/cl4l/blob/master/utils.lisp), it uses [unique indexes](https://github.com/codr4life/cl4l#indexes) for mapping tags to tests and fixtures in order; the same thing could be accomplished with (gensym), (intern), built-in sets and manual sorting if you prefer more code in your code.

```lisp
(defpackage cl4l-test
  (:export call-next-fixture define-fixture
           define-test fixture run-fixtures run-suite
           run-test run-tests test)
  (:shadowing-import-from cl4l-utils string! with-symbols)
  (:use cl cl4l-index))

(in-package cl4l-test)

(defstruct (suite)
  (fixtures (index nil))
  (tests (index nil)))

(defparameter *suite* (make-suite))

(defmacro define-fixture ((&rest tags) &body body)
  "Defines fixture with TAGS around BODY"
  (with-symbols (_next _skip _tags)
    `(let ((,_tags (list ,@tags)))
       (fixture ,_tags
                (lambda (,_skip ,_next)
                  (macrolet ((call-next-fixture ()
                               "Runs next fixture"
                               `(funcall ,',_next)))
                    (if (intersection ,_skip ,_tags)
                        (call-next-fixture)
                        (progn ,@body))))))))

(defmacro define-test ((&rest tags) &body body)
  "Defines test with TAGS around BODY"
  `(test (list ,@tags) (lambda () ,@body)))

(defun fixture (tags fn &key (suite *suite*))
  "Replaces fixture for TAGS with FN in SUITE"
  (index-remove (suite-fixtures suite) tags)
  (index-add (suite-fixtures suite) fn :key tags))

(defun test (tags fn &key (suite *suite*))
  "Replaces test for TAGS with FN in SUITE"
  (index-remove (suite-tests suite) tags)
  (index-add (suite-tests suite) fn :key tags))

(defun run-fixtures (&key (last (lambda ()))
                          skip
                          (suite *suite*))
  "Runs fixtures not matching SKIP in SUITE around LAST"
  (labels ((run-fx (next)
             (if next
                 (funcall (rest (first next)) skip
                          (lambda () (run-fx (rest next))))
                 (funcall last))))
    (funcall #'run-fx (index-first (suite-fixtures suite)))))

(defun run-suite (run &key (warmup 0) (reps 1)
                            skip
                           (suite *suite*))
  "Runs fixtures followed by tests matching RUN in SUITE x REPS 
   with WARMUP, skipping fixtures and tests in SKIP"
  (run-fixtures :last (lambda ()
                        (run-tests run :warmup warmup
                                       :reps reps
                                       :skip skip
                                       :suite suite))
                :skip skip
                :suite suite))

(defun run-test (fn &key (warmup 0) (reps 1))
  "Runs test FN x REPS with WARMUP and prints elapsed time"
  (dotimes (_ warmup) (funcall fn))
  (let ((time (get-internal-run-time)))
    (dotimes (_ reps) (funcall fn))
    (setf time (- (get-internal-run-time) time))
    (format t "~5f~%"
            (/ time internal-time-units-per-second))
    time))

(defun run-tests (run &key (warmup 0) (reps 1)
                           skip
                           (suite *suite*))
  "Runs tests matching RUN in SUITE x REPS with WARMUP,
   skipping tests in SKIP; and prints a table of tags
   and elapsed times"
  (tagbody
   retry-suite
     (let ((tot-time 0))
       (dolist (test (index-first (suite-tests suite)))
         (let ((tags (first test))
               (test-fn (rest test)))
           (when (and (null (set-difference run tags))
                      (null (intersection skip tags)))
             (tagbody
              retry-test
                (restart-case
                    (progn
                      (format t "~30a"
                              (string-downcase (string! tags)))
                      (incf tot-time
                            (run-test test-fn
                                      :warmup warmup
                                      :reps reps)))
                  (skip-test ()
                    (format t " SKIP~%"))
                  (retry-test ()
                    (format t " TEST~%")
                    (go retry-test))
                  (retry-suite ()
                    (format t "SUITE~%")
                    (go retry-suite)))))))
       (format t "~30a~5f~%"
               "TOTAL"
               (/ tot-time
                  internal-time-units-per-second)))))
```

### postramble
I've been playing around with this approach for 20 years now, and have yet to find a language where it isn't doable enough to improve on the status quo. The excuses usually offered for enforcing more ceremonial test procedures are mostly about CI; and CI in itself is mostly process fluff to cover up for lacking culture.
    
### peace, out
You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of this framework and more <a href="https://github.com/codr4life/cl4l">here</a>.
