# [vicsy/dev](https://github.com/codr4life/vicsydev) | Transactional memory in Forth, in Lisp
posted Feb 21st 2017, 10:00 pm

### preramble
Transactional memory is most often mentioned in the context of multi-threaded programming, where the reason the system tracks memory access is to prevent corruption. Another kind of transactional memory is the database, SQL or otherwise; one of the main reasons they exist is to provide transactions over data. Transactions are useful in many contexts where not commonly reached for; [transactional collections](https://github.com/codr4life/cl4l#indexes) offer a nice compromise between primitive collections and databases, and the world would be a better place if more undo facilities tracked change sets rather than individual changes.

### the case for embedded languages
Implementing general purpose transactional memory from the inside is mostly impossible, there's just no way to hook into the system deep enough to capture all possibilities without ending up replacing every single part of the language that is covered. One of the motives for embedding a higher level language like [Lifoo](https://github.com/codr4life/lifoo) into a general purpose language like Common Lisp is the added leverage you get from mastering your environment.

### transactions
[Lifoo](https://github.com/codr4life/lifoo) provides memory transactions that track updates to the stack, any place that can be [set](https://github.com/codr4life/vicsydev/blob/master/consing_forth.md#setf) or [deleted](https://github.com/codr4life/vicsydev/blob/master/consing_forth.md#del); and the word dictionary. Transactions may be committed and rolled back several times during their lives, and are reset each time.

```
;; Roll back changes to stack

Lifoo> 1 2 (3 4 rollback) trans stack

(2 1)

;; Try commit before rollback

Lifoo> 1 2 (3 4 commit 5 6 rollback) trans stack

(4 3 2 1)

;; Roll back delete from hash table;
;; hash tables take an optional source as parameter;
;; GET marks the spot and DEL does the job

Lifoo>  ((1 . :foo) (2 . :bar)) hash
        (1 get del rollback) trans
        list nil sort

((1 . :FOO) (2 . :BAR))

;; Define new struct with two fields,
;; BAR defaults to -1.

Lifoo> ((bar -1) baz) :foo struct

NIL

;; Rollback struct update;
;; constructors take an optional assoc list as source

Lifoo> (:bar 41) make-foo
       (foo-bar 42 set drop commit
        foo-bar 43 set rollback) trans
       foo-bar

42

;; Roll back word (re-)definition,
;; uses DROP to skip arguments since it already made up it's mind

Lifoo> drop drop 42 $ 
       (number number) :+ define 
       rollback $ 
       trans
       1 2 +

3
```

### implementation
Since [Lifoo](https://github.com/codr4life/lifoo) uses separate instances and channels for multi-threaded programming, and has no need for protection against memory corruption; it simply logs closures for rolling back changes in reverse order.

```
(defstruct (lifoo-cell (:conc-name cell-))
  val set del)

(defun lifoo-push-cell (it &key (exec *lifoo*))
  "Pushes CELL onto EXEC stack"  
  (vector-push-extend it (stack exec))
  (when-let (trans (lifoo-trans :exec exec))
    (push (lambda ()
            (lifoo-pop-cell :exec exec))
          (on-rollback trans)))
  it)

(defun lifoo-pop-cell (&key (exec *lifoo*))
  "Pops cell from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (let ((it (vector-pop (stack exec))))
      (when-let (trans (lifoo-trans :exec exec))
        (push (lambda ()
                (lifoo-push-cell it :exec exec))
              (on-rollback trans)))
      it)))

(defun (setf lifoo-val) (val cell &key (exec *lifoo*))
  (when-let (trans (lifoo-trans :exec exec))
    (let ((prev (cell-val cell)))
      (push (lambda ()
              (setf (lifoo-val cell :exec exec) prev))
            (on-rollback trans))))
  (when-let (set (cell-set cell))
    (funcall set val))
  (setf (cell-val cell) val))

(defun lifoo-del (self &key (exec *lifoo*))
  (let* ((val (lifoo-val self))
         (del (or (cell-del self)
                  (error "missing del: ~a" val))))
    (declare (type function del))
    (when-let (trans (lifoo-trans :exec exec))
      (push (lambda ()
              (setf (lifoo-val self :exec exec) val))
            (on-rollback trans)))
    (funcall del)))
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out

