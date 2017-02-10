# [vicsy/dev](https://github.com/codr4life/vicsydev) | Transactional memory in Lispy Forth

posted Feb 9th 2017, 10:00 pm

### preramble
Transactional memory is most often mentioned in the context of multi-threaded programming, where the reason the system tracks memory access is to prevent corruption. Another kind of transactional memory is the database, SQL or otherwise; one of the main reasons they exist is to provide transactions over data. Transactions are useful in many contexts where they are not commonly used, [transactional collections](https://github.com/codr4life/cl4l#indexes) offer a nice compromise between primitive collections and databases.

### transactional memory in Common Lisp
Implementing general purpose transactional memory as a library in Common Lisp is impossible, there's just no way to hook into the system deep enough to capture all possibilities; you would end up replacing every single part of the language. The story is the same in any general purpose language I've seen so far. One of the ideas with embedding more restricted languages like [Lifoo](https://github.com/codr4life/lifoo) into more powerful host languages like Common Lisp is the increased leverage you get from controlling the box from the outside, especially if the embedded language is designed for embedding.

### trans
Lifoo provides memory transactions that track updates to the stack, any place that can be set or deleted; and the word dictionary; which is pretty much everything as far as [Lifoo](https://github.com/codr4life/lifoo) is concerned. Since [Lifoo](https://github.com/codr4life/lifoo) uses separate runtime instances and channels for multi-threaded programming, and has no need for protecting against memory corruption; it simply logs lambdas to be run in the case of a rollback. Transactions may be committed and rolled back several times during their lives, and are reset each time.

```
;; Roll back changes to stack
Lifoo> 1 2 (3 4 rollback)@ trans stack

(2 1)

;; Add commit before rollback
Lifoo> 1 2 (3 4 commit 5 6 rollback)@ trans stack

(4 3 2 1)

;; Roll back delete from hash table
Lifoo>  ((1 . :foo) (2 . :bar)) hash
    (1 get del rollback)@ trans
    list nil sort

((1 . :FOO) (2 . :BAR))

;; Roll back word definition
Lifoo> ((drop drop 42)@ (number number) :+ define rollback)@ trans
       1 2 +

3


(define-lisp-word :set (t) ()
  (let* ((val (lifoo-pop))
         (cell (lifoo-peek-cell))
         (set (lifoo-set cell)))
    (unless set
      (error "missing set: ~a" val))
    (when-let (trans (lifoo-trans :exec exec))
      (let ((prev (lifoo-val cell)))
        (push (lambda ()
                (funcall set prev)
                (setf (lifoo-val cell) val))
              (on-rollback trans))))
    (funcall set val)
    (setf (lifoo-val cell) val)))
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life).

peace, out
