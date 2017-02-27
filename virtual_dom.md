# [vicsy/dev](https://github.com/codr4life/vicsydev) | rolling a virtual DOM
posted Feb 27th 2017, 03:00 am

### preramble
One of the tools that I often end up rolling myself is a virtual DOM for re-/generating HTML dynamically. One can hope that spreading the word on their utility will help take some of that weight of my shoulders in the future. A virtual DOM may sound daunting; but we're aiming for utility, not encoding entire standards to the letter. The focus of this post is laying a simple and extensible foundation, with [transaction](https://github.com/codr4life/cl4l#transactions) support thrown in as a hint of what's to come.

### virtual DOM
The main reason you may want to consider the virtual DOM approach is that it allows using the full power of your programming language to modularise, generate and update content dynamically. Templates have their uses, but abusing them to implement dynamic content is one of the worst ideas that came out of Rails. Splitting application logic in two parts/languages, one for the server and one for the client; is a massive waste of energy. And writing the entire server in a crappy language to get around the impedance mismatch takes the price for worst idea ever; it doesn't matter how many layers of tooling, JIT-compilers and static typing you pile on top. The first example of a virtual DOM that I came across was in the [Seaside](http://www.seaside.st/) framework, sometime around Y2k; and it ruined me to the point where I briefly considered writing entire application servers in [Squeak](http://squeak.org/). Luckily, experience and common sense caught up with me and since then I've been rolling my own whenever not forced to use lesser tools. These days; generating content on the server is even more attractive, since it improves the experience for mobile users to the point where performance becomes a feature.

### a basic example
The example below generates an HTML document with title and a link, and shows how to use [transactions](https://github.com/codr4life/cl4l#transactions) to roll back changes.

```
CL4L-HTML> (let* ((doc (html-doc :title "foobar"))
                  (body (html-body doc))
                  (link (html-a body :href "http://www.foo.com")))
             (html-text link "bar")
             (with-trans ()
               (setf (html-attr link "href") "http://www.bar.com")
               (html-text link "baz" :replace? t)
               (rollback))
             (princ (to-html doc)))

<!DOCTYPE html>
<html>
<head><title>foobar</title></head>
<body><a href="http://www.foo.com">bar</a></body>
</html>
```

### implementation
The general idea is to only implement the functionality we really need; documents, tags, attributes and content; in a simple and extensible framework; and layer higher level functionality such as change-tracking, [transactions](https://github.com/codr4life/cl4l#transactions) and callbacks on top in an opt-in fashion. The layered, extensible approach makes it possible to gradually evolve the library along the way; which simplifies the task enough to be doable; and making higher level functionality opt-in allows re-using the same code for any kind of HTML generation. Please excuse the use ```define-fn``` and ```define-body```; the reason for their presence is to simplify gradual, whole-system [optimisation](https://github.com/codr4life/cl4l/blob/master/cl4l.lisp).

```
(defstruct (html)
  (tag (error "missing tag") :type string)
  (attrs nil :type list)
  (elems nil :type list))

(defstruct (html-head (:include html))
  title)

(defstruct (html-doc (:include html)
                     (:conc-name html-))
  head body)

(define-fn add-html (self
                     &key tag
                     (elem (make-html :tag tag))
                     (trans *trans*)) ()
  (when trans
    (push (lambda ()
            (pop (html-elems self)))
          (on-rollback trans)))
  
  (push elem (html-elems self))
  elem)

(define-fn html-clear (self &key (trans *trans*)) ()
  (when trans
    (let ((elems (html-elems self)))
      (push (lambda ()
              (setf (html-elems self) elems))
            (on-rollback trans))))
  (setf (html-elems self) nil))

(define-fn html-text (self text &key replace? (trans *trans*)) ()
  (when replace? (html-clear self :trans trans))
  (add-html self :elem text :trans trans))

(define-fn html (tag) ()
  (make-html :tag tag))

(define-fn html-doc (&key title) ()
  (let ((doc (make-html-doc :tag "html")))
    (setf (html-head doc)
          (add-html doc :elem (make-html-head :tag "head"))
          (html-body doc)
          (add-html doc :elem (html "body")))
    (when title
      (html-text (html-title doc) title))
    doc))

(define-fn html-title (self) ()
  (let ((head (html-head self)))
    (or (html-head-title head)
        (setf (html-head-title head)
              (add-html head :tag "title")))))

(define-fn html-find-attr (self key) ()
  (assoc key (html-attrs self) :test #'string=))

(define-fn html-attr (self key) ()
  (rest (html-find-attr self key)))

(define-fn (setf html-attr) (val self key &key (trans *trans*)) ()
  (let ((found? (html-find-attr self key)))
    (when trans
      (let ((prev (rest found?)))
        (push (lambda ()
                (if found?
                    (rplacd (html-find-attr self key) prev)
                    (remove-if (lambda (attr)
                                 (string= (first attr) key))
                               (html-attrs self))))
              (on-rollback trans))))
    
    (if found?
        (rplacd found? val)
        (push (cons key val) (html-attrs self)))))

(define-fn html-a (self &key href) ()
  (let ((a (add-html self :elem (html "a"))))
    (when href (setf (html-attr a "href") href))
    a))

(defgeneric write-html (self out &key)
  (:method ((self html) out &key (pretty? t))
    (define-body ()
      (write-char #\< out)
      (write-string (html-tag self) out)
      
      (dolist (attr (html-attrs self))
        (write-char #\space out)
        (write-string (first attr) out)
        (write-char #\= out)
        (write-char #\" out)
        (princ (rest attr) out)
        (write-char #\" out))
      
      (let ((elems (html-elems self)))
        (unless elems (write-char #\/ out))
        (write-char #\> out)
        (when elems
          (let ((br? (and pretty? (> (length elems) 1))))
            (when br? (terpri out))
            (dolist (elem (reverse elems))
              (write-html elem out :pretty? pretty?)
              (when br? (terpri out))))
          (write-char #\< out)
          (write-char #\/ out)
          (write-string (html-tag self) out)
          (write-char #\> out)))))

  (:method ((self html-doc) out &key (pretty? t))
    (write-string "<!DOCTYPE html>" out)
    (when pretty? (terpri out))
    (call-next-method))
  
  (:method ((self string) out &key pretty?)
    (declare (ignore pretty?))
    (write-string self out)))

(define-fn to-html (self &key (pretty? t)) ()
  (with-output-to-string (out)
    (write-html self out :pretty? pretty?)))
```

### performance
The basic example above manages 200k repetitions per second on my machine. Most of that time is spent generating the document, which may then be reused for callbacks and updates.

```
CL4L-HTML> (cl4l-test:run-suite '(:cl4l :html) :reps 200000)
(cl4l html)                   1.044
TOTAL                         1.044
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/cl4l).

peace, out
