# [vicsy/dev](https://github.com/codr4life/vicsydev) | rolling a virtual DOM
posted Feb 27th 2017, 10:00 am

### preramble
One of the tools that I often end up rolling myself is a virtual DOM for re-/generating HTML dynamically. One can hope that spreading the word on their utility will help take some of that weight of my shoulders in the future. A virtual DOM may sound daunting; but we're aiming for utility, not encoding entire standards to the letter. The focus of this post is laying a simple and extensible foundation, with [transaction](https://github.com/codr4life/cl4l#transactions) support thrown in as a hint of what's to come.

### virtual DOM
The main reason you may want to consider the virtual DOM approach is that it allows using the full power of your programming language to modularise, generate and update content dynamically. Templates have their uses, but abusing them to implement dynamic content is one of the worst ideas that came out of Rails. Splitting application logic in two parts/languages, one for the server and one for the client; is a massive waste of energy. And writing entire applications in a crappy language to get around the impedance mismatch takes the price for worst idea ever; it doesn't matter how many layers of tooling, JIT-compilers and static typing you pile on top. The first example of a virtual DOM that I came across was in the [Seaside](http://www.seaside.st/) framework, sometime around Y2k; and it ruined me to the point where I briefly considered writing web application servers in [Squeak](http://squeak.org/). Luckily, experience and common sense caught up with me and since then I've been rolling my own whenever not forced to use lesser tools. These days; generating content on the server is even more attractive, since it improves the experience for mobile users enough to turn performance into a feature.

### a basic example
The example below generates an HTML document with title and a link, and shows how to use [transactions](https://github.com/codr4life/cl4l#transactions) to roll back changes.

```
CL4L-HTML> (let* ((doc (html-doc :title "foobar" :dynamic? t))
                  (body (html-body doc))
                  (link (html-a body :id :my-link
                                     :href "http://www.foo.com")))
             (html link "bar")
             (with-trans ()
               (setf (html-attr link :href) "http://www.bar.com")
               (html link "baz" :replace? t)
               (rollback))
             (princ (to-html doc)))

<!DOCTYPE html>
<html>
<head id="g772"><title id="g774">foobar</title></head>
<body id="g773"><a href="http://www.bar.com" href="http://www.foo.com" id="my-link">bar</a></body>
</html>
```

### implementation
The general idea is to only implement the functionality we really need; documents, tags, attributes and content; in a simple and extensible framework; and layer higher level functionality such as change-tracking, [transactions](https://github.com/codr4life/cl4l#transactions) and callbacks on top in an opt-in fashion. The layered, extensible approach makes it possible to gradually evolve the library along the way; which simplifies the task enough to be doable; and making higher level functionality opt-in allows re-using the same code for any kind of HTML generation. Please excuse the use of ```define-fn``` and ```define-body```; the reason for their presence is to enable gradual, whole-system [optimisation](https://github.com/codr4life/cl4l/blob/master/cl4l.lisp).

```
(defstruct (html)
  root
  (tag (error "missing tag") :type string)
  (attrs nil :type list)
  (elems nil :type list))

(defstruct (html-head (:include html))
  title)

(defstruct (html-doc (:include html)
                     (:conc-name html-))
  head body
  ids dynamic?)

(define-fn escape-char (ch) ()
  "Returns escape code for CH"
  (case ch
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code ch)))))

(define-fn escape (in chars) (:speed 1)
  "Returns IN with escaped CHARS"
  (with-output-to-string (out)
    (flet ((escape? (ch) (find ch chars)))
      (let ((start 0) pos)
        (do-while ((setf pos
                         (position-if #'escape? in
                                      :start start)))
          (write-sequence in out :start start :end pos)
          (write-sequence (escape-char (char in pos)) out)
          (setf start (1+ pos)))
        
        (let ((len (length in)))
          (when (< start len)
            (write-sequence in out :start start :end pos)))))))

(define-fn escape-html (in) ()
  "Returns IN with escaped html chars"
  (escape in "<>&"))

(define-fn escape-html-attr (in) ()
  "Returns IN with escaped attr chars"
  (escape in "<>&\"'"))

(defgeneric html-id (self)
  (:documentation
   "Returns id attribute from SELF")
  
  (:method ((self html))
    (html-attr self :id))
  (:method ((self string))
    nil))

(define-fn (setf html-id) (val self) ()
  "Sets id attribute to VAL in SELF"
  (setf (html-attr self :id) val))

(define-fn add-html-id (self id elem) ()
  "Registers ELEM for ID in SELF"
  (setf (gethash id (or (html-ids self)
                        (setf (html-ids self)
                              (make-hash-table :test 'eq))))
        elem)
  id)

(define-fn add-html (self
                     &key elem id tag
                     (root (html-root self))
                     (trans *trans*)) ()
  "Adds ELEM to SELF in optional TRANS"
  (when trans
    (push (lambda ()
            (pop (html-elems self)))
          (on-rollback trans)))
  
  (unless elem
    (setf elem (make-html :tag tag :root root))
    (setf (html-id elem) (if (html-dynamic? root)
                             (or id (gensym))
                             id)))
  
  (when (html-dynamic? root)
    (add-html-id root (html-id elem) elem))
  
  (push elem (html-elems self))
  elem)

(define-fn html-clear (self &key (trans *trans*)) ()
  "Clears SELF elems in optional TRANS"
  (when trans
    (let ((elems (html-elems self)))
      (push (lambda ()
              (setf (html-elems self) elems))
            (on-rollback trans))))
  (setf (html-elems self) nil))

(define-fn html (self str
                 &key (escape? t)
                 replace? (trans *trans*)) ()
  "Adds STR to SELF;
   optionally escapes if ESCAPE?;
   optionally replaces content if REPLACE?;
   in optional TRANS"
  (when replace? (html-clear self :trans trans))
  (add-html self :elem (if escape?
                           (escape-html str)
                           str)
                 :trans trans))

(define-fn html-doc (&key title dynamic?) ()
  "Returns new html-doc with optional TITLE"
  (let* ((doc (make-html-doc :tag "html"
                             :dynamic? dynamic?))
         (head (add-html (setf (html-root doc) doc)
                         :elem (make-html-head :tag "head"
                                               :root doc))))
    (when dynamic?
      (setf (html-id head) (add-html-id doc (gensym) head)))
    (setf (html-root doc) doc
          (html-head doc) head
          (html-body doc) (add-html doc :tag "body"))
    (when title
      (html (html-title doc) title))
    doc))

(define-fn html-title (self) ()
  "Lazy-initialises and returns title for SELF"
  (let ((head (html-head self)))
    (or (html-head-title head)
        (setf (html-head-title head)
              (add-html head :tag "title")))))

(define-fn find-html-attr (self key) ()
  "Returns attr with KEY from SELF"
  (assoc key (html-attrs self) :test #'eq))

(define-fn html-attr (self key) ()
  "Returns attr value for KEY from SELF"
  (rest (find-html-attr self key)))

(define-fn del-html-attr (self key) ()
  "Removes attr with KEY from SELF"
  (remove-if (lambda (attr)
               (string= attr key))
             (html-attrs self)
             :key #'first))

(define-fn (setf html-attr) (val self key
                             &key (trans *trans*)) (:speed 1)
  "Sets attr value for KEY in SELF to VAL in optional TRANS"
  (let ((found? (find-html-attr self key)))
    (if val
        (progn
          (when trans
            (let ((prev (rest found?)))
              (push (lambda ()
                      (if found?
                          (rplacd (find-html-attr self key) prev)
                          (del-html-attr self key)))
                    (on-rollback trans))))
          (if found?
              (rplacd found? val)
              (push (cons key val) (html-attrs self))))
        (progn
          (when (and trans found?)
            (let ((prev (rest found?)))
              (push (lambda ()
                      (push (cons key prev) (html-attrs self)))
                    (on-rollback trans))))
          (when found?
            (del-html-attr self key))))))

(define-fn html-a (self &key href id) ()
  "Returns a new link with optional HREF"
  (let ((a (add-html self :tag "a" :id id)))
    (when href (setf (html-attr a :href) href))
    a))

(defgeneric write-attr-html (self out)
  (:documentation
   "Writes HTML for SELF to OUT")
  
  (:method ((self string) out)
    (write-string (escape-html-attr self) out))
  (:method (self out)
    (write-attr-html (string-downcase (str! self)) out)))

(defgeneric write-html (self out &key pretty?)
  (:documentation
   "Writes HTML for SELF to OUT, with formatting if PRETTY?")
  
  (:method ((self html) out &key (pretty? t))
    (define-body ()
      (write-char #\< out)
      (write-string (html-tag self) out)
      
      (dolist (attr (html-attrs self))
        (write-char #\space out)
        (write-string (string-downcase (str! (first attr))) out)
        (write-string "=\"" out)
        (write-attr-html (rest attr) out)
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
          (write-string "</" out)
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
  "Returns HTML for SELF, with formatting if PRETTY?"
  (with-output-to-string (out)
    (write-html self out :pretty? pretty?)))
```

### performance
The basic example above manages 200k repetitions per second on my machine; most of that time is spent generating the document, which may then be reused for callbacks and updates.

```
CL4L-HTML> (cl4l-test:run-suite '(:cl4l :html) :reps 200000)
(cl4l html)                   1.044
TOTAL                         1.044
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/cl4l).

peace, out
