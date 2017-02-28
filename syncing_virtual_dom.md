# [vicsy/dev](https://github.com/codr4life/vicsydev) | syncing a virtual DOM
posted Feb 28th 2017, 4:00 am

### preramble
A [virtual DOM](https://github.com/codr4life/vicsydev/blob/master/virtual_dom.md) in itself has several use cases where content is relatively static once generated but the process still gains from a structured approach to generating it. And since nodes in the DOM are addressable; may have their content updated, and their HTML re-generated; all the pieces are already in place. Separating concerns this way allows user code to decide how much control it wants to trade for convenience.
 
### use
Documents track updates when ```dynamic?``` is true. Each element keeps track of it's own ```update?``` status, which is set to true once the element has it's HTML generated. ```html-update-script``` returns a script containing the jQuery incantations needed to update the browser with all changes since last call. 

```
CL4L-HTML> (let* ((doc (html-doc :dynamic? t))
                  (body (html-body doc))
                  (hdr (html-h body 1 :id :hdr :body "foo")))
             (setf (html-update? hdr) t)
             (html-empty hdr)
             (setf (html-id hdr) :foo-hdr)
             (html-a hdr :id :lnk
                         :href "http://www.foo.com"
                         :body "click me")
             (princ (html-update-script doc)))

$('#hdr').empty();
$('#hdr').attr('id', 'foo-hdr');
$('#foo-hdr').append('<a href="http://www.foo.com" id="lnk">click me</a>');
```
 
### implementation
Logging updates when the virtual DOM is modified may not be as impressive as diffing and working backwards; but from most other aspects it is superior. Having access to both aspects of HTML generation at the same time, and the option of going back and forth at any granularity; while still retaining the possibility for full automation; is clearly the more powerful approach.

```
(defstruct (html)
  root update?
  (tag (error "missing tag") :type string)
  (attrs nil :type list)
  (elems nil :type list))

(defstruct (html-doc (:include html)
                     (:conc-name html-))
  head body
  ids dynamic? updates)

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
    (when id (setf (html-id elem) id)))
  
  (when (and (html-p elem)
             (or id (html-dynamic? root))
             (null (html-id elem)))
    (setf (html-id elem) (or id (make-html-id))))
  
  (when (html-update? self)
    (if (and (html-p elem) (html-update? elem))
        (update-html root
                     "$('#~a').append($('#~a'));"
                     (html-sid self) (html-sid elem))
        (progn
          (update-html root
                       "$('#~a').append('~a');"
                       (html-sid self) (to-html elem))
          (when (html-p elem)
            (setf (html-update? elem) t)))))

  (push elem (html-elems self))
  elem)

(define-fn html-empty (self &key (trans *trans*)) ()
  "Clears SELF elems in optional TRANS"
  (when trans
    (let ((elems (html-elems self)))
      (push (lambda ()
              (setf (html-elems self) elems))
            (on-rollback trans))))
  
  (when (html-update? self)
    (update-html (html-root self)
                 "$('#~a').empty();"
                 (html-sid self)))
  
  (setf (html-elems self) nil))

(define-fn (setf html-attr) (val self key
                             &key (trans *trans*)) (:speed 1)
  "Sets attr value for KEY in SELF to VAL in optional TRANS"
  (let ((root (html-root self))
        (found? (find-html-attr self key)))
    (if val
        (progn
          (when trans
            (let ((prev (rest found?)))
              (push (lambda ()
                      (if found?
                          (rplacd (find-html-attr self key) prev)
                          (del-attr self key)))
                    (on-rollback trans))))

          (when (and (html-update? self) (html-dynamic? root))
            (update-html root
                         "$('#~a').attr('~a', '~a');"
                         (html-sid self)
                         (string-downcase (str! key))
                         (with-output-to-string (out)
                           (write-html-attr val out))))

          (if found?
              (rplacd found? val)
              (push (cons key val) (html-attrs self))))
        (progn
          (when (and trans found?)
            (let ((prev (rest found?)))
              (push (lambda ()
                      (push (cons key prev) (html-attrs self)))
                    (on-rollback trans))))

          (when (and (html-update? self) (html-dynamic? root))
            (update-html root
                         "$('#~a').removeAttr('~a');"
                         (html-sid self)
                         (string-downcase (str! key))))

          (when found?
            (del-attr self key))))))

(define-fn update-html (self script &rest args) ()
  "Pushes script as update in SELF, formatted when ARGS"
  (push (if args
            (apply #'format nil script args)
            script)
        (html-updates (html-root self))))

(defgeneric write-html (self out &key pretty?)
  (:documentation
   "Writes HTML for SELF to OUT, with formatting if PRETTY?")
  
  (:method ((self html) out &key (pretty? t))
    (define-body ()
      (write-char #\< out)
      (write-string (html-tag self) out)
      
      (dolist (attr (sort (html-attrs self)
                          #'string<
                          :key (lambda (x)
                                 (symbol-name (first x)))))
        (write-char #\space out)
        (write-string (string-downcase (str! (first attr))) out)
        (write-string "=\"" out)
        (write-html-attr (rest attr) out)
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
          (write-char #\> out)))

      (when (html-dynamic? (html-root self))
        (setf (html-update? self) t))))

  (:method ((self html-doc) out &key (pretty? t))
    (write-string "<!DOCTYPE html>" out)
    (when pretty? (terpri out))
    (call-next-method))
  
  (:method ((self string) out &key pretty?)
    (declare (ignore pretty?))
    (write-string self out)))

(define-fn write-html-updates (self out &key (pretty? t)) ()
  "Writes update script for SELF to OUT"
  (dolist (it (nreverse (html-updates self)))
    (write-string it out)
    (when pretty? (terpri out)))
  (setf (html-updates self) nil))

(define-fn html-update-script (self &key (pretty? t)) ()
  "Returns update script for SELF"
  (with-output-to-string (out)
    (write-html-updates self out :pretty? pretty?)))
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/cl4l).

peace, out
