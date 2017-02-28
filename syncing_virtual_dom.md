# [vicsy/dev](https://github.com/codr4life/vicsydev) | syncing the virtual DOM
posted Feb 28th 2017, 4:00 pm

### preramble
The virtual DOM implementation described in a previous [post](https://github.com/codr4life/vicsydev/blob/master/virtual_dom.md) has several use cases where content is relatively static once generated but the process still gains from a structured approach to generating it. And since nodes in the DOM are addressable; may have their content updated, and their HTML re-generated; all the pieces needed for dynamic updates are already in place. Separating concerns this way allows user code to decide how much control it wants to trade for convenience.
 
### use
Documents track updates when ```dynamic?``` is true. Each element keeps track of it's own ```update?``` status, which is set to true once the element has it's HTML generated. ```html-update-script``` returns a script containing the jQuery incantations needed to update the browser with all changes since last call. 

```
CL4L-HTML> (let* ((doc (html-doc :dynamic? t))
                  (body (html-body doc)))
             (html-script doc :id :jquery-js :src "scripts/jquery.js")
             (princ (to-html doc))

             (let ((hdr (html-h body 1 :id :hdr :body "foo")))
               (setf (html-style hdr :text-decoration) "underline")
               (html-a hdr :id :lnk
                           :href "http://www.foo.com"
                           :body "click me"))
    
             (princ (html-update-script doc)))

<!DOCTYPE html>
<html>
<head id="head"><script id="jquery-js" src="scripts/jquery.js"></script></head>
<body id="body"/>
</html>$('#body').append('<h1 id="hdr">foo</h1>');

$('#hdr').attr('style', 'text-decoration: underline');
$('#hdr').append('<a href="http://www.foo.com" id="lnk">click me</a>');
```
 
### implementation
We're going to side-step the complexity of diffing content by simply logging updates when the virtual DOM is modified; this also fits better with the stated goal of supporting both static, dynamic and mixed HTML-generating scenarios. Included below are the parts of the implementation that deal with updates in any way; the actual update is performed by calling ```(update-html root "...")```. Please excuse the use of ```define-fn``` and ```define-body```; the reason for their presence is to enable gradual, whole-system [optimisation](https://github.com/codr4life/cl4l/blob/master/cl4l.lisp).

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
                     "$(~a).append($(~a));"
                     (html-js-id self) (html-js-id elem))
        (progn
          (update-html root
                       "$(~a).append('~a');"
                       (html-js-id self) (to-html elem))
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
                 "$(~a).empty();"
                 (html-js-id self)))
  
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
                         "$(~a).attr('~a', ~a);"
                         (html-js-id self)
                         (string-downcase (str! key))
                         (encode-html-attr-js key val)))

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
                         "$(~a).removeAttr('~a');"
                         (html-js-id self)
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
        (let ((key (first attr)))
          (write-string (string-downcase (str! key)) out)
          (write-string "=\"" out)
          (write-string (encode-html-attr key (rest attr))
                        out))
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
