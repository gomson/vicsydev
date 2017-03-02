# [vicsy/dev](https://github.com/codr4life/vicsydev) | wrapping up the virtual DOM
posted March 1st 2017, 11:00 pm

### preramble
It's time to take the virtual DOM implementation [described](https://github.com/codr4life/vicsydev/blob/master/virtual_dom.md) in [previous](https://github.com/codr4life/vicsydev/blob/master/syncing_virtual_dom.md) [posts](https://github.com/codr4life/vicsydev/blob/master/calling_virtual_dom.md) for a first spin. We will use Hunchentoot to implement the HTTP end points.

### callbacks
The first thing we need to do is set up an end point for DOM callbacks from the browser. Any URL will do, as long as the same string is passed as ```:call-url``` when creating DOM documents. ```html-call``` expects parameter names to be keywords, which is the reason for mapping.

```
(defvar *docs* (make-hash-table :test 'equal))

(hunchentoot:define-easy-handler (cl4l :uri "/cl4l") ()
  (let* ((doc-id (hunchentoot:parameter "cl4l-doc"))
         (doc (gethash doc-id *docs*)))

    (html-call doc
               (mapcar (lambda (arg)
                         (cons (keyword! (first arg)) (rest arg)))
                       (hunchentoot:post-parameters*)))

    (setf (hunchentoot:content-type*) "application/javascript")
    (html-update-script doc)))
```

### document
With callbacks in place, it's time to set up an end point for the document. Including ```jquery``` and ```cl4l.js``` is mandatory for dynamic documents, but how that happens is up to user code. The document contains an input and a button, and the button is wired up to append a greeting when clicked.

```
(hunchentoot:define-easy-handler (demo :uri "/demo") ()
  (let* ((doc (html-doc :dynamic? t :call-url "cl4l"))
         (body (html-body doc))
         (input (html-input body :type :text)))

    (html-script doc :src "jquery.js")
    (html-script doc :src "cl4l.js")

    (html-button body
                 :body "Greet"
                 :onclick
                 (lambda ()
                   (html body (format nil "Hello ~a!"
                                      (html-attr input :value)))
                   (html-br body)))
    
    (html-br body)
    
    (setf (gethash (html-doc-id doc) *docs*) doc)
    (setf (hunchentoot:content-type*) "text/html")
    (to-html doc)))
```

### demo
The repository contains a full [demo](https://github.com/codr4life/cl4l/blob/master/html-demo.lisp) and a prepared [www-catalog](https://github.com/codr4life/cl4l/tree/master/www). Once everything is in place, evaluating ```(start-html-demo)``` and pointing a web browser at ```127.0.0.1:8080/demo``` should get the show started.

```
(defparameter *port* 8080)
(defparameter *root* "~/cl4l/www/")

(defvar *server*)

(defun start-html-demo ()
  (setf *server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port *port*
                        :document-root *root*))))

(defun stop-html-demo ()
  (hunchentoot:stop *server*))
```

![Screenshot](https://github.com/codr4life/vicsydev/blob/master/virtual-dom-demo.png)

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/cl4l).

peace, out
