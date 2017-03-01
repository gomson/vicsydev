# [vicsy/dev](https://github.com/codr4life/vicsydev) | calling the virtual DOM
posted March 1st 2017, 2:00 am

### preramble
The virtual DOM implementation described in [previous](https://github.com/codr4life/vicsydev/blob/master/virtual_dom.md) [posts](https://github.com/codr4life/vicsydev/blob/master/syncing_virtual_dom.md) is almost ready for prime time, the only missing piece of the puzzle is callbacks.

### piggy-backing
Piggy-backing modifications from the browser on the next request made to the server is a solid approach from my experience. I have yet to run into a case where the reason the server is interested in updates isn't tied to a callback being executed. Inputs are automatically hooked up to log their modifications. A minimal run-time is included to simplify the generated code; this script has to be included in dynamic documents, how that happens is up to user code.

```
var cl4l = {
    call: function(elem, event) {
        var data = cl4l.updates;
        cl4l.updates = {};
        data["cl4l-elem"] = elem;
        data["cl4l-event"] = event;
        
        $.ajax(cl4l.url, {
            data: data,
            dataType: "json",
            error: function(req, err, ex) {
                console.error(req, err, ex);
            },
            success: function(data, res, req) {
                eval(data);
            }
        });
    },
    url: null,
    update: function(id, val) {
        updates[id] = val;
    },
    updates: {}
};
```

### calling home
Callbacks are stored in lists, hashed by element- and event id; any number of callbacks may be registered for the same element/event. Calling ```html-call``` with an alist of params on each request and making sure the update script reaches the client is left in the hands of user code.

```
CL4L-HTML> (let* ((doc (html-doc :dynamic? t :call-url "foobar"))
                  (body (html-body doc))
                  (input (html-input body :id :name :type :text)))
       
             (html-button
               body
               :id :hello
               :body "Hello"
               :onclick (lambda ()
                          (update-html doc
                                       "alert('Hello ~a!');"
                                       (html-attr input :value))))
  
             (format t "~a~%~%" (to-html doc))
  
             (html-call doc '((:cl4l-elem  . "hello")
                              (:cl4l-event . "onclick")
                              (:name       . "World")))

             (princ (html-update-script doc)))
             
<!DOCTYPE html>
<html>
  <head id="head">
    <script id="init-script">
      $(function() {
        cl4l.url = 'foobar';
        $('#name').on('change', function() {
          cl4l.update(this.id, this.value);
        });
      });
    </script>
</head>
<body id="body">
  <input id="name" type="text"/>
  <button id="hello" onclick="cl4l.call('hello', 'onclick');">Hello</button>
</body>
</html>

alert('Hello World!');
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/cl4l).

peace, out
