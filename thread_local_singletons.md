# [vicsy/dev](https://github.com/codr4life/vicsydev) | thread-local singletons in C
posted Mar 21st 2017, 02:00 am

### preramble
Despite their bad reputation, singletons remain a useful tool. Some things are system global, period; and no amount of wrapping is going to change that fact. Declaring singletons as thread-local takes away the multi-threaded pain, which is the only objection to their use that really holds any substance.

```
C4SINGLETON(c4dom_attr_html_escape, c4escape, 
	    C4ARRAY(struct c4esc,
		    {'<',  "&lt;"}, 
		    {'>',  "&gt;"},
		    {'&',  "&amp;"},
		    {'\'', "&apos;"},
		    {'"',  "&quot;"}));

#define C4ARRAY(type, ...)					\
  (type[]){__VA_ARGS__}, C4ARRAY_LEN(type, ##__VA_ARGS__)	\

#define C4ARRAY_LEN(type, ...)				\
  (sizeof((type[]){__VA_ARGS__}) / sizeof(type))	\

struct c4esc {
  char ch;
  const char *str;
};

struct c4escape *c4escape_init(struct c4escape *self, 
			       struct c4esc *its, int_fast32_t nits);
```

### implementation
The ```C4SINGLETON```-macro declares an anonymous destructor that is registered for execution on exit when the value is initialized. The value is declared as thread-local and statically stack allocated and lazy-initialized on access, passing remaining arguments to the constructor. Any struct that implements ```X_init()``` and ```X_free()``` is supported.

```
#define _C4SINGLETON(name, type, __free, ...)				\
  void __free() { C4SYM(type, _free)(name()); }				\
									\
  struct type *name() {							\
    static __thread struct type val;					\
    static __thread bool init = true;					\
									\
    if (init) {								\
      C4SYM(type, _init)(&val, ##__VA_ARGS__);				\
      c4atexit(&__free);						\
      init = false;							\
    }									\
									\
    return &val;							\
  }									\
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of these ideas and more <a href="https://github.com/codr4life/libc4l">here</a>.

peace, out