# [vicsy/dev](https://github.com/codr4life/vicsydev) | scoping resources in C
posted Mar 19th 2017, 11:00 am

### preramble
Strategies for scoping resources in C generally falls to either end of the spectrum between injecting ```for```-statements and using paired begin/end-macros. The first approach requires the entire scope to be implemented by user code, rendering it invisible to the macro; while begin/end-macros lead to brittle, alien looking code. This post describes a middle way that uses varargs to pass reusable code blocks to macros.

### pass the code
The reason we need varargs is that more complex blocks risk being interpreted as multiple parameters by the pre-processor.

```C
#define DO_LOCK(lock, ...)  \
  ...                       \
  __VA_ARGS__;	            \
  ...                       \

DO_LOCK(&lock, { 
  ...
});
```

### more on varargs
Speaking of varargs, I'm suggest reserving them for idiomatic scenarios. Exposing array literals instead allows runtime processing of arguments and passing multiple varargs to the same macro/function, while leaving varargs available for code blocks.

```C
#define C4ARRAY(type, ...)					\
  (type[]){__VA_ARGS__}, C4ARRAY_LEN(type, ##__VA_ARGS__)	\

#define C4ARRAY_LEN(type, ...)				\
  (sizeof((type[]){__VA_ARGS__}) / sizeof(type))	\

#define C4TAGS(...)				\
  C4ARRAY(const char *, ##__VA_ARGS__)		\

struct c4timer c4suite_run(struct c4suite *self,
			   int_fast32_t warmups, int_fast32_t reps,
			   const char **run, int_fast8_t nrun,
			   const char **skip, int_fast8_t nskip);

c4suite_run(&tests, warmups, reps, C4TAGS("foo", "bar"), C4TAGS("baz"));
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and several examples of these ideas and more <a href="https://github.com/codr4life/libc4l">here</a>.

peace, out