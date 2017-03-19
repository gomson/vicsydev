# [vicsy/dev](https://github.com/codr4life/vicsydev) | on the sad state of test frameworks
posted Mar 18th 2017, 04:00 am

### preramble
Hi, my name is Andreas and I prefer rolling my own test frameworks. I tried my best for several years, really did; but gradually came to the conclustion that I just couldn't justify wasting more energy on arbitrary limitations. Testing is difficult enough the way it is, both technically and emotionally; the last thing needed is long-winded ceremonies and hoops to jump through. This post describes a test framework in reasonably portable C that does it's thing and gets out of the way.

```C
#include "c4l/test.h"

static C4FIXTURE(setup, { 
  WITH_FOOBAR () { C4FIXTURE_NEXT(); }
});

static C4TEST(foo_bar, {
  assert(...);
  ...
});

static C4TEST(foo_baz, {
  ...
});

static C4TEST(other, {
  ...
});

int main () {
  // Declares and initializes test suite
  C4SUITE(tests, C4FIXTURES(&setup), C4TESTS(&foo_bar, &foo_baz, &other));
  
  // Runs foo_bar with with 100 warmups and 100000 reps
  c4suite_run(&tests, 100, 100000, C4TAGS("foo"), C4TAGS("baz"));

  c4suite_free(&tests);
  return 0;
}
```

### fixtures
The proper way to implement fixtures is to provide means to execute the rest of the suite from within the fixture. This reduces duplication in the framework and allows straight forward implementations for the most common use case.

```C
#define C4FIXTURE(name, ...)					\
  struct c4timer name(struct c4suite *suite,			\
		      int_fast32_t warmups, int_fast32_t reps,	\
		      struct c4bset *run, struct c4bset *skip,	\
		      int_fast16_t fixture) {			\
    struct c4timer timer;					\
    __VA_ARGS__;						\
    return timer;						\
  }								\

#define C4FIXTURE_NEXT()						\
  timer = c4fixture_next(suite, warmups, reps, run, skip, fixture)	\

struct c4timer c4fixture_next(struct c4suite *suite,		       
			      int_fast32_t warmups, int_fast32_t reps,	
			      struct c4bset *run, struct c4bset *skip,	
			      int_fast16_t fixture) {		       
  if (fixture == 0) { return c4suite_run_tests(suite, warmups, reps, run, skip); } 

  fixture--;								
  return (*(c4fixture *)c4vec_at(&suite->fixtures, fixture))(suite,		
							     warmups, reps,	
							     run, skip,		
							     fixture);	
}
```

### dynamic groups
The dimensions and granularity I want to use for grouping tests depends on the scenario. Sometimes I want a quick regression check for the entire suite; at other times I want to run all database-dependent tests, or skip them as the case may be; this means that strict hierarchies like JUnit are out. Golang kind of provides the required flexibility, but requires discipline when naming tests and serious regex-fu skills for triggering complex scenarios. The best solution I've come up with is to use tags, things like application name; module, class or type, bug-tracker id; and whatever else is needed to create a unique name; as time goes by more potential categories tend to emerge almost by themselves. When running tests, two sets of tags may be specified; tests that have all their tags in the first set are triggered, unless they have any tags in the second set.

```C
#define C4ARRAY(type, ...)					\
  (type[]){__VA_ARGS__}, C4ARRAY_LEN(type, ##__VA_ARGS__)	\

#define C4ARRAY_LEN(type, ...)				\
  (sizeof((type[]){__VA_ARGS__}) / sizeof(type))	\

#define C4TAGS(...)				\
  C4ARRAY(const char *, ##__VA_ARGS__)		\

#define C4TEST(name, ...)						\
  struct c4timer name(int_fast32_t warmups, int_fast32_t reps,		\
		      struct c4bset *run, struct c4bset *skip) {	\
    struct c4timer timer;						\
    c4timer_reset(&timer);						\
    if (!c4test_match(C4STR(name), run, skip)) { return timer; }	\
									\
    printf("%-20s", C4STR(name));					\
    { __VA_ARGS__; }							\
									\
    C4TRY("test") {							\
      for (int32_t i = 0; i < warmups; i++) { __VA_ARGS__; }		\
									\
      C4DO_TIMER(&timer) {						\
	for (int32_t i = 0; i < reps; i++) { __VA_ARGS__; }		\
      }									\
    }									\
									\
    C4CATCH(e, NULL) {							\
      c4error_print(e, stdout);						\
      c4error_free(e);							\
    }									\
  									\
    printf("%14.3lfk\n", c4timer_msecs(&timer) / 1000.0);		\
    return timer;							\
  }									\

bool c4test_match(const char *name, struct c4bset *run, struct c4bset *skip) {
    struct c4bset tags;							
    c4bset_init(&tags, sizeof(char *), &c4cmp_str);			
    struct c4bset_match match;	
					
    char *n = strcpy(c4acq(c4malloc(), strlen(name)+1), name);		
    char *start = n, *end = NULL; 
    while ((end = strstr(start, "_"))) {				
      *end = 0;								
      *(char **)c4bset_add(&tags, &start, NULL) = start;		
      start = end + 1;							
    }									
									
    if (end != start) { *(char **)c4bset_add(&tags, &start, NULL) = start; }
									
    if (skip &&								
	c4bset_len(skip) &&						
	c4bset_match_next(c4bset_match(&tags, 0, skip, 0, &match))) {	
      return false;							
    }									
									
    if (run && c4bset_len(run)) {					
      c4bset_match(&tags, 0, run, 0, &match);				
      C4DO_BSET(run, t) {						
	if (!c4bset_match_next(&match)) { return false;	}		
      }									
    }									
									
    c4bset_free(&tags);							
    c4rel(c4malloc(), n);			
    return true;
}
```

### benchmarks
Most frameworks I've come across either completely ignore benchmarks, or focus too much on hard core use cases. Premature optimisation is the root of many problems, but so is not having any idea about performance as code evolves. Once tests may be grouped dynamically; adding support for warm up and repetition, and printing a table with elapsed times; takes care of most benchmarking needs without additional ceremony.

```
TEST                          MSECS
foo_bar                      0.683k
TOTAL                        0.001M
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of this idea and more <a href="https://github.com/codr4life/libc4l">here</a>.

peace, out