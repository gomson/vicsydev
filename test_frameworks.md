# [vicsy/dev](https://github.com/codr4life/vicsydev) | the sad state of test frameworks
posted Mar 18th 2017, 04:00 am

### preramble
Hi, my name is Andreas and I prefer rolling my own test frameworks. I tried my best for several years, really did; but gradually came to the conclustion that I simply couldn't justify wasting more energy on arbitrary limitations. Testing is difficult enough, technically and emotionally; the last thing needed is long-winded ceremonies and hoops to jump through. This post describes a test framework in reasonably portable C that does it's thing and gets out of the way.

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

### dynamic groups
The dimensions and granularity I want to use for grouping tests depends on the scenario. Sometimes I want a quick regression check for the entire suite; at other times I want to run all database-dependent tests, or skip them as the case may be; this means that strict hierarchies like JUnit are out. Golang kind of provides the required flexibility, but requires serious regex-fu skills for triggering complex scenarios. The best solution I've come up with is to use tags instead of names and hierarchies; things like application name, module, class or type, bug-tracker id; and whatever else is needed to create a unique name. As time goes by more potential categories tend to emerge almost by themselves. When running tests, two sets of tags may be specified; tests that match all tags in the first set are triggered, unless they match any tags in the second set; fixtures that don't match any tags in the second set are run once for each repetition around the tests.

```C
#define C4ARRAY(type, ...)					\
  (type[]){__VA_ARGS__}, C4ARRAY_LEN(type, ##__VA_ARGS__)	\

#define C4ARRAY_LEN(type, ...)				\
  (sizeof((type[]){__VA_ARGS__}) / sizeof(type))	\

#define C4FIXTURES(...)				\
  C4ARRAY(c4fixture_callback, ##__VA_ARGS__)	\

#define C4TESTS(...)				\
  C4ARRAY(c4test_callback, ##__VA_ARGS__)	\

#define C4TAGS(...)				\
  C4ARRAY(const char *, ##__VA_ARGS__)		\

#define C4FIXTURE(name, ...)						\
  void C4SYM(run_, name)(struct c4fixture *self,			\
			 struct c4bset *run, struct c4bset *skip) {	\
									\
    __VA_ARGS__;							\
  }									\
									\
  struct c4fixture *name(struct c4suite *suite) {			\
    return c4suite_fixture(suite, C4STR(name), &C4SYM(run_, name));	\
  }									\

#define C4TEST(name, ...)						\
  void C4SYM(run_, name)() {						\
    __VA_ARGS__;							\
  }									\
									\
  struct c4test *name(struct c4suite *suite) {				\
    return c4suite_test(suite, C4STR(name), &C4SYM(run_, name));	\
  }									\

struct c4suite {
  struct c4ls root;
  struct c4bset fixtures, tests;
};

void c4suite_run(struct c4suite *self,
		 int64_t warmups, int64_t reps,
		 const char **run, int_fast8_t nrun,
		 const char **skip, int_fast8_t nskip) {
  struct c4bset runs, skips;

  c4bset_init(&runs, sizeof(char *), &c4cmp_str);
  for (int_fast8_t i = 0; i < nrun; i++) { 
    *(const char **)c4bset_add(&runs, run + i, NULL) = run[i]; 
  }

  c4bset_init(&skips, sizeof(char *), &c4cmp_str);
  for (int_fast8_t i = 0; i < nskip; i++) { 
    *(const char **)c4bset_add(&skips, skip + i, NULL) = skip[i];
  }

  C4TRY("test") {						       
    for (int32_t i = 0; i < warmups; i++) { 
      c4suite_run_fixtures(self, &runs, &skips);
    }		
    C4CATCH(e, NULL) { c4error_free(e); }
  }

  C4DO_BSET(&self->tests, _t) {
    struct c4test *t = _t;
    c4timer_reset(&t->timer);
  }

  c4suite_run_fixtures(self, &runs, &skips);
    
  C4TRY("test") {						       
    for (int32_t i = 0; i < reps-1; i++) { 
      c4suite_run_fixtures(self, &runs, &skips);
    }		
    C4CATCH(e, NULL) { c4error_free(e); }
  }									

  struct c4timer timer;
  c4timer_reset(&timer);
  printf("%-20s%15s\n", "TEST", "uSECS");

  C4DO_BSET(&self->tests, _t) {
    struct c4test *t = _t;
    printf("%-20s%15" PRIdFAST64 "\n", t->name, c4timer_usecs(&t->timer));
    timer.ticks += t->timer.ticks;
  }

  printf("%-20s%15" PRIdFAST64 "\n", "TOTAL", c4timer_usecs(&timer));

  c4bset_free(&runs);
  c4bset_free(&skips);
}

void c4suite_run_tests(struct c4suite *self,
		       struct c4bset *run, struct c4bset *skip) {  
  C4DO_BSET(&self->tests, t) { c4test_run(t, run, skip); }
}
```

### fixtures
The proper way to implement fixtures is to provide means to execute the rest of the suite from within the fixture; choosing this approach over before/after-callbacks reduces api duplication and simplifies fixture implementations.

```C
struct c4fixture {
  const char *name;
  struct c4suite *suite;
  struct c4ls node;
  struct c4bset tags;
  c4fixture_fn fn;
};

struct c4fixture *c4suite_fixture(struct c4suite *self, 
				  const char *name,
				  c4fixture_fn fn) {
  int_fast64_t pos = -1;
  struct c4fixture *f = c4bset_find(&self->fixtures, &name, NULL, 0, 0, &pos);

  if (f) { 
    f->fn = fn; 
    return f;
  } 

  return c4fixture_init(c4bset_ins(&self->fixtures, pos), self, name, fn);
}

static void parse_tags(const char *in, struct c4bset *out) {
  char *n = strcpy(c4acq(strlen(in)+1), in);		
  char *start = n, *end = NULL;					
  while ((end = strstr(start, "_"))) {				
    *end = 0;								
    *(char **)c4bset_add(out, &start, NULL) = start;		
    start = end + 1;						
  }									
									
  if (end != start) { *(char **)c4bset_add(out, &start, NULL) = start; }
}

struct c4fixture *c4fixture_init(struct c4fixture *self, 
				 struct c4suite *suite,
				 const char *name, 
				 c4fixture_fn fn) {
  self->suite = suite;
  self->name = name;
  self->fn = fn;
  c4bset_init(&self->tags, sizeof(const char *), &c4cmp_str);
  parse_tags(name, &self->tags);
  c4ls_prepend(&suite->root, &self->node);
  return self;
}

struct c4fixture *c4fixture_free(struct c4fixture *self) {
  c4bset_free(&self->tags);
  c4ls_del(&self->node);
  return self;
}

bool c4fixture_match(struct c4fixture *self,
		     struct c4bset *run, struct c4bset *skip) {
  struct c4bset_match match;						

  if (skip &&								
      c4bset_len(skip) &&						
      c4bset_match_next(c4bset_match(&self->tags, 0, skip, 0, &match))) {
    return false;							
  }									

  return true;
}

void c4fixture_run(struct c4fixture *self,
		   struct c4bset *run, struct c4bset *skip) {
  if (c4fixture_match(self, run, skip)) { self->fn(self, run, skip); }
  else { c4fixture_next(self, run, skip); }
}

void c4fixture_next(struct c4fixture *self, 
		    struct c4bset *run, struct c4bset *skip) {		       
  if (self->node.next == &self->suite->root) { 
    c4suite_run_tests(self->suite, run, skip);
  } else { c4fixture_run(C4PTROF(c4fixture, node, self->node.next), run, skip); }
}
```

### benchmarks
Most frameworks I've come across either completely ignore benchmarks, or focus too much on hard core use cases. Premature optimisation is the root of many problems, but so is not having any idea about performance as code evolves. Once tests may be grouped dynamically; adding support for warm up and repetition, and printing a table with elapsed times; takes care of most benchmarking needs without additional ceremony.

```
TEST                          uSECS
c4_array_tests                   36
c4_bset_tests                  8445
c4_buf_tests                    201
c4_fmt_tests                    124
c4_malloc_tests                  44
TOTAL                          8850
```

You may find more posts in the same spirit <a href="http://vicsydev.blogspot.de/">here</a>, and a full implementation of these ideas and more <a href="https://github.com/codr4life/libc4l">here</a>.

peace, out