# [vicsy/dev](https://github.com/codr4life/vicsydev) | Lifoo - a Lispy, embedded Forth
posted Feb 25th 2017, 04:00 pm

### preramble
I've been craving for a simple but powerful embedded language that feels just right for a long, long time. I'm aware of Lua, but that's still not the kind of simplicity and seamless integration I'm aiming for. Since I'm mostly slinging Common Lisp these days, I decided to go all in and see how far it's possible to take the idea within that context.

### Forth
If you have no idea what Forth is; a first step is to think of it as Reverse Polish Notation for code, the kind of code Yoda would write. Like Lisp, Forth is more idea than implementation; arguably even more so than Lisp because of it's simplicity. While widely popular in embedded hardware circles, it's unfortunately mostly forgotten outside of that niche. Unfortunate because it's one of the most trivial languages to implement, and a solid foundation for embedded and/or domain specific languages. 

### Lifoo
There's a saying in Forth circles; that if you've seen one Forth compiler, you've seen one Forth compiler. [Lifoo](https://github.com/codr4life/lifoo) tries hard to borrow just enough ideas from Common Lisp to make the integration seamless, without sacrificing the simplicity of Forth.

```
Lifoo> 1 2 +

3

Lifoo> (1 2 +)

(1 2 +)

Lifoo> (1 2 +) eval

3

Lifoo> (1 2 +) compile

(PROGN
        (LIFOO-PUSH 1)
        (LIFOO-PUSH 2)
        (LIFOO-SAFE-CALL [word: + (number number)]))

Lifoo> (1 2 +) compile link

#<FUNCTION {1003F99E8B}>

Lifoo> (1 2 +) compile link call

3
```

### words
Forth likes to call it's functions words, and [Lifoo](https://github.com/codr4life/lifoo) keeps with that tradition. Words can be defined from within the language or via the API, using either [Lifoo](https://github.com/codr4life/lifoo) or Lisp.

```
Lifoo> (:foo cons)@ (symbol) :foo define
       :bar foo

(:FOO . :BAR)

Lifoo> (lifoo-push (concatenate 'string
                                (lifoo-pop)
                                (lifoo-pop))) lisp $
       (string string) :+ define
       "def" "abc" +

"abcdef"


(define-word :foo (symbol) ()
  :foo cons)

(define-lisp-word :+ (string string) ()
  (lifoo-push (concatenate 'string
                           (lifoo-pop)
                           (lifoo-pop))))
```

### repl
A basic REPL is [provided](https://github.com/codr4life/lifoo#setup) for playing around with code in real-time.

```
CL-USER> (lifoo-repl:lifoo-repl)
Welcome to Lifoo,
press enter on empty line to evaluate,
exit ends session

Lifoo> "hello Lifoo!" print ln

hello Lifoo!
NIL

Lifoo> exit
```

### tracing
Stack tracing takes on a whole new meaning in Forth, [Lifoo](https://github.com/codr4life/lifoo) offers integrated tracing to help untangle messy stacks.

```
Lifoo> (number number) :+ word trace

NIL

Lifoo> "Every :+ entry and exit is traced from here" log

NIL

Lifoo> 1 2 +

3

Lifoo> untrace

NIL

Lifoo> "Nothing is traced from here" log

NIL

Lifoo> 3 4 +

7

Lifoo> print-trace

LOG   Nothing is traced from here
EXIT  + (3)
ENTER + (2 1)
LOG   Every :+ entry and exit is traced from here
NIL
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of this idea and more [here](https://github.com/codr4life/lifoo).

peace, out
