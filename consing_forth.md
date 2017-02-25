# [vicsy/dev](https://github.com/codr4life/vicsydev) | building a Lispy Forth
posted Feb 25th 2017, 8:00 pm

### preramble
In a previous [post](https://github.com/codr4life/vicsydev/blob/master/lispy_forth.md), I presented the humble beginnings of [Lifoo](https://github.com/codr4life/lifoo); a Lispy, Forth-like language implemented and embedded in Common Lisp. This post goes further into specific features and the reasoning behind them. I decided from the start that this was going to be a fresh take on Forth, in the spirit of Common Lisp; taking nothing for granted; and I ran into plenty of interesting design choices as a result.

### repl
If you wan't to play along with the examples, a basic REPL may be started by cloning the [repository](https://github.com/codr4life/lifoo), followed by loading and evaluating ```(lifoo-repl:lifoo-repl)```. Linux binaries are also [provided](https://github.com/codr4life/lifoo#setup).

```
CL-USER> (lifoo-repl:lifoo-repl)
Welcome to Lifoo,
press enter on empty line to evaluate,
exit ends session

Lifoo> 1 2 +

3

Lifoo> exit
```

### reader
One of the goals set early in the design process was to reuse the Lisp reader for parsing Lifoo code. Looking back, sticking with this choice was fundamental to achieving a seamless integration since it formed a natural obstacle to deviating too far from Lisp.

```
Lifoo> "1 2 +" read

(1 2 +)

Lifoo> "1 2 +" read eval

3

Lifoo> (1 2 +) write

"1 2 +"

Lifoo> (1 2 +) write read

(1 2 +)

Lifoo> (1 2 +) write read eval

3
```

### quoting
List literals are quoted by default, items are treated as code when evaluated. The price for convenience is that evaluating literal items as anything but code takes a bit of ceremony, but the approach fits like a glove with the simplicity of Forth. ```@``` pre-compiles the preceding Lifoo expression to a lambda.

```
Lifoo> (1 2 3)

(1 2 3)

Lifoo> (1 2 3) (2 *)@ map

(2 4 6)

Lifoo> (2 *)@

#<FUNCTION {1008CD673B}>

Lifoo> (1 2 3) (2 *)@ map

(2 4 6)

Lifoo> ((1 2 +) (3 4 +) (5 6 +)) (eval)@ map

(3 7 11)

Lifoo> (1 2 +)@ (3 4 +)@ (5 6 +)@
       3 list
       (call)@ map

(3 7 11)
```

### symbols
Symbols in the token stream are interpreted as words to be called, a tradition inherited from Forth. Luckily, Common Lisp offers another kind of symbol in the form of keywords.

```
Lifoo> "lifoo" sym

:LIFOO

Lifoo> :lifoo

:LIFOO

Lifoo> 1 2 + stack

(3)

Lifoo> 1 2 :+ stack

(:+ 2 1)
```

### comparisons
Common Lisp reserves the usual operators for numbers, ```+-*/<>``` among others; Lifoo follows this tradition but additionally provides generic comparisons that work with any kind of values.

```
Lifoo> 1 2 + 3 =

T

Lifoo> "abc" "def" neq?

T

Lifoo> "def" "abc" lt?

T

Lifoo> '(1 2 3) '(1 2 3 4) cmp

1
```

### setf
The beauty of ```setf``` is that it untangles specifying a place from setting its value. If you still can't see it; imagine writing a generic function that can set indexes in arrays and replace tails of lists in any other language; then add fields in structs and keys in hash tables; ```setf``` allows you to pull tricks like that without missing a beat; including hooking your own places into the protocol. Lifoo provides a ```set``` word that sets values for any place that implements the protocol.

```
Lifoo> #(1 2 3) 1 nth 4 set drop

#(1 4 3)

Lifoo> (t t) :put word source

"rotl get rotr set"

Lifoo> nil hash :foo 42 put drop list

((:FOO . 42))

Lifoo> :bar var 42 set drop
       :bar var

42
```

### del
One thing Python got right was providing an extendable protocol for deletion. Separating concerns into independent pieces of generic functionality enables exponential power gains. Lifoo provides a ```del``` word that deletes any place that implements the protocol.

```
Lifoo> "abc" 1 nth del

"ac"

Lifoo> ((:foo . 1) (:bar . 2)) hash :foo get del drop list

((:BAR . 2))


Lifoo> :bar var 42 set drop
       :bar var del drop
       :bar var

NIL
```

### definitions
I decided to deviate from the traditional Forth syntax for defining words, since neither the Lisp reader nor I approve of the syntax. Lifoo provides a ```define``` word that defines preceding code and symbol as a word. ```define``` can be called anywhere; overwrites any previous bindings, and makes the new definition available for immediate use.

```
Lifoo> (lifoo-push (concatenate 'string
                                (lifoo-pop)
                                (lifoo-pop)))@@
       (string string) :+ define
       "def" "abc" +

"abcdef"
```

### packages
Lifoo provides support for modular and extensible protocols. Words may belong to several different protocols. Besides support for ```init```; protocols are second class, the only way of defining one is via the API. The good news is that the API is right around the corner as long as you remembered to initialise the ```:meta``` protocol; and the REPL initialises all protocols by default.

```
Lifoo> (define-lifoo-init (:foo :bar) ()
         (define-word :baz () () 39 +)) lisp
       (:foo :bar) init
       3 baz

42
```

### structs
Lifoo provides a simple but effective interface to defstruct. Outside of Lifoo the struct is anonymous to not clash with existing Lisp definitions. Words are automatically generated for ```make-foo```, ```foo?``` and fields including setters when ```struct``` is evaluated.

```
Lifoo> ((bar -1) baz) :foo struct
       nil make-foo foo?

T

Lifoo> (:bar 42) make-foo
       foo-bar

42

Lifoo> (:bar 42) make-foo
       foo-bar 43 set
       foo-bar

43
```

### macros
Once token streams come served on silver plates, it's really hard to resist the temptation of going all in with macros. What I ended up with is essentially Lisp macros with a touch of Forth. Like Lisp, Lifoo macros operate on streams of tokens. But since Forth is post-fix; macros deal with preceding, rather than wrapped tokens. Lifoo provides macro words that are called to translate the token stream when compiling.

```
Lifoo> (number number) :+ word

[word: + (number number)]

Lifoo> (+ 1 2) compile

(PROGN
        (LIFOO-SAFE-CALL [word: + (number number)])
        (LIFOO-PUSH 1)
        (LIFOO-PUSH 2))

Lifoo> :@ macro

[macro word: @]

Lifoo> ((+ 1 2)@) compile

(LIFOO-PUSH #<FUNCTION (LAMBDA ()) {10068E0DDB}>)


(define-macro-word :@ (in out)
  (let* ((code `(lambda ()
                  ,(lifoo-body (lifoo-compile (first in)))))
         (fn (when (lifoo-link?) (eval code))))
      (values (rest in)
              (cons `(lifoo-push ,(if (lifoo-link?) fn code))
                    out))))
```

You may find more in the same spirit [here](http://vicsydev.blogspot.de/) and [here](https://github.com/codr4life/vicsydev), and a full implementation of these ideas and more [here](https://github.com/codr4life/lifoo).

peace, out
