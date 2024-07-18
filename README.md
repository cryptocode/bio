<img src="https://user-images.githubusercontent.com/34946442/143932794-213c6eaa-76ca-4f98-985f-345d1fc8e925.png" width=100 height=100>

Bio is an experimental Lisp dialect similar to Scheme, with an interpreter written in [Zig](https://ziglang.org)

Features include macros, garbage collection, error handling, a module facility, destructuring, and a standard library.

Example:

```scheme
(filter
    (quicksort '(5 40 1 -3 2) <)
        (λ (x) (>= x 0)))

(1 2 5 40)
```

The core of Bio is lambda expressions, from which the standard library builds syntax like `type` and `fun`:

```scheme
; Create a composite type. This is sugar for `(var Point (lambda (x y) ... (self)))`
(type Point (x y) 
    (fun area () (* x y)))

; Make an instance of Point
(var pt (Point 5 7))

; Call a member function. All this is just syntax sugar based on a lambda
; returning its own environment.
(print "Area:" (pt (area)) '\n)

Area: 35
```

The documentation, like the language, is work in progress. For up-to-date examples, I recommend studying `std.lisp`, `test.lisp` and the files in the `examples` directory.

Table of Contents
=================

   * [Building and running](#building-and-running)
      * [Running tests](#running-tests)
   * [Language Reference](#language-reference)
      * [Naming conventions](#naming-conventions)
      * [Intrinsics](#intrinsics)
         * [nil](#nil)
         * [#t and #f](#t-and-f)
         * [#! and #value](#-and-value)
         * [typename](#typename)
         * [number? symbol? list? bool? callable? error?](#number-symbol-list-bool-callable-error)
         * [var and define](#var-and-define)
         * [vars](#vars)
         * [set! and unset!](#set-and-unset)
         * [arithmetic functions](#arithmetic-functions)
         * [equality](#equality)
         * [order](#order)
         * [env and gc](#env-and-gc)
         * [fun, lambda and λ](#lambda-and-λ)
         * [type](#type)
         * [macro](#macro)
         * [macroexpand](#macroexpand)
         * [self and environment lookups](#self-and-environment-lookups)
         * [&amp;rest](#rest)
         * [quote](#quote)
         * [quasiquote](#quasiquote)
         * [unquote](#unquote)
         * [unquote-splicing](#unquote-splicing)
         * [eval](#eval)
         * [apply](#apply)
         * [gensym](#gensym)
         * [if](#if)
         * [cond](#cond)
         * [loop](#loop)
         * [begin](#begin)
         * [try](#try)
         * [error](#error)
         * [print](#print)
         * [as](#as)
         * [list](#list)
         * [append](#append)
         * [range](#range)
         * [len](#len)
         * [string](#string)
         * [import](#import)
         * [assert](#assert)
         * [exit](#exit)
         * [debug-verbose](#debug-verbose)
         * [math.pi and math.e](#mathpi-and-mathe)
         * [math.floor](#mathfloor)
         * [string.split](#stringsplit)
      * [Standard library](#standard-library)
         * [car, cdr, caar, cadr, cddr, caddr, last, nth](#car-cdr-caar-cadr-cddr-caddr-last-nth)
         * [cons](#cons)
         * [nil?](#nil-1)
         * [atom?](#atom)
         * [bool?](#bool)
         * [relational functions](#relational-functions)
         * [logical functions](#logical-functions)
         * [let macro](#let-macro)
         * [filter](#filter)
         * [map](#map)
         * [quicksort](#quicksort)
         * [while macro](#while-macro)
         * [list.iterate and each](#listiterate-and-each)
         * [reduce-with](#reduce-with)
         * [each-pair](#each-pair)
         * [matrix functions](#matrix-functions)
         * [hashmap](#hashmap)
         * [io.read-number](#ioread-number)
         * [typename](#typename-1)
         * [time.now](#timenow)
         * [double-quote](#double-quote)
         * [inc! and dec!](#inc-and-dec)
         * [file i/o and stdin/stdout](#file-io-and-stdinstdout)
         * [math.odd? math.even? odd-items and even-items](#mathodd-matheven-odd-items-and-even-items)
         * [math.abs](#mathabs)
         * [math.pow](#mathpow)
         * [math.average](#mathaverage)
         * [math.sqrt](#mathsqrt)
         * [math.safe-div](#mathsafe-div)
         * [math.make-random-generator and math.random-list](#mathmake-random-generator-and-mathrandom-list)
         * [math.fib and math.fact](#mathfib-and-mathfact)
         * [Y](#y)
      * [Modules](#modules)
         * [Module example](#module-example)

# Building and running

Clone the repository and cd to the root directory.

Bio currently targets Zig v13.0

**Build**

```bash
zig build
```

**Run the REPL:**

```bash
zig-out/bin/bio
```

**Run a Bio source file:**

```bash
zig-out/bin run examples/triangles.lisp
```

You can also use `import` to evaluate files from the REPL, e.g. `(import "examples/albums.lisp")`

## Running tests

The test suite in `test.lisp` can be evaluated with `zig test src/main.zig` or `./bio run test.lisp`

# Language Reference

A Bio program consists of one or more s-expressions. An s-expression is recursively defined as being either

1. an atom
2. a list, which may contain lists and atoms

An atom is either a 64-bit floating point number, or a symbol. A symbol is any sequence of utf8 code points that is not a valid number. Symbols serve as strings when the symbol is enclosed in double-quotes, such as `"This is a symbol"`. Bio does not have a separate string type; the term string is simply used to denote a symbol serving as a string.

## Naming conventions

* Predicates have a question mark suffix, such as `atom?`
* Destructive actions have an exclamation point suffix, such as `set!`
* Sentinels are prefixed with an ampersand, such as `&rest`
* Symbols with special meaning are prefixed `#`, such as `#t`
* Identifiers are kebab-case, while composite types such as modules are PascalCase

## Intrinsics

Intrinsics are built-in functions, macros, and symbols implemented by the interpreter. They are building blocks for the standard library and user programs.

### nil

A symbol representing the absence of a value. Note that `nil` and `'()` are considered equal.

### #t and #f

Symbols representing true and false, respectively. Note that `nil` coerces to `#f`.

### #?

A symbol representing the last top-level expression evaluation.

Example:

```scheme
(+ 2 3)
(* 2 #?)
10
```

### #! and #value

The `#value` symbol contains the value returned by a *tried* expression. This usually removes the need to use a temporary variable when you need to both check for errors *and* use the result.

The `#!` symbol contains the error after a `try` expression. If no error occurs, this is set to `nil`.

```
(try (math.safe-div (io.read-number) (io.read-number))
    (print "The doubled result is: " (* 2 #value))
    (print "Failed: " #!))
```

### typename

The 'typename' functions returns a string representation of the type. The most specific classification is returned, so `(typename #t)` is "bool", even though #t is also a symbol.

```scheme
(typename 'a)
"symbol"

(typename math.pi)
"number"

(typename #t)
"bool"

(typename '(a b c))
"list"

(typename +)
"function"
```

### number? symbol? list? bool? callable? error?

Predicates to determine expression types. There's also an `atom?` predicate in the standard library.

These all return #t

```scheme
(number? -5.7)
(number? x)
(symbol? 'x)
(symbol? #t)
(bool? #t)
(bool? #f)
(list? '(abc))
(callable? +)
(error? (math.safe-div 4 0))
```

### var and define

'var' creates a variable binding in the *current* environment. The binding will fail if it already exists in the current environment. The `define` function is just an alias to `var`

Variable names can be any utf8 sequence that doesn't start with a number, " and '. Variable names are case-sensitive.

```scheme
(var x 5)
(define y 5)
(var name (io.read-line))
(var double (lambda (val) (* 2 val)))
(var 😀 "Smiley")
(print 😀)
"Smiley"
```

Local variables:

```scheme
(var x 2)
(var y 2)
(var z 2)
(var some-function (lambda (x)

    ; Allowed, y is not in the local scope
    (var y 10)

    ; Not allowed, x is a formal in the same scope
    ; (var x 10)

    (print x y z)
))

> (some-function 3)
3 10 2
```

### vars

A list can be destructured into variables using `vars`


```scheme
(var stuff '(1 2 3))
(vars a b c stuff)
(assert (= a 1))
(assert (= b 2))
(assert (= c 3))
```

### set! and unset!

Changes the value of an existing binding (values themselves are immutable). The binding is searched from current to root scope.

```scheme
; Define x, then update it
(var x 5)
(set! x 10)

; Be evil and redefine + to mean -
(set! + -)
(+ 10 2)
8

; Remove binding, allowing it to be defined again
(unset! x)
(var x 'hey)
```

You can optionally pass a specific environment for the binding being set as the first argument:


```scheme
(type Point (x y)
    (fun area () (* x y)))

(var pt (Point 5 7))
(print "Area:" (pt (area)) '\n)

Area: 35

; Change x by passing the pt environment to set!
(set! pt x 6)
(print "Area:" (pt (area)) '\n)

Area: 42
```

### arithmetic functions

The arithmetic functions work on floating-point numbers. Unlike infix notation, any number of arguments can be given.

```scheme
(+ -3 5)
2

(/ 2 (* 10 (+ math.pi 2 3 (- 2 3))))
0.028004957675577865

; pi symbol is same as math.pi (tip: this is available with Option+P if you're on a mac)
(define circumference (λ (x) (* 2 π x)))
```

Additional math functions are available in the standard library.

### equality

The `=` and `~=` functions check for exact and approximate equality. The approximate case is only for numeric operands and accepts a third argument to override the tolerance (epsilon is by default 1e-7)

```scheme
(= x 5)
(= '(1 2 3 4) nums)
(= '(1 3) (odd-items nums))
(~= a b 0.0005)
```

Exact equality is implemented in terms of the `order` function, allowing nested lists to be compared.

### order

The `order` function returns 0, -1, 1 do indicate if the first argument is equal, less than or greater than the second argument. All expression types are supported.

Lists are recursively compared. If list lengths differ, the shorter one is considered smaller. Empty lists and `nil` are considered equal, otherwise `nil` is always considered smaller.

```scheme
(order 100 200)
-1

(order 'def 'abc)
1

(order '(1 2 3) '(0 1 2))
1

(order '(1 2) '(0 1 2))
-1

(order 6 (* 2 3))
0
```

Standard library functions such as `<` are implemented in terms of `order`.

### env and gc

`env` prints the content of the current environment.

```
> (env)
Environment for global: Env@10db4b000
    import = <function>, env *Env@0
    exit = <function>, env *Env@0
    gc = <function>, env *Env@0
    #f = #f, env *Env@0
    #t = #t, env *Env@0
    #? = #t, env *Env@0
    nil = nil, env *Env@0
    ...
```

The garbage collector runs periodically, though the criteria and extent are intentionally left undefined by this language reference.

### fun, lambda and λ

Creating functions in Bio can be done either with `fun` or `lambda`. The `fun` function is just a convenience macro that expands to a lambda definition.

```scheme
> (fun square (x) (* x x))
> (square 5)
25
```

Below is the equivalent lambda definition:

```scheme
> (var square (lambda (x) (* x x)))
> (square 5)
25
```

Direct application without binding to a variable:

```scheme
> ((lambda (x) (* x x)) 5)
25
```

The lambda symbol can be used in place of the lambda identifier

```scheme
(var doubler (λ (x) (* 2 x)))
```

A lambda invocation has its own environment, and the parent environment is the one that existed when the lambda was defined. In other words, Bio is lexically scoped.

### type

A type expression is syntax sugar for functions returning their own environment. This is useful when making composite types.

```scheme
(type Point (x y)
    (fun area () (* x y))
)

(var pt (Point 5 7))
(print "Area:" (pt (area)) '\n)

; Change x by passing the pt environment to set!
(set! pt x 6)
(print "Area:" (pt (area)) '\n)
```

Composite types can contain local variables and other functions, just like regular lambda expression.

### macro

This function creates a macro.

Unlike lambdas, arguments are not evaluated when the macro is invoked. Instead, they're evaluated if and when the body does so. Note
that eager evaluation of macro arguments can be forced by placed `&eval` in front of the formal parameter.
 
When the macro is invoked, the body is evaluated. The returned expression (which represents Bio code) is then evaluated as the final result.

```scheme
(var print-with-label (macro (label &rest values)
	`(print label ": " ,@values)
))

(print-with-label Primes 2 3 5 7)

Primes: 2 3 5 7
```

A macro invocation has its own environment, and the parent environment is the current one. This is different from lambdas whose parent environment is the one in which the lambda was defined.

### macroexpand

You can stop the evaluation of the code returned from a macro by wrapping it in `macroexpand`

Consider a typical swap macro:

```scheme
(var swap (macro (a b)
    `(let ((temp ,a))
       (set! ,a ,b)
       (set! ,b temp))))
```

Here's a typical usage example:

```scheme
(let ((x 3) (y 7))
    (swap x y)
    (print "Swapped:" x y "\n")
)

Swapped: 7 3
```

But now we wanna see how the macro is expanded as code instead:

```scheme
(let ((x 5) (y 8))
    (print "Macro expansion:" (macroexpand (swap x y)) "\n")
)

Macro expansion: (let ((temp x)) (set! x y) (set! y temp))
```

Of course, you can store away the expansion for later invocation, or just evaluate the expansion directly:

```scheme
(let ((x 5) (y 8))
    (eval (macroexpand (swap x y)))
    (print "Swapped:" x y "\n")
)

Swapped: 8 5
```

### self and environment lookups

The `self` function returns the current environment as an expression. This can then be used as a function to perform lookups in that environment. This enables composite data types with their own functions, as well as modules. The distinction is purely conceptual.

A top-level `(self)` call will return the root environment as an expression:

```scheme
> (self)
<env>

> ((self) +)
<function>

> ((self) (+ 1 2))
3

> (((self) +) 1 2)
3
```

As you can see, when an environment is placed in the first position of a list, it changes which environment the following argument is looked up in.

The argument can either be a symbol, or a list which will be interpreted in the context of the new environment.

Use cases of `self` include modules, composite data types, polymorphic behavior, and enabling duck-typed interfaces/protocols.

See [Modules](#modules) for more information and an example.

### &rest

A sentinel symbol causing the rest of the arguments to be delivered as a single list argument. This enables variadic functions and macros.

### quote

The `quote` function and the `'` shorthand returns the argument unevaluated.

```scheme
> (quote a)
a

> 'a
a

> '(a b 1 2)
(a b 1 2)

```

### quasiquote

The `quasiquote` function and the \` shorthand returns the argument unevaluated. Unlike `quote`, however, it allows arguments to be selectively evaluated using `unquote` and `unquote-splicing`

```scheme
; Evaluate one of the list items
> (quasiquote (1 2 (unquote (+ 1 2)) 4))
(1 2 3 4)

; Same thing using shorthand notation
> `(1 2 ,(+ 1 2) 4)
(1 2 3 4)

; Use unquote-splicing to make a larger list of primes:
> (var primes '(2 3 5 7 11 13))
> `(,@primes 17 19 23)
(2 3 5 7 11 13 17 19 23)
```

Quasi quotation is commonly used to make templates in macros, but it has uses in regular functions as well.

### unquote
In the context of a quasiquote, evaluate the argument. The shorthand version is `,`

### unquote-splicing
In the context of a quasiquote, evaluate the elements of the list and place the result in the enclosing list. The shorthand version is `,@`

### eval

Evaluates all arguments, leaving the last evaluation as the result. If quote and quasiquote expressions are encountered, these are unquoted before evaluation.

```scheme
(eval '(+ 1 2))
3

(var expr '(* 2 3))
```

### apply

Evaluates the given function with the given argument list. The last argument must be a list argument. Any preceding arguments are prepended to that list. This means that `(apply + 1 '(2 3))` is equivalent to `(apply + '(1 2 3))`.

```scheme
> (apply + 5 2 1 '(10 20))
38

> (var list-of-numbers '(1 2 3 4))
> (apply * list-of-numbers)
24

> +
<function>

> (apply #? '(5 2))
7
```

Using `apply` is mostly useful when arguments are given as a list and the function at hand expects arguments to be passed individually.

### gensym

Generates a unique symbol.

```scheme
> (gensym)
gensym_1

> (gensym)
gensym_2
```

### if

The `if` expression evaluates the first argument. If true, then the second argument is evaluated, otherwise the third (optional) argument is evaluated. Each branch can have multiple expressions using constructs such as `begin` or `let`.

```scheme
(if (< x 10) 'Yes 'No)

(var res (if (math.odd? x) 'Odd 'Even))
```

Here's a list of if expressions from `examples/fizzbuzz-if.lisp`, none of which have an else branch:

```scheme
(if (= 0 x) (print "Fizz"))
(if (= 0 y) (print "Buzz"))
(if (and (!= 0 x) (!= 0 y)) (print i))
```

### cond

The `cond` expression is useful when if/else conditions lead to deep nesting. It takes a variable number of predicate/body pairs, and ends with an else clause. The else clause is *required* and consists only of the body. It must be the last entry in the cond expression.

Here's the cond expression from `examples/fizzbuzz-cond.lisp`:

```scheme
(cond
    ((and (= 0 x) (= 0 y)) (print "FizzBuzz" "\n"))
    ((= 0 x) (print "Fizz" "\n"))
    ((= 0 y) (print "Buzz" "\n"))
    ((print i "\n"))
)
```
### loop

The `loop` function loops from n to m, or until &break is encountered

The current iteration is optionally available in the given induction variable.

```scheme
; loops 10 times
(loop '(0 10) (print 'Hi\n))

; loops 10 times counting down
(loop '(10 0) (print 'Hi\n))

; loops 10 times, current iteration count goes into the idx variable
; the current iteration is available in the idx variable (you can call it anything)
(loop 'idx '(0 10) (print "Hi #" idx "\n"))

; loops forever until &break is encountered
(loop 'idx '() (print "Hi #" idx "\n") (if (= idx 4) &break))
```

### begin

Evaluates a list of expressions and returns the last one as the result. This is useful when more than one expression needs to be evaluated, like in the branches of the `if` function:

```scheme
(if (< i 10)
    (begin
        (var x 5)
        (set! x (+ x 1))
        (print (* x 2))
    )
)

12
```

### try

The `try` function evaluates the first argument. If the result is not an error expression, then the second argument is evaluated (the success branch), otherwise the third argument is evaluated (the error branch). The error branch is optional (in which case `nil` will be returned; add an error branch if you want to propagate the error.)


It's often necessary to know the value of the tried expression if it succeeds. This can be done using an intermediary variable, or by looking up the `#value` symbol:

```scheme
(try (math.safe-div 6 2)
    (if (= #value 3)
        (print "As expected!\n")
    )
)
```

The error expression is available through the `#!` symbol, used here by the error branch:

```scheme
(try (math.safe-div 6 2)
    (if (= #value 3)
        (print "As expected!\n")
    )
    (print "Could not divide: " #!)
)
```

A function that may fail does not have to be used in a try function:

```scheme
> (var res (math.safe-div x y))
3.5

> (math.safe-div x 0)
Division by zero
```

You can also use `try` after the fact:

```scheme
> (var res (math.safe-div x 0))
> (try (math.safe-div 1 0) #t `(string "Not good: " ,#!))
Not good: Division by zero
```

### error

The `error` function creates a new error expression.

```scheme
(var fail-if-ten (lambda (x)
    (if (= x 10)
        (error "10 is not allowed")
        #t
    )
))

(try (fail-if-ten 10)
    "All good"
    (begin
        (print "Something went terribly wrong!\n")
        #!
    )
)

Something went terribly wrong!
10 is not allowed
```

Notice how the `try` expression propagates the error by putting `#!` as the last expression in the error case.

Errors don't have to be symbols, any expression will do.

### print

`print` prints one or more expressions separated by a space. Combine `print` with `string` if you need to print verbatim (without spaces between expressions)

```scheme
(print "What's your name?")
(var name (io.read-line))

(print "What's your age?")
(var age (as number (io.read-line)))

(print "Hi" name (if (> age 80) "... you're quite old" ""))
```

IO examples work best if you put them in a file and then use `(import "thename.lisp")` or `bio run thename.lisp`

See also the `io.` functions in the standard library.

### as

The `as` function converts an expression from one type to another. If a conversion is not supported, `nil` is returned.

The target conversion is either `number`, `symbol`, or `list`.

```scheme
(var age (as number (io.read-line)))
(assert (= '(5) (as list 5)))
(as symbol mynumber)
(set! age (as symbol age))
```

### list

Creates a new list from its arguments. Quoting can be used to create lists without evaluating the expressions.

```scheme
> (var x 3)
> (list 1 2 x)
(1 2 3)

> '(1 2 x)
(1 2 x)
```
### append

Creates a new list from its arguments. List arguments are spliced into the new list.

```scheme
> (append '(a b c) '(d e f (g h)))
(a b c d e f (g h))

> (append 'a 'b 1 2 (+ 1 2))
(a b 1 2 3)
```

### range

The `range` function is the building block for querying lists, and is used to build standard library functions such as `car` and `cdr`.

* If called with no arguments, the first expression in a list is returned.
* If called with one argument, a list containing the sublist from `start` to end-of-list is returned.
* If called with two arguments, a list containing the sublist from `start` to `end` (exclusive) is returned.

Negative indices are end-of-list relative. `nil` is returned if any indices are out of range.

```scheme
> (var letters '(a b c d e))
(a b c d e)

> (range letters)
a

> (range letters -1)
(e)

> (range (range letters -1))
e

> (range letters 2)
(c d e)

> (range letters 2 4)
(c d)

> (range letters -4 -2)
(c d)

```

### len

The length of a list (in item count) or symbol (in bytes)

```scheme
> (len '(1 2 3))
3
```

### string

Creates a symbol by concatenating the *rendering* of its arguments.

```scheme
> (var message (string "The value is " x))
The value is 5

> (string "An error occurred : " #!)
An error occurred : Division by zero
```

### import

Reads and evaluates the given file. The path can be relative or absolute.

```scheme
(import "examples/albums.lisp")
```

### assert

Checks if the expression evaluates to #t. If not, an error is printed and the process is terminated. Evaluates to #t if successful.

```scheme
(assert (= '(a b c 1 2 3) mylist))
```

### exit

Exits the process with an optional exit code (default is 0)

```scheme
(exit 1)
```

### debug-verbose

Toggles the verbosity flag. When on, some details are printed during evaluation, such as `nil` results and quasiquote expansions.

### Math pi and e

The values of π and Euler's number respectively. The symbol `π` is an alias to `pi` in the Math module.

### Math floor

Returns the largest integer less than or equal to the argument.

```scheme
(var math (Math))
(math (floor (math pi)))
3
```

### string.split

Given one or more delimiters, tokenizes the input symbol and produces a list of symbols.

```scheme
(assert (= (string.split "" ",") '()))
(assert (= (string.split "a" ",") '(a)))
(assert (= (string.split "a,b,c" ",") '(a b c)))
(assert (= (string.split "a,b,c," ",") '(a b c)))
(assert (= (string.split "a,b;c," ",;") '(a b c)))
(assert (= (string.split " " ",") (list " ")))
```

## Standard library

The standard library is a file called `std.lisp` that's loaded and evaluated when the interpreter starts.

### car, cdr, caar, cadr, cddr, caddr, last, nth

These functions treat a list as pairs in the classical Lisp sense. `car` returns the first list item, while `cdr` returns the rest of the list. `last` returns the last list item.

```scheme
(var nums '(1 2 3 4))
(assert (= 1 (car nums)))
(assert (= 2 (cadr nums)))
(assert (= 3 (caddr nums)))
(assert (= 4 (last nums)))
(assert (= 3 (nth 2 nums)))
(assert (nil? (nth 100 nums)))

```

### cons

Prepends an item to a list:

```scheme
> (cons 'a '(b c))
(a b c)


```

### nil?

True if the argument is `nil` or an empty list.

### atom?

True if the argument is a number or a symbol (in other words, not a list)

### bool?

True if the argument is `#t` or `#f`

### relational functions

The relation functions are `<=`, `<`, `>`, `>=`, `!=` in addition to the intrinsic `=`

### logical functions

The `and` and `or` macros perform the usual shortcut evaluation. The `not` function checks if the argument is false. If so, the result is then `#t`, otherwise it's `#f`

### let macro

Local bindings can be created with a `lambda` expression. If the only reason to create a lambda is to have local variables, then the `let` macro is more convenient.

```scheme
(var x 10)
(var y 20)

(let ((x 5) (y 6))
    (+ x y)
)

11
```

### filter

Filters a list.

```scheme
> (filter (lambda (x) (< x 5)) '(3 9 5 8 2 4 7))
(3 2 4)
```

### map

Applies a function over one or more lists.

```scheme
; Create a list of sums taking operands from three lists
> (map + '(0 2 5) '(1 2 3) '(1 2 3))
(2 6 11)

; Double every element in a list
> (map (λ (x) (* 2 x)) '(1 2 3))
(2 4 6)

; A list of pairs with the order reversed
> (map (λ (x y) (list y x)) '(a b c) '(1 2 3))
((1 a) (2 b) (3 c))
```

### quicksort

Sorts a list using the supplied comparator function. The following example sorts the same list in ascending and descending order by passing `<` and `>` as the comparator functions. In the ascending example, we also filter out negative numbers:

```scheme
> (filter
    (quicksort '(5 40 1 -3 2) <)
    (λ (x) (>= x 0)))
(1 2 5 40)

> (quicksort '(5 40 1 -3 2) >)
(40 5 2 1 -3)

```

You can also pass a lambda to do your own ordering. See the **albums** example file for an example of doing this to sort albums.

### while macro

Expands to a tail-recursive function running a body while the predicate holds:

```scheme
(while (< c 10000000)
    (print "Value is now " c "\n")
    (inc! c)
)
```

### list.iterate and each

The `list.iterate` function allows for convenient iteration of lists. The first argument is a list. The second argument is a function that's called for every item in the list, with the item as an argument. `each` is an alias to this function.

```scheme
; Print all numbers
(each lots-of-numbers print)

; Create a new list of numbers, with double the values
(var result '())
(each nums (lambda (item)
    (set! result (append result (* 2 item)))
))

(2 4 6 8 10 12)
```

### reduce-with

Signature: `(reduce-with initial fn op list)`

Calls `op` on every item in `list`, but only after applying the function `fn` to the item.

```scheme
(reduce-with 0 (lambda (x) (+ x 1)) + '(1 2 3))

9
```

This works like this:

* start with the list `'(1 2 3)`
* apply the lambda which adds 1 to each element, leaving `'(2 3 4)`
* reduce to a single number `9` using `+` with the initial number `0`

### each-pair

Calls a supplied lambda with each consecutive pair in a list.

```scheme
; Pair iteration where each pair 1 5, 3 3 and 4 2 all sum to 6
(each-pair '(1 5 3 3 4 2)
    (λ (a b)
        (assert (= 6 (+ a b)))))
```

### matrix functions

```scheme
; Multidimensional list (matrix) access
(var M '(((10 11 12) (13 14 15)) ((16 17 18) (19 20 21))))
(assert (= 20 (matrix-at M 1 1 1)))
(assert (= nil (matrix-at M 1 1 100)))
(assert (= '(10 11 12) (matrix-at M 0 0)))

(var M2 '( (1 2 3 4) (a b c d)))
(assert (= 'c (matrix-at M2 1 2)))

; Update matrix; the old value is returned
(assert (= 'c (matrix-set! M2 'x 1 2)))
(assert (= 'x (matrix-at M2 1 2)))

; Trying to set a value outside the matrix returns an error
(assert (error? (try (matrix-set! M2 0 200 2) #t #!)))
```

### hashmap

The are numerous hashmap related functions available:

```scheme
(var mymap (hashmap.new ("1" 2) (3 4)))
(assert (hashmap? mymap))
(assert (= (len mymap) 2))
(hashmap.put mymap 5 6)
(hashmap.put mymap 7 "Initial entry")
(var initial-entry (hashmap.put mymap 7 "Another entry"))
(assert (= initial-entry "Initial entry"))
(assert (= (len mymap) 4))
(assert (= (hashmap.get mymap 7) "Another entry"))
(hashmap.remove mymap 7)
(assert (= (hashmap.get mymap 7) nil))
(assert (= (len mymap) 3))

(var keys '())
(var vals '())
(hashmap.iterate mymap (λ (k v)
    (item-append! keys k)
    (item-append! vals v)
))
(assert (= '(1 3 5) keys))
(assert (= '(2 4 6) vals))

(var count-removed (hashmap.clear mymap))
(assert (= count-removed 3))
(assert (= (len mymap) 0))

; k -> '()
(var hmlist (hashmap.new))
(hashmap.append! hmlist 'a 1)
(hashmap.append! hmlist 'a 2)
(assert (= (hashmap.get hmlist 'a) '(1 2)))

; The 'a entry exists, so the lambda is called, which updates the list
(hashmap.put-or-apply hmlist 'a 3 (λ (list)
    (assert (= list '(1 2)))
    (item-append! list 3)
))

(assert (= (hashmap.get hmlist 'a) '(1 2 3)))
(assert (= (hashmap.maybe-put hmlist 'a '(5 5 5)) '(1 2 3)))
(assert (= (hashmap.maybe-put hmlist 'b '(5 5 5)) '(5 5 5)))

(assert (contains? hmlist 'a))
(assert (not (contains? hmlist 'not-there)))
```

### io.read-number

Read a number from stdin.

If input is not a number, an `error` expression is returned.

### typename

Returns the type name of its argument

```scheme
>(define x 5)
>(typename x)
number

>(typename 'x)
symbol
```

### time.now

Returns the current time in milliseconds since unix epoch:

```scheme
>(time.now)
1618413357184
```

### double-quote

Renders the argument and wraps the result in double-quotes.

```scheme
> (double-quote name)
"Joanna"

> (double-quote 5)
"5"

> (double-quote (+ 2 3))
"5"
```

### inc! and dec!

Increments and decrements

### file i/o and stdin/stdout

A file is opened with `io.open-file`, which takes a relative or absolute path name as an argument. The file is then closed with `io.close-file`. Currently, line oriented reading and writing is supported.

```scheme
(var report (io.open-file "report.csv"))
(io.read-line report)
(io.write-line report "a new line is appended")
(io.close-file report)
```

`io.read-line` returns the *error* expression "EOF" if end of the file is reached.

`io.read-line` and `io.write-line` without a file argument reads and writes to stdin and stdout respectively.

`io.read-byte` reads one byte at a time from a file.

### math.odd? math.even? odd-items and even-items

`math.odd?` and `math.even?` determine if a number is odd or even, respectively.

`odd-items` and `even-items` create a list with odd- and even indexed items respectively. These functions are 1 based.

```scheme
>(math.odd? 5)
#t

> (odd-items '(a b c d e f g))
(a c e g)
```

### math.abs

Returns the absolute value of the argument.

```scheme
(math.abs -17)
17
```

### math.pow

Calculates `x^y`:

```scheme
>(math.pow 2 32)
4294967296
```

### math.average

The average of a list of numbers

### math.sqrt

Calculate the square root using Newton's method

```scheme
>(math.sqrt 986)
31.40063693621559
```

### math.safe-div

Divides the first argument by the second. If the divisor is zero, the result is an error.

```scheme
(try (safe-div 6 2)
    (if (= #value 3)
        (print "As expected!\n")
    )
)
```

### math.make-random-generator and math.random-list

Given a seed, `math.make-random-generator` creates a linear congruent random number generator.

Example using current Unix epoch as seed:

```scheme
>(var rng (math.make-random-generator 0))

>(rng)
362807296

>(rng)
1965043776
```

Given a random number generator, `math.random-list` generates a list of `n` random numbers:

```scheme
>(var rng (math.make-random-generator 0))

>(math.random-list rng 10)
(1055406848 752570112 91411200 3016512 1968096889 765038592 339902464 1666232384 1888402176 197119744)
```

### math.fib and math.fact

Recursive Fibonacci and factorial functions (note: as Bio doesn't support arbitrary precision numbers yet, it can only handle relatively small inputs)

### Y

The Y fixpoint combinator (technically, the Z combinator as Bio is applicative)

```scheme
> (var ! (Y (lambda (r) (lambda (x) (if (< x 2) 1 (* x (r (- x 1))))))))
> (! 5)

120
```

## Modules
A Bio module is a module *by convention*, somewhat similar to classical Javascript modules:

1. A `mod-<modulename>.lisp` file with the contents wrapped in a lambda call
2. The last expression is `(self)`, making the environment available to the importer
3. The following definitions are available:
    - module-name, a string describing the module
    - module-version, a list of numbers signifying major, major, and patch
    - module-description, an *optional* description of the module

## Modules
A Bio module is a module *by convention*, somewhat similar to classical Javascript modules:

1. A `mod-<modulename>.lisp` file with the contents wrapped in a lambda call
2. The last expression is `(self)`, making the environment available to the importer
3. The following definitions are available:
    - module-name, a string describing the module
    - module-version, a list of numbers signifying major, major, and patch
    - module-description, an *optional* description of the module

### Module example

The examples directory contains a sample module called `mod-pos.lisp`

To use the module in a REPL:

```scheme
bio>  (var Point (import "examples/mod-pos.lisp"))
<env>

bio>  (var pt (Point (new-point 2 5.4)))
<env>

bio> (pt x)
2

bio> (pt y)
5.4

bio> (pt (as-string))
2 5.4

bio> (var loc (Point (new-location 100.5 200.5)))
<env>

bio> (loc x)
100.5

bio> (loc y)
200.5

bio> (loc (as-string))
100.5° N  200.5° E

```
