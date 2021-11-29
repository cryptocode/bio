; Run tests with: bio run test.lisp

; Scope everything in a lambda so we can evaluate this file multiple times in a REPL session.
; This also how Bio modules are made: see the last three lines of the lambda.

((lambda ()
    (var a 5)
    (var nums '(1 2 3 4))
    (var letters '(a b c d e))
    (var pairlist '((a b) (c d) (e f)))

    (assert (atom? 'a))
    (assert (number? 5))
    (assert (number? a))
    (assert (list? '()))
    (assert (list? '(1 2 3)))
    (assert (list? nums))
    (assert (error? (math.safe-div 1 0)))

    (assert (= 7 (eval (+ 2 5))))
    (assert (= 7 (eval '(+ 2 5))))
    (assert (= 7 (eval `(+ 2 ,a))))
    (assert (= '(+ 2 5) (eval '(list '+ 2 a))))
    (assert (= 7 (math.abs -7)))
    (assert (= 3 (nth 2 nums)))
    (assert (nil? (nth 100 nums)))
    (assert (nil? (nth -1 nums)))

    (assert (= '(a 5) `(a ,a)))
    (assert (= '() nil))
    (assert (= 1 (car nums)))
    (assert (= 2 (cadr nums)))
    (assert (= 3 (caddr nums)))
    (assert (= 4 (last nums)))
    (assert (= '(1 2 3 4) nums))
    (assert (= '(1 3) (odd-items nums)))
    (assert (= '(2 4) (even-items nums)))
    (assert (= '(1 2 3 4 5) `(,@nums ,a)))
    (assert (= '(1 2 3) (cons '1 '(2 3))))
    (assert (=  '(j k l) (cons 'j '(k l))))
    (assert (= '(1 2 3) (append '(1 2) '3)))
    (assert (= '(1 2 3 4) (append '(1 2) '(3 4))))
    (assert (= '(1 2 3 4) (append (list 1 2) '(3 4))))
    (assert (= '(a b 1 2 3) (append 'a 'b 1 2 (+ 1 2))))
    (assert (= '(a b c d e f (g h)) (append '(a b c) '(d e f (g h)))))

    (assert (= 'a (range letters)))
    (assert (= '(e) (range letters -1)))
    (assert (= 'e (range (range letters -1))))
    (assert (= '(c d e) (range letters 2)))
    (assert (= '(c d) (range letters 2 4)))
    (assert (= '(b c) (range letters -4 -2)))
    (assert (nil? (range letters 5)))
    (assert (nil? (range letters 0 0)))

    (assert (= 'a (caar pairlist)))
    (assert (= '(c d) (cadr pairlist)))
    (assert (= '((e f)) (cddr pairlist)))
    (assert (= '(e f) (caddr pairlist)))

    ; Logical functions
    (assert (or #f #t))
    (assert (or #t #f))
    (assert (or #t #t))
    (assert (not (or #f #f)))
    (assert (and #t #t))
    (assert (not (and #t #f)))
    (assert (not (and #f #t)))

    ; Ordering
    (assert (= -1 (order '() '(1))))
    (assert (=  1 (order '(1) '())))
    (assert (=  0 (order '() '())))
    (assert (= -1 (order 0 1)))
    (assert (=  1 (order 1 0)))
    (assert (=  0 (order 1 1)))
    (assert (= -1 (order nil 1)))
    (assert (=  1 (order 5 nil)))
    (assert (=  0 (order nil nil)))
    (assert (= -1 (order '(1 2) '(0 1 2))))
    (assert (=  1 (order '(1 2 3) '(0 1 2))))
    (assert (=  0 (order '(1 2 3) '(1 2 3))))

    ; Conversions
    (assert (symbol? (as symbol 5)))
    (assert (symbol? (as symbol 'already-symbol)))
    (assert (number? (as number "5")))
    (assert (number? (as number 5)))
    (assert (list? (as list 5)))
    (assert (= '(5) (as list 5)))
    (assert (= '(5) (as list '(5))))

    ; Lambda application
    (assert (= 11 ((λ (a b) (+ a b)) 5 6)))

    ; If with nesting
    (define w 10)
    (assert (= 0 (if (< w 20) 0 1)))
    (assert (= 1 (if (< w 20) (if (< 5 0) 0 1))))

    ; Begin
    (assert (= 10 (begin 1 5 (* 2 5))))
    (assert (= 10 (begin 10)))

    ; Recursion
    (assert (= 120 (math.fact 5)))
    (assert (= 120 (math.fact-y 5)))
    (assert (= 0 (math.fib 0)))
    (assert (= 55 (math.fib 10)))

    ; Errors are falsy
    (assert (= #f (math.safe-div 4 0)))
    (assert (not (if (math.safe-div 4 0) #t #f)))
    (assert (if (math.safe-div 4 1) #t #f))

    ; Anything not falsy is true
    (assert (if 2 #t))

    ; Test correct scoping of unset!
    (assert (= 5 (begin (var x 5) (var local (lambda () (begin (var x 100) (unset! x)))) (local) x)))

    ; Test gensym
    (var gensym-test
        (macro ()
            (var varname (gensym))
            `(begin (var ,varname 5) (* 2 ,varname))
        )
    )
    (assert (= 30 (* 3 (gensym-test))))

    ; Test while macro
    (var counter 0)
    (while (< counter 100)
        (set! counter (+ counter 1))
    )
    (assert (= counter 100))

    ; Test n-times macro
    (var countdown 5)
    (n-times 5 (dec! countdown))
    (assert (= countdown 0))

    ; Test (each)
    (let ((i 0) (bag-of-numbers '(1 2 3 4 5)))
        (each bag-of-numbers (lambda (num)
            (set! i (+ i num))
        ))
        (assert (= i 15))
    )

    ; Test (filter)
    (assert (= '(3 2 4) (filter '(3 9 5 8 2 4 7) (λ (x) (< x 5)))))

    ; Test sorting a list in ascending and descending order
    (assert (= '(1) (quicksort '(1) <)))
    (assert (= '(-3 1 2 5 40) (quicksort '(5 40 1 -3 2) <)))
    (assert (= '(40 5 2 1 -3) (quicksort '(5 40 1 -3 2) >)))
    (assert (= '(1 2 5 40) (filter (quicksort '(5 40 1 -3 2) <) (λ (x) (>= x 0)))))

    (assert (= '(2 6 11) (map + '(0 2 5) '(1 2 3) '(1 2 3))))
    (assert (= '(0 0 0) (map - '(1 2 3) '(1 2 3))))
    (assert (= '(1 2 3) (map + '(1 2 3))))

    ; Double every element in a list
    (assert (= '(2 4 6) (map (λ (x) (* 2 x)) '(1 2 3))))

    ; A list of pairs with the order reversed
    (assert (= '((1 a) (2 b) (3 c)) (map (λ (x y) (list y x)) '(a b c) '(1 2 3))))

    ; I failed expression without an failure branch evaluates to nil
    (assert (nil? (try (map + '1) #t)))

    (print "Tests passed\n")

    ; You can load and use this file as a module with:
    ;   (var mod-test (load "test.lisp"))
    ;   (print "Module name   :" (. mod-test module-name))
    ;   (print "Major version :" (car (. mod-test module-version)))
    ;   (. mod-test counter)
    (var module-name "Test Module")
    (var module-version '(1 0))
    (self)
))