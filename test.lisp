; Run tests with: bio run test.lisp

; Scope everything in a lambda so we can evaluate this file multiple times in a REPL session.
; This also how Bio modules are made: see the last three lines of the lambda.

((lambda ()
    (var a 5)
    (var nums-empty '())
    (var nums-one '(1))
    (var nums '(1 2 3 4))
    (var nums-odd-length '(1 2 3 4 5))
    (var letters '(a b c d e))
    (var pairlist '((a b) (c d) (e f)))

    (assert (= nil (math.middle-item nums-empty)))
    (assert (= 1 (math.middle-item nums-one)))
    (assert (= 3 (math.middle-item nums)))
    (assert (= 3 (math.middle-item nums-odd-length)))

    (assert (= 2 (indexof nums 3)))
    (assert (= nil (indexof nums 100)))

    (assert (= 2 (math.min '(5 3 9 2 4 2 5 6))))
    (assert (= 9 (math.max '(5 3 9 2 4 9 5 6))))

    (var nums-iterated '())
    (list.iterate nums (λ (val)
        (item-append! nums-iterated val)
    ))
    (assert (= nums nums-iterated))

    (var thing "thing")
    (var thing-as-list '())
    (sym.iterate thing (λ (byte)
        (item-append! thing-as-list byte)
    ))
    (assert (= thing-as-list '(t h i n g)))

    (var updateble '(1 2 3))
    (item-apply! 2 updateble + 10)
    (assert (= '(1 2 13) updateble))
    (assert (= '(13 2 1) (reverse! updateble)))
    (assert (= '(2 1 13) (rotate-left! updateble 1)))
    (assert (= '(2 1 12) (replace-first! updateble 13 12)))
    (assert (= '(2 1 12) (replace-first! updateble 'not-there 12)))
    (set! updateble '(2 2 2))
    (assert (= '(3 3 3) (replace-all! updateble 2 3)))
    (set! updateble '(1 2 3))
    (assert (= 2 (item-remove! 1 updateble)))
    (assert (= '(1 3) updateble))

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
    (assert (= 100 (nth-orelse 50 nums 100)))
    (assert (nil? (nth 100 nums)))
    (assert (nil? (nth -1 nums)))
    (assert (= 1 (math.pow 2 0)))
    (assert (= 2 (math.pow 2 1)))
    (assert (= 4 (math.pow 2 2)))
    (assert (= 13 (bitset-to-number '(1 1 0 1))))
    (assert (= 13 (bitset-to-number '(7 8 0 9))))

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
    (assert (= '(1 2 3 4 5) `(,@(list 1 2 3 4) ,a)))
    (assert (= '(1 2 3 4 5) `(,@ nums ,a)))
    (assert (= '(1 2 3 4 5) `(,@ (list 1 2 3 4) ,a)))

    (assert (= '(1 2 3) (cons '1 '(2 3))))
    (assert (=  '(j k l) (cons 'j '(k l))))
    (assert (= '(1 2 3) (append '(1 2) '3)))
    (assert (= '(1 2 3 4) (append '(1 2) '(3 4))))
    (assert (= '(1 2 3 4) (append (list 1 2) '(3 4))))
    (assert (= '(a b 1 2 3) (append 'a 'b 1 2 (+ 1 2))))
    (assert (= '(a b c d e f (g h)) (append '(a b c) '(d e f (g h)))))
    (assert (= '(1 9 3 4) (replace-or-append '(1 2 3 4) 1 9)))
    (assert (= '(1 2 3 4 9) (replace-or-append '(1 2 3 4) 4 9)))
    (assert (= '(1 1 1 1 1) (listof 1 5)))

    (assert (= 9 (reduce-with 0 (lambda (x) (+ x 1)) + '(1 2 3))))

    ; Destructuring
    (var stuff '(1 2 3))
    (vars a b c stuff)
    (assert (= a 1))
    (assert (= b 2))
    (assert (= c 3))

    ; Mutation
    (var original '(1 2 3))
    (var new-list (copy-list original))
    (append &mut original '(4))
    (assert (= '(1 2 3 4) original))
    (assert (= '(1 2 3) new-list))
    (item-append! original 5)
    (assert (= '(1 2 3 4 5) original))

    ; Reference equality
    (var same-1 '(1 2 3))
    (var same-2 '(1 2 3))
    (assert (= same-1 same-2))
    (assert (not (^= same-1 same-2)))
    (assert (^= same-1 same-1))

    ; Range
    (assert (= 'a (range letters)))
    (assert (= '(e) (range letters -1)))
    (assert (= 'e (range (range letters -1))))
    (assert (= '(c d e) (range letters 2)))
    (assert (= '(c d) (range letters 2 4)))
    (assert (= '(b c) (range letters -4 -2)))
    (assert (= '(a b c d) (range letters 0 -1)))
    (assert (nil? (range letters 5)))
    (assert (nil? (range letters 0 0)))

    (assert (= 'a (caar pairlist)))
    (assert (= '(c d) (cadr pairlist)))
    (assert (= '((e f)) (cddr pairlist)))
    (assert (= '(e f) (caddr pairlist)))

    ; Ascii
    (assert (lowercase? 'abc))
    (assert (lowercase? (lowercase 'ABC)))
    (assert (uppercase? 'ABC))
    (assert (uppercase? (uppercase 'abc)))

    ; Indexed lookup
    (assert (= 2 (item-at 1 '(1 2 3))))
    (assert (= nil (item-at 100 '(1 2 3))))
    (assert (= nil (item-at -1 '(1 2 3))))

    ; In-place mutation
    (var inplace '(100 2 3))
    (var old (item-set 0 inplace 1))
    (assert (= '(1 2 3) inplace))
    (assert (= old 100))

    (item-set 4 inplace 4)
    (assert (= '(1 2 3 4) inplace))

    (item-set -1 inplace 13)
    (assert (= '(13 1 2 3 4) inplace))

    ; In-place append modifies the first argument directly rather than making a new list
    (set! inplace '(1 2 3))
    (append &mut inplace 4)
    (assert (= '(1 2 3 4) inplace))

    ; Logical functions
    (assert (or #f #t))
    (assert (or #t #f))
    (assert (or #t #t))
    (assert (not (or #f #f)))
    (assert (and #t #t))
    (assert (not (and #t #f)))
    (assert (not (and #f #t)))
    (assert (and #t #t #t))

    ; Arithmetic
    (assert (= (- 5 1) 4))
    (assert (= (+ 1 2 3) 6))
    (assert (= (* 1 2 3) 6))
    (assert (= (/ 6 2 2) 1.5))
    (let ((a 1)) (assert (= (+= a 1 2 3) 7)))
    (let ((a 9)) (assert (= (-= a 1 2 3) 3)))
    (let ((a 1)) (assert (= (*= a 1 2 3) 6)))
    (let ((a 9)) (assert (= (/= a 3 2) 1.5)))

    ; Approximate equality works for both numbers (relative tolerance) and symbols (case)
    (assert (~= 0.0000001 0.000000099 0.05))
    (assert (~= 0.0000001 0.00000009999999))
    (assert (~= "abc" "ABC"))

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
    (assert (= '(a b c) (atom.split 'abc)))
    (assert (= (list 'a " " 'b 'c) (atom.split "a bc")))

    ; Pop
    (var poppable '(1 2 3))
    (assert (= 3 (pop-last! poppable)))
    (assert (= '(1 2) poppable))
    (assert (= 1 (pop-first! poppable)))
    (assert (= '(2) poppable))
    (assert (= 2 (pop-first! poppable)))
    (assert (= '() poppable))
    (assert (= nil (pop-first! poppable)))
    (assert (= '() poppable))

    ; Swap
    (var s1 '(1 2))
    (var s2 '(3 4))
    (env.swap! s1 s2)
    (assert (= s1 '(3 4)))
    (assert (= s2 '(1 2)))

    ; Containment
    (var containment-map (hashmap.new ("1" 2) (3 nil)))
    (var containment-list '(1 2 nil))
    (var containment-sym 'demo)
    (assert (contains? containment-map "1"))
    (assert (contains? containment-map 3))
    (assert (not (contains? containment-map 4)))
    (assert (contains? containment-list 1))
    (assert (contains? containment-list nil))
    (assert (not (contains? containment-list 3)))
    (assert (contains? containment-sym 'e))
    (assert (not (contains? containment-sym 'f)))

    ; Hashmap
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

    ; Immediate lambda application
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

    ; Test type and set!!
    (type SomeType () (var local 5))
    (var some-instance (SomeType))
    (var callee-local 6)
    (set!! some-instance local callee-local)
    (assert (= 6 (some-instance local)))
    
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

    (var sum 0)
    (n-times-with 5 (λ (index) (+= sum index)))
    (assert (= sum 10))

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

    ; A failed expression without an failure branch evaluates to nil
    (assert (nil? (try (map + '1) #t)))

    ; Test tokenizing
    (assert (= (string.split "" ",") '()))
    (assert (= (string.split "a" ",") '(a)))
    (assert (= (string.split "a,b,c" ",") '(a b c)))
    (assert (= (string.split "a,b,c," ",") '(a b c)))
    (assert (= (string.split "a,b;c," ",;") '(a b c)))
    (assert (= (string.split " " ",") (list " ")))

    ; Pair iteration where each pair sums to 6
    (each-pair '( 1 5 3 3 4 2) (λ (a b)
        (assert (= 6 (+ a b) ))
    ))

    ; Test reading file by opening ourself and reading the first byte
    (var file (io.open-file "test.lisp"))
    (assert (= (io.read-byte file) ";"))
    (io.close-file file)

    ; Var without value is nil by default
    (var novalue)
    (assert (= novalue nil))

    ; Test 0..10
    (var loop-count 0)
    (loop '(0 10) (+= loop-count 1))
    (assert (= loop-count 10))

    ; Test &break
    (set! loop-count 0)
    (loop '(0 10) (+= loop-count 1) (if (= loop-count 5) &break nil))
    (assert (= loop-count 5))

    ; Test &break
    (set! loop-count 0)
    (loop '() (+= loop-count 1) (if (= loop-count 100) &break nil))
    (assert (= loop-count 100))

    ; Multidimensional list (matrix) access
    (var M '( ( (10 11 12) (13 14 15) )
              ( (16 17 18) (19 20 21) )))
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

    (print "Tests passed\n")

    (var module-name "Test Module")
    (var module-version '(1 0))
    (self)
))