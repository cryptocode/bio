; ---------------------------------------
;        The Bio standard library
; ---------------------------------------

; First item of a list. Note that calling range without arguments returns
; the first item of a list, or nil if the list is empty.
(var car (lambda (list) (range list)))

; If range gets only one argument, the returned list goes to the end
(var cdr (lambda (list) (range list 1)))
(var caar (lambda (list) (car (car list))))
(var cadr (lambda (x) (car (cdr x))))
(var cddr (lambda (x) (cdr (cdr x))))
(var caddr (lambda (x) (car (cddr x))))

(var nth-orelse (lambda (index list default)
    (var item (item-at index list))
    (if item item default)
))

; The n'th item in a list, or nil if out of bounds
(var nth (lambda (index list)
    (item-at index list)
))

; Negative range indices means "from the end"
(var last (lambda (list) (car (range list (- 1)))))

; Prepend item to list
;     (cons 'a '(b c))     -> '(a b c)
;     (cons '(a b) '(c d)) -> '((a b) c d)
(var cons (lambda (new existing-list) (append (list new) existing-list)))

; Interpret a list as digits in a binary number, convert to number. Any non-zero
; list item is interpreted as 1 so '(12 0 1 0 0 23) is 1 0 1 0 0 1 = 41
(var bitset-to-number (lambda (list)
    (var res 0)
    (n-times-with (len list) (lambda (n)
        (var index (- 0 (+ n 1)))
        (var item (range list index))
        (if (> (car item) 0) (+= res (math.pow 2 n)))
    ))
    res
))

(var nil? (lambda (x) (= nil x)))
(var atom? (lambda (x) (if (or (number? x) (symbol? x)) #t #f)))
(var bool? (lambda (x) (if (or (= #t x) (= #f x)) #t #f)))

(var <= (lambda (x y) (if (or (< x y) (= x y) ) #t #f)))
(var >= (lambda (x y) (if (or (> x y) (= x y) ) #t #f)))
(var < (lambda (x y) (if (= (order x y) (- 1)) #t #f)))
(var > (lambda (x y) (if (= (order x y) 1) #t #f)))
(var != (lambda (x y) (not (= x y))))
(var not (lambda (x) (if x #f #t)))
(var += (macro (/expr1 &rest /expr2) `(set! ,/expr1 (+ ,/expr1 ,@/expr2))))
(var -= (macro (/expr1 &rest /expr2) `(set! ,/expr1 (- ,/expr1 ,@/expr2))))
(var *= (macro (/expr1 &rest /expr2) `(set! ,/expr1 (* ,/expr1 ,@/expr2))))
(var /= (macro (/expr1 &rest /expr2) `(set! ,/expr1 (/ ,/expr1 ,@/expr2))))

; Logical or with shortcut evaluation. Identifiers starting with / is
; a convention for macro parameters to help avoid name clashes.
(var or (macro (/expr1 /expr2)
    `(if ,/expr1
        #t
        (if ,/expr2 #t #f)
    )
))

; Logical and with shortcut evaluation
(var and (macro (/expr1 /expr2)
    `(if ,/expr1
        (if ,/expr2 #t #f)
        #f
    )
))

; The let macro, which makes a new scope with an arbitrary number of local bindings
; The transformation goes like this:
;
;   (let ((a 5) (b 6)) (print (* a b)))
;       => ((lambda (a b) (print (* a b))) 5 6)
;       => 30
;
; The let body can have multiple expressions without using (begin) due to the &rest sentinel
(var let (macro (/binding-pairs &rest /body)
    (var params '())
    (var args '())

    (each /binding-pairs (lambda (item)
        (set! params (append params (list (car item))))
        (set! args (append args (list (eval (cadr item)))))
    ))

    `((lambda (,@params) ,@/body) ,@args)
))

; The while macro expands to a tail-recursive lambda
(var while (macro (/predicate &rest /body)
    (var loop-name (gensym))
    `(begin
        (var ,loop-name (lambda ()
            (if ,/predicate
                (begin
                    ,@/body
                    (,loop-name)
                )
            )
        ))
        (,loop-name)
    )
))

; The n-times macro expands to a tail-recursive lambda
(var n-times (macro (/n &rest /body)
    (var loop-name (gensym))
    (var countdown-var (gensym))
    `(begin
        (var ,countdown-var ,/n)
        (var ,loop-name (lambda ()
            (if (> ,countdown-var 0)
                (begin
                    ,@/body
                    (dec! ,countdown-var)
                    (,loop-name)
                )
            )
        ))
        (,loop-name)
    )
))

(var n-times-with (lambda (times fn)
    (var count 0)
    (loop (list 0 times)
        (fn count)
        (+= count 1)
    )
))

; Replaces an item in a list at a given index, returning a new list.
; If the index is past the end of the list, a new item is appended.
(var replace-or-append (λ (list index value)
    (item-set index list value)
    list
    ;(append (range list 0 index) value (range list (+ index 1)))
))

(var each (lambda (lst fn)
    (loop 'index (list 0 (len lst))
        (fn (item-at index lst))
    )
))

(var each-pair (lambda (lst fn)
    (if (not (nil? (car lst)))
        (begin
            (fn (car lst) (car (cdr lst)))
            (if (not (nil? (cdr (cdr lst))))
                (each-pair (cdr (cdr lst)) fn)
                nil
            )
        )
        nil
    )
))

; Creates a new list containing the unfiltered expressions of the input list
; (filter (lambda (x) (< x 5)) '(3 9 5 8 2 4 7))) => (3 2 4)
(var filter (lambda (lst pred)
        (if (nil? lst)
            '()
            (if (pred (car lst))
                (cons (car lst) (filter (cdr lst) pred))
                (filter (cdr lst) pred)
            )
    )
))

; Calls the supplied function with arguments collected from multiple lists
; Fails with an error a list is not passed as the second argument
;   (map + '(0 2 5) '(1 2 3) '(1 2 3)) => (2 6 11)
;   (map (λ (x) (* 2 x)) '(1 2 3))     => (2 4 6)
(var map (λ (f ls &rest more)
    (if (not (list? ls))
        (error (string "Expected a list, received " (typename ls)))
        (if (nil? more)
            (begin
                (var map-one (λ (ls)
                    (if (nil? ls)
                        '()
                        (cons
                            (f (car ls))
                            (map-one (cdr ls)))
                    )
                ))
                (map-one ls)
            )

            (begin
                (var map-more (λ (ls more)
                    (if (nil? ls)
                        '()
                        (cons
                            (apply f (car ls) (map car more))
                            (map-more (cdr ls) (map cdr more)))
                    )
                ))
                (map-more ls more)
            )
        )
    )
))

; Sorts a list in an order according to the comparator, (quicksort < '(5 40 1 -3 2))  => (-3 1 2 5 40)
(var quicksort (λ (lst comparator)
    (if (nil? lst)
        nil
        (let ((pivot (car lst)))
            (append (quicksort (filter (cdr lst) (λ (n) (comparator n pivot))) comparator)
                    (list pivot)
                    (quicksort (filter (cdr lst) (λ (n) (not (comparator n pivot)))) comparator)
            )
        )
    )
))

; Get the items at odd locations, (1 8 12 14 19) -> (1 12 19)
(var odd-items (lambda (lst) (modulo-items lst !=)))

; Get the items at even locations, (1 8 12 14 19) -> (8 14)
(var even-items (lambda (lst) (modulo-items lst =)))

; Helper for odd-items and even-items
(var modulo-items (lambda (lst op)
    (var index 1)
    (var res '())
    (each lst (lambda (item)
                  (if (eval (op 0 (math.mod index 2)))
                    (set! res (append res item))
                    nil
                  )
                  (set! index (+ index 1))
              )
    )
    res
))

; Read a number from stdin, returns an error if input is not a number
(var io.read-number (lambda ()
    (var input (io.read-line))
    (var n (as number input))
    (if (nil? n)
        (error (as symbol (list input " is not a number")))
        n)
))

; Increment variable
(var inc! (macro (/var)
    `(set! ,/var (+ ,/var 1))
))

; Decrement variable
(var dec! (macro (/var)
    `(set! ,/var (- ,/var 1))
))

(var math.mod (lambda (num div) (- num (* div (math.floor (/ num div))))))

; Division that emits an error for zero denominators
(var math.safe-div (lambda (x y)
    (if (= y 0)
        (error "Division by zero")
        (/ x y)
    )
))

; Absolute value of x
(var math.abs (lambda (x)
	(if (< x 0)
		(- x)
		x)
))

; Average of a list of numbers, 0 if the list is empty
(var math.avg (lambda (x)
    (try (math.safe-div (apply + x) (len x)) #value 0)
))

; Squares the input
(var math.square (lambda (x) (* x x)))

; Compute sqrt using Newton's method
(var math.sqrt (lambda (x)
    (var good-enough? (lambda (guess)
        (< (math.abs (- (math.square guess) x)) 0.0001)
    ))

    (var improve (lambda (guess)
        (math.avg (list guess (/ x guess)))))

    (var solve (lambda (guess)
        (if (good-enough? guess)
            guess
            (solve (improve guess)))))

    (solve 1.0)
))

(var math.odd? (lambda (x) (!= 0 (math.mod x 2))))
(var math.even? (lambda (x) (= 0 (math.mod x 2))))

; n'th number in the fibonacci sequence. Note that the "math." prefix is just a namespacing convention.
(var math.fib (lambda (n)
    (cond
        ((= n 0) 0)
        ((= n 1) 1)
        ((+ (math.fib (- n 1)) (math.fib (- n 2))))
    )
))

; Compute n!
(var math.fact (lambda (n)
    (if (= n 0)
        1
        (* n (math.fact (- n 1)))
    )
))

; Returns a linear congruent RNG, using the BSD libc formula
(var math.make-random-generator  (lambda (seed)
    (λ ()
        (set! seed (math.mod (+ (* 1103515245 seed) 12345) 2147483648)
        )
        seed
    )
))

; Given an RNG generator, generate a list of n random numbers
(var math.random-list (lambda (generator n) (if (= 0  n) '() (cons (generator) (math.random-list generator (- n 1))))))

; The Y combinator and a version of factorial using it
(var Y (lambda (f) ((lambda (g) (g g)) (lambda (g) (f (lambda (a) ((g g) a)))))))
(var math.fact-y (Y (lambda (r) (lambda (x) (if (< x 2) 1 (* x (r (- x 1))))))))

; π alias
(var π math.pi)

; Given an expression, return the type name
(var typename (lambda (x)
    (cond
        ((list? x) "list")
        ((bool? x) "bool")
        ((number? x) "number")
        ((symbol? x) "symbol")
        ((callable? x) "function")
        ("unknown")
    )
))

(var module-name "Bio Standard Library")
(var module-version '(0 1 0))
(var version (λ () '(Major 0 Minor 1 Patch 0)))

'Done
