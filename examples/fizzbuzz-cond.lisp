; FizzBuzz using cond - see fizzbuzz-if.lisp for an improvement
(var fizzbuzz (lambda (i n)
    (if (<= i n)
        (let ((x (math.mod i 3)) (y (math.mod i 5)))
            (cond
                ((and (= 0 x) (= 0 y)) (print "FizzBuzz" "\n"))
                ((= 0 x) (print "Fizz" "\n"))
                ((= 0 y) (print "Buzz" "\n"))
                ((print i "\n"))
            )
        (fizzbuzz (+ i 1) n))
    )
))

(fizzbuzz 1 20)
