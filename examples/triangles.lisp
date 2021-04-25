(var sierpinski (lambda (n)
    (var generator (lambda (acc spaces n)
        (if (= 0 n)
            acc
            (generator
                (append
                    (map (lambda (x) (append spaces x spaces)) acc)
                    (map (lambda (x) (append x (list " ") x)) acc))
                (append spaces spaces)
                (- n 1)
            )
        )
    ))

    (map
        (lambda (x) (print (as symbol x) "\n"))
        (generator (list "^") (list " ") n)
    )
))

; Prints a Sierpinksi Triangle of size 5; change size at your leisure
(sierpinski 5)
