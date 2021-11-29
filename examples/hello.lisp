(print "How many times to print Hello?\n")

; Attempt to read a number from stdin. If io.read-number fails, the error messages
; is available in #!, which we print.
(try (var count (io.read-number))
    (n-times count
        (print "Hello\n")
    )
    (print "Could not read number of times:" #! "\n")
)
