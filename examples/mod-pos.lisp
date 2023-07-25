; You can test this module from the REPL with:
;
;    (var Point (import "examples/mod-pos.lisp"))
;
; and then calling the modules' test function:
;
;    (Point (test))
;
; which should print:
;
;    New York     : 40.7554351° N  73.9981619° W
;    Bejing       : 39.9390731° N  116.1172802° E
;    Buenos Aires : 34.5777632° S  58.409201° W
;    Sidney       : 33.8568285° S  151.2130914° E
;    Sidney zeroed: 0° N  0° E
;
; Making a new point can be done with:
; (var pt (Point (new-point 2 5.4)))
;
; To access a member:
; (pt y)
; 5.4
;
; Two ways to call member functions:
; ((pt as-string))
; 2 5.4
; (pt (as-string))
; 2 5.4

((lambda ()

    ; Both point types adopt this as their "update" function, but each
    ; point type have their own implementation of the "as-string" function.
    ; This is a macro, so x and y is available in the environment.
    (var generic-update (macro (new-x new-y)
        (set! x new-x)
        (set! y new-y)
        nil
    ))

    ; A point datatype with regular formatting
    (var new-point (lambda (x y)
        (var update generic-update)
        (var as-string (lambda ()
            (string x " " y)
        ))
        (self)
    ))

    ; A version with longitude and latitude formatting
    (var new-location (lambda (x y)
        (var update generic-update)
        (var as-string (lambda ()
            (string
                (math.abs x) (if (< x 0) "° S" "° N")
                "  "
                (math.abs y) (if (< y 0) "° W" "° E"))
        ))
        (self)
    ))

    (var test (lambda ()
        (var new-york (new-location 40.7554351 -73.9981619))
        (var bejing (new-location 39.9390731 116.1172802))
        (var buenos-aires (new-location -34.5777632 -58.409201))
        (var sidney (new-location -33.8568285 151.2130914))

        (assert (= ((new-york as-string)) "40.7554351° N  73.9981619° W"))
        (assert (= ((bejing as-string)) "39.9390731° N  116.1172802° E"))
        (assert (= ((buenos-aires as-string)) "34.5777632° S  58.409201° W"))
        (assert (= ((sidney as-string)) "33.8568285° S  151.2130914° E"))

        (print "New York     :" ((new-york as-string)) "\n")
        (print "Bejing       :" ((bejing as-string)) "\n")
        (print "Buenos Aires :" ((buenos-aires as-string)) "\n")
        (print "Sidney       :" ((sidney as-string)) "\n")

        (sidney (update 0 0))
        (print "Sidney zeroed:" ((sidney as-string)) "\n")
    ))

    (var module-name "Position Module")
    (var module-version '(1 0 0))
    (self)
))
