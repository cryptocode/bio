; Prints a list of random numbers, using current epoch milliseconds as seed
(var math (Math))
(var count 100)
(print (math (random-list (math (make-random-generator (std-time-now))) count)) "\n")
