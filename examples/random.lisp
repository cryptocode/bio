; Prints a list of random numbers, using current epoch milliseconds as seed
(var count 100)
(print (math.random-list (math.make-random-generator (time.now)) count) "\n")
