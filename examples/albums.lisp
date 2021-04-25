; The database is a list where each entry is a ("name" rating) pair
; These are serialized to file as Bio expressions, and then replayed
; on startup.
(var albums '())

(var print-menu (lambda ()
    (print "1: Add    2: Find by name    3: List all    4: Exit\n")
))

(var print-heading (λ (heading)
    (print "\n------------------------------------\n")
    (print heading "\n")
    (print "------------------------------------\n")
))

; Add new album to database
(var album.add (lambda ()
    (print "\nName of album: ")
    (var album-name (readline))

    (print "Rating: ")
    (var rating (as number (readline)))

    (set! albums (append albums (list (list album-name rating))))
    (print "\nAlbum was added!\n")

    ; Append to album database file
    (var dat (io.open-file "albums.dat"))
    (io.write-line dat (string `(set! albums (append albums (list (list ,(double-quote album-name) ,rating))))))
    (io.close-file dat)
))

; List all albums
(var album.list (lambda ()
    (if (> (len albums) 0)
        (begin
            (print-heading "All albums")
            (var sorted-album-list (quicksort albums (lambda (first second)
                ; Compare album names
                (< (car first) (car second))
            )))
            (each sorted-album-list (lambda (album)
                (album.print-entry album)
            ))
        )
        (print "\nThere are no albums in the database\n")
    )
    (print "\n")
))

; Find an album by name
(var album.find (lambda ()
    (print "Album name: ")
    (var to-find (readline))
    (each albums (lambda (album)
        (if (= to-find (car album))
            (begin
                (print-heading "Found a matching album:")
                (album.print-entry album)
            )
        )
    ))
    (print "\n")
))

(var album.print-entry (lambda (album)
    (print "Name   : " (car album) "\n")
    (print "Rating : " (cadr album) "\n")
    (print "------------------------------------\n")
))

(print "\nWelcome to the Bio Album Database\n \n")

; Read albums from the database file until EOF
(var dat (io.open-file "albums.dat"))
(var keep-reading #t)
(while keep-reading
    (try (io.read-line dat)
        (eval-string #value)
        (set! keep-reading #f)
    )
)
(io.close-file dat)

; Menu loop
(while #t
    (print-menu)
    (let ((action (io.read-number)))
        (if (= action 1) (album.add))
        (if (= action 2) (album.find))
        (if (= action 3) (album.list))
        (if (= action 4) (exit 0))
    )
)
