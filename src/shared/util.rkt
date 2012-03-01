;Given a byte string bs, determine if bs represents a .wav file
(define (is-wav? bs)
    (let ((check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
        (and (check 0 #\R)
            (check 1 #\I)
            (check 2 #\F)
            (or (check 3 #\F) (check 3 #\X))
            (check 8 #\W)
            (check 9 #\A)
            (check 10 #\V)
            (check 11 #\E))))

;Given a byte string bs, determine if bs represents a .mp3 file
;NOTE: This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
(define (is-mp3? bs)
    (let ((check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
        (and (check 0 #\I)
            (check 1 #\D)
            (check 2 #\3))))
