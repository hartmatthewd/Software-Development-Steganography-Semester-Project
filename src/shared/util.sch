;Given a byte string bs, determine if bs is from a .wav file
(define (is-wav? bs)
    (let ((check (lambda (index value) (= (bytes-ref bs index) value))))
        (and (check 0 82)   ;R
            (check 1 73)    ;I
            (check 2 70)    ;F
            (check 3 70)))) ;F
