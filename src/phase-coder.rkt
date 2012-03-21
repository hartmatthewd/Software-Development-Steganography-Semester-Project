(define one-shift (/ pi 2))
(define zero-shift (- 0 (/ pi 2)))

;;; Given a complex number (representing a sample in the frequency domain) and a bit to encode,
;;; return a new sample with the bit encoded

(define (get-shifted-value orig bit)
    (let [(ang (- (mod (+ (angle orig) pi (get-shift bit)) (* 2 pi)) pi))]
;         (display orig) (display "     ") (display (make-polar (magnitude orig) ang))(newline)
         (make-polar (magnitude orig) ang)))


;;; Given a bit, return the amount to shift the phase

(define (get-shift bit)
    (if (= bit 0)
        zero-shift
        one-shift))


(define (get-bit-from-frequency frequency)
;    (display frequency)(newline)
    (if (= (angle frequency) one-shift)
        1
        0))
