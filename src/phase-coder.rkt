;;; The amount to shift when encoding a 0
(define zero-shift (- 0 (/ pi 2)))
;;; The amount to shift when encoding a 1
(define one-shift (/ pi 2))

;;;;;;;;;;;;;;;;;;
;;; Given a complex number (representing a sample in the frequency domain) and a bit to encode,
;;; return a new sample with the bit encoded
;;; frequency - the frequency of which to encode the given bit into
;;; bit - the bit to encode

(define (get-shifted-frequency frequency bit)
    (make-polar (magnitude frequency) (get-shift bit)))
;    (let [(ang (- (mod (+ (angle frequency) pi (get-shift bit)) (* 2 pi)) pi))]
;         (display frequency) (display "     ") (display (make-polar (magnitude frequency) ang))(newline)
;         (make-polar (magnitude frequency) ang)))


;;;;;;;;;;;;;;;;;;
;;; Given a bit, return the amount to shift the phase
;;; bit - the bit to encode

(define (get-shift bit)
    (if (= bit 0)
        zero-shift
        one-shift))

;;;;;;;;;;;;;;;;;;
;;; Given a frequency, return what bit is encoded in it
;;; frequency - the frequency to pull an encoded bit out of

(define (get-bit-from-frequency frequency)
   (if (= (angle frequency) one-shift)
       1
       0))
