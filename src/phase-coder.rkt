;;;;;;;;;;;;;;;;;;
;;; Given a complex number (representing a sample in the frequency domain) and a bit to encode,
;;; return a new sample with the bit encoded
;;; frequency - the frequency of which to encode the given bit into
;;; bit - the bit to encode

(define (get-shifted-frequency frequency bit)
    (let [(shift (get-shift bit))]
         (make-polar (magnitude frequency) shift)))
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
   (if (< (abs (angle frequency)) (+ zero-shift 1e-2))
       0
       1))
