;;;;;;;;;;;;;;;;;;
;;; Given a complex number (representing a sample in the frequency domain) and a bit to encode,
;;; return a new sample with the bit encoded
;;; frequency - the frequency of which to encode the given bit into
;;; bit - the bit to encode

(define (get-shifted-frequency frequency bit)
    (if (= bit 0)
        (make-polar (magnitude frequency) (+ (* phase-delta (round (- (/ (angle frequency) phase-delta) 0.5))) (/ phase-delta 2)))
        (make-polar (magnitude frequency) (* phase-delta (round (/ (angle frequency) phase-delta))))))

;;;;;;;;;;;;;;;;;;
;;; Given a frequency, return what bit is encoded in it
;;; frequency - the frequency to pull an encoded bit out of

(define (get-bit-from-frequency frequency)
    (if (angle-is-one? (abs (angle frequency))) 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (is-same-angle? a b)
    (or (< (abs (- a b)) round-off-error) 
        (< (abs (- b a)) round-off-error)))

;;;;;;;;;;;;;;;;;;
(define (angle-is-one? x)
    (or (is-same-angle? x 0)
        (is-same-angle? x pi/2)
        (is-same-angle? x pi)
        (is-same-angle? x 3pi/2)))
