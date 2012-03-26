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
    (let [(test-same-angle (lambda (a b)
                                   (or (< (abs (- a b)) round-off-error)
                                       (< (abs (- b a)) round-off-error))))
          (ang (angle frequency))]
         (if (or (test-same-angle (abs ang) 0)
                 (test-same-angle (abs ang) pi/2)
                 (test-same-angle (abs ang) pi)
                 (test-same-angle (abs ang) 3pi/2))
             1
             0)))
