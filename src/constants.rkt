;;; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -q 9 ")

;;; pi (3.1415.....)
(define pi (acos -1.0))
(define pi/2 (/ pi 2))
(define 3pi/2 (/ (* 3 pi) 2))
(define 2pi (* 2 pi))
(define i2pi (* 2.0 pi +1.0i))

;;; the number of samples to use for each fft
(define samples-per-fft 256)

;;; The amount of error we allow for round off error in determining the phase of a frequency
;;; on decoding
(define round-off-error (/ pi 8))

(define phase-delta (/ pi 2))

;;; The index (base 0) of the frequency to encode each bit on when sorted by magnitude
;;; For example: the fundamental frequency would be 0, the first overtone would be 1
(define frequency-to-encode 2)

(define tmpsrc "/tmp/stegosrc")
(define tmpdest "/tmp/stegodest")
