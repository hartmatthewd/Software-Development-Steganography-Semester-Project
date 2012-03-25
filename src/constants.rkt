;;; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -q 9 ")

;;; pi (3.1415.....)
(define pi (acos -1.0))

;;; the number of samples to use for each fft
(define samples-per-fft 64)

(define i2pi (* 2.0 pi +1.0i))

;;; The amount to shift when encoding a 0
(define zero-shift 0)

;;; The amount to shift when encoding a 1
(define one-shift pi)

;;; The amount of error we allow for round off error in determining the phase of a frequency
(define round-off-error 0.4)

;;; The index (base 0) of the frequency to encode each bit on when sorted by magnitude
;;; For example: the fundamental frequency would be 0, the first overtone would be 1
(define frequency-to-encode 1)

(define tmpsrc "/tmp/stegosrc")
(define tmpdest "/tmp/stegodest")
