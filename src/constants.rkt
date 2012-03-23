;;; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -h")

;;; pi (3.1415.....)
(define pi (acos -1.0))

;;; the number of samples to use for each fft
(define samples-per-fft 64)

(define i2pi (* 2.0 pi +1.0i))

;;; The amount to shift when encoding a 0
(define zero-shift 0)

;;; The amount to shift when encoding a 1
(define one-shift pi)

(define tmp-file "/tmp/stego")
