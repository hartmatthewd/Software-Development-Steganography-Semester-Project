;;; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -q 9 ")

;;; the number of samples to use for each fft
(define samples-per-fft 512)

;;; Which frequency components to encode/decode on (base 1)
;;; More components make more noise but allow for more encoding
;;; Arranged as a vector of vectors 
;;; vector[i][j]
;;;   -- i - different bit in the same frame
;;;   -- j - same bit striped across multiple components
(define frequency-components-to-encode (vector (vector 10 11 12 13 14) 
                                               (vector 15 16 17 18 19)
                                               (vector 20 21 22 23 24)
                                               (vector 25 26 27 28 29)))

;;; The minimum magnitude in which to encode a bit
;;; NOTE: The system will boost magnitudes to be encoded that are below this level to this level
;;; so best to keep it fairly low
(define min-magnitude 1000)

;;; Different temp files to use during encoding/decoding/testing
(define tmpsrc (path->string (make-temporary-file "~a")))
(define tmpdest (path->string (make-temporary-file "~a")))
(define tmptemp (path->string (make-temporary-file "~a.mp3")))

;;; Samples files to use with testing
(define testwav "tst/testwav.wav")
(define testwav-2 "tst/excerpt0.wav")
(define testmp3 "tst/testmp3.mp3")
(define testmp3-2 "tst/brobob.mp3")
(define testmp3-3 "tst/classical.mp3")
(define testpayload "tst/testpayload.txt")

;;; The percentage of bits that are allowed to be decoded incorrectly and still pass testing
(define biterrorlimit 0.01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Don't Change anything below this line
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pi (3.1415.....)
(define pi (acos -1.0))
(define pi/2 (/ pi 2))
(define pi/4 (/ pi 4))
(define 3pi/2 (/ (* 3 pi) 2))
(define 2pi (* 2 pi))
(define i2pi (* 2.0 pi +1.0i))
(define -pi/4 (- 0 pi/4))
