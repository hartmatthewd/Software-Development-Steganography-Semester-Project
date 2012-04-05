;;; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -q 9 ")

;;; the number of samples to use for each fft
(define samples-per-fft 512)

;;; Which frequency components to encode/decode on (base 1)
;;; More components make more noise but allow for more encoding
(define frequency-components-to-encode (vector 10 11 12 13))

;;; The minimum magnitude in which to encode a bit
;;; NOTE: The system will boost magnitudes to be encoded that are below this level to this level
;;; so best to keep it fairly low
(define min-magnitude 1000)

;;; Different temp files to use during encoding/decoding/testing
(define tmpsrc "/tmp/stegosrc")
(define tmpdest "/tmp/stegodest")
(define tmptemp "/tmp/stegotemp.mp3")

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
