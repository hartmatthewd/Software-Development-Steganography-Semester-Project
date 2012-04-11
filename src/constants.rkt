; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -q 9 ")

; the number of samples to use for each fft (must be a power of 2)
(define samples-per-fft 512)

; if the payload cannot fit within the carrier, still encode as much as possible?
(define all-or-nothing #t)

; Which frequency components to encode/decode on
;    - More components make more noise but allow for more encoding
;    - Higher frequencies are typically more steganographic but may lead to more data loss
;    - Vector of vectors
;        - A different bit can be encoded within each first level vector
;        - The bit will be encoded on each of the components in the second level vector
;            - Encoding each bit on multiple components can help to reduce error rate
;            - When decoding, majority rules. So if encoding on 5 components, 2 decode a 0 and 3 decode 1, a 1 is used
;            - Ties go to 0
(define frequency-components-to-encode #(#(10 11 12 13 14) 
                                         #(15 16 17 18 19)
                                         #(20 21 22 23 24)
                                         #(25 26 27 28 29)))

; The minimum magnitude in which to encode a bit
; The system will boost magnitudes to be encoded that are below this level to this level
; Best to keep it fairly low (below 5000)
(define min-magnitude 1000)

; Different temp files to use during encoding/decoding/testing
(define tmpsrc (path->string (make-temporary-file "~a")))
(define tmpdest (path->string (make-temporary-file "~a")))
(define tmptemp (path->string (make-temporary-file "~a.mp3"))) ; must end in .mp3

; Samples files to use with testing
(define testwav "tst/testwav.wav")
(define testwav-2 "tst/excerpt0.wav")
(define testmp3 "tst/testmp3.mp3")
(define testmp3-2 "tst/brobob.mp3")
(define testmp3-3 "tst/classical.mp3")
(define testpayload "tst/testpayload.txt")

; The percentage of bits that are allowed to be decoded incorrectly and still pass testing
(define biterrorlimit 0.01)

; An exception thrown when wavfile tries to read more bytes than are left in the file
(define max-pages-exceeded 'max-pages-exceeded)

; pi (3.1415.....)
(define pi (acos -1.0))
(define pi/2 (/ pi 2))
(define pi/4 (/ pi 4))
(define 2pi (* 2 pi))
(define i2pi (* 2.0 pi +1.0i))
(define -pi/4 (- 0 pi/4))
