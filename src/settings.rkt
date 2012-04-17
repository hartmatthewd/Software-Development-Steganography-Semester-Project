; if the payload cannot fit within the carrier, still encode as much as possible?
(define all-or-nothing #t)

; Which frequency components to encode/decode on
;    - More components make more noise but allow for more encoding or more accurate decoding
;    - Vector of vectors
;        - A different bit can be encoded within each first level vector
;        - The bit will be encoded on each of the components in the second level vector (DO NOT encode on 0)
;            - Encoding each bit on multiple components can help to reduce error rate
;            - When decoding, majority rules. So if encoding on 5 components, 2 decode a 0 and 3 decode 1, a 1 is used
;            - Ties go to 0
;    - For more encoding, add more first level vectors
;    - For more accurate encoding, extend each second level vector
;    - Low pitch audio files are most steganographic with lower frequencies components
;    - High pitch audio files are most steganographic with higher frequencies components
(define frequency-components-to-encode #(
                                         #(20 21 22 23 24)
                                         #(25 26 27 28 29)
                                         #(30 31 32 33 34)
                                         #(35 36 37 38 39)
                                        ))

; The minimum magnitude of a frequency in which to encode a bit
; The system will boost magnitudes to be encoded that are below this level to this level
; Best to keep it fairly low (below 5000)
; Default: 1000
(define min-magnitude 10)
