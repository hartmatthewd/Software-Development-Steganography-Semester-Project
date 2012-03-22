
;;;;;;;;;;;;;;;;;;
;;; Given a vector of bits of length 8, returns a byte whos bits match those in the vector
;;; bits - the vector of bits of which to make a byte out of

(define (get-byte-from-bit-vector bits)
  (do [(index 1 (add1 index))
       (byte (vector-ref bits 0) (bitwise-ior (vector-ref bits index) (arithmetic-shift byte 1)))]
      [(= index 8) byte]))

;;;;;;;;;;;;;;;;;;
;;; Given a byte, return a list whos elements are the individual bits of the byte
;;; byte - the byte of which to return a vector of its bits

(define (get-bits-from-byte byte)
  (let ((v (make-vector 8)))
    (do [(i 7 (sub1 i)) (b byte (arithmetic-shift b -1))]
        [(< i 0)]
        (vector-set! v i (bitwise-and b 1)))
    v))

;;;;;;;;;;;;;;;;;;
;;; Given a real number value, an endianess and a number of bytes, 
;;; return a bytestring of the given length representing that value
;;; val - the value to break into bytes
;;; end - endianess of the byte to return ('little or 'big)
;;; len - the number of bytes to turn the value into

(define (value->bytes val end len)
    (letrec ((bytes (make-bytes len))
             (get-i (if (eq? 'little end)
                       (lambda (i) i)
                       (lambda (i) (- len i 1))))
             (recurse (lambda (v i) (if (= i len)
                                        bytes
                                        (let ((j (mod v 256)))
                                             (bytes-set! bytes (get-i i) j)
                                             (recurse (/ (- v j) 256) (add1 i)))))))
            (recurse val 0)))


;;;;;;;;;;;;;;;;;;
;;; Returns the next index of the samples to encode to

(define current-samples-index 0)

(define (get-next-sample-index)
    (set! current-samples-index (+ current-samples-index samples-per-fft))
    current-samples-index)
;;    (mod (exact (floor (* (random) (vector-length samples)))) samples-per-fft))


;;;;;;;;;;;;;;;;;;
;;; Given a vector of frequencies in the frequency domain, find the fundamental frequency
;;; frequencies - the vector of frequencies of which to find the fundamental one

(define (get-fundamental-frequency frequencies)
    1)
;     (letrec [(vector-mid (/ (vector-length frequencies) 2))
;              (get-index-with-max-mag (lambda (a b)
;                                   (if (< (magnitude (vector-ref frequencies a))
;                                          (magnitude (vector-ref frequencies b)))
;                                       a
;                                       b)))
;              (func (lambda (i n)
;                            (if (= n vector-mid)
;                                i
;                                (func (get-index-with-max-mag i n) (add1 n)))))]
;             (func 1 0)))

;;;;;;;;;;;;;;;;;;
;;; Returns true if the given wavfile bytes are big endian, false otherwise
(define (is-big-endian? wav)
    (eq? (wavfile-endianess wav) 'big))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extracted from: /course/cs4500wc/Examples/FFT/fft.sls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2010 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Complex FFT in R6RS Scheme
;;;
;;; Translated (by William D Clinger) from the pseudocode in
;;;
;;; Cormen, Leiserson, Rivest, and Stein.
;;; Introduction to Algorithms, 3rd edition.
;;; MIT Press, 2009.
;;;
;;; (fft v)
;;;
;;;     The input v must be a vector of complex numbers, and
;;;     its length must be a power of 2.
;;;
;;;     Returns a vector of complex numbers of the same length
;;;     as v.  If v represents samples in the time domain, then
;;;     the result represents coefficients in the frequency
;;;     domain.
;;;
;;; (fft-inverse v)
;;;
;;;     The input v must be a vector of complex numbers, and
;;;     its length must be a power of 2.
;;;
;;;     Returns a vector of complex numbers of the same length
;;;     as v.  If v represents coefficients in the frequency
;;;     domain, then the result represents samples in the time
;;;     domain.
;;;
;;; (fft-inverse (fft v)) returns v (to within roundoff error).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fft a)
  (fft-in-place (bit-reverse-copy a) i2pi))

(define (fft-inverse a)
  (let ((a (fft-in-place (bit-reverse-copy a) (- i2pi))))
    (do ((n (vector-length a))
         (i 0 (+ i 1)))
        ((= i n)
         a)
      (vector-set! a i (/ (vector-ref a i) n)))))

(define (bit-reverse-copy a)
  (let* ((n (vector-length a))
         (lgn (bitwise-length (- n 1)))
         (a2 (make-vector n 0.0)))
    (do ((k 0 (+ k 1)))
        ((= k n)
         a2)
      (vector-set! a2 (fxreverse-bit-field k 0 lgn) (vector-ref a k)))))

(define (fft-in-place a i2pi)
  (let* ((n (vector-length a))
         (lgn (bitwise-length (- n 1))))
    (do ((m 2 (+ m m)))
        ((> m n)
         a)
      (let ((omega_m (exp (/ i2pi m))))
        (do ((k 0 (+ k m)))
            ((= k n))
          (let ((m/2 (div m 2)))
            (do ((omega 1.0 (* omega omega_m))
                 (j 0 (+ j 1)))
                ((= j m/2))
              (let* ((t (* omega (vector-ref a (+ k j m/2))))
                     (u (vector-ref a (+ k j))))
                (vector-set! a (+ k j) (+ u t))
                (vector-set! a (+ k j m/2) (- u t))))))))))
