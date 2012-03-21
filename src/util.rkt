;;;
;;; A set of various utility methods
;;;

(define pi (acos -1.0))
(define samples-per-fft 128)
(define duplicate-encoding-each-channel #f)


; Given a vector of bits of length 8, returns a byte whos bits match those in the vector
(define (get-byte-from-bit-vector bits)
  (letrec
    ((set-bit (lambda (index byte)
      (if (= index 8)
          byte
          (set-bit (+ index 1) (bitwise-ior (vector-ref bits index) (arithmetic-shift byte 1)))))))
    (set-bit 1 (bitwise-ior 0 (vector-ref bits 0)))))

; Given a byte, return a list whos elements are the individual bits of the byte
(define (get-bits-from-byte byte)
  (let ((v (make-vector 8)))
    (letrec
      ((set-bit (lambda (index byte)
        (when (>= index 0)
          (begin (vector-set! v index (bitwise-and byte 1))
                 (set-bit (- index 1) (arithmetic-shift byte -1)))))))
      (set-bit 7 byte)) v))

;;; Given a real number value, an endianess and a number of bytes, 
;;; return a bytestring of the given length representing that value
(define (value->bytes val end len)
    (letrec ((bytes (make-bytes len))
             (get-i (if (eq? 'little end)
                       (lambda (i) i)
                       (lambda (i) (- len i 1))))
             (recurse (lambda (v i) (if (= i len)
                                        bytes
                                        (let ((j (modulo v 256)))
                                             (bytes-set! bytes (get-i i) j)
                                             (recurse (/ (- v j) 256) (+ i 1)))))))
            (recurse val 0)))


;;; Returns the next index of the samples to encode to

(define current-samples-index 0)
(define (get-next-sample-index samples)
    (set! current-samples-index (+ current-samples-index samples-per-fft))
    current-samples-index)
;;    (modulo (exact (floor (* (random) (vector-length samples)))) samples-per-fft))


;;; Given a vector of frequencies in the frequency domain, find the fundamental frequency

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
;                                (func (get-index-with-max-mag i n) (+ n 1)))))]
;             (func 1 0)))



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

(require rnrs/arithmetic/bitwise-6)
(require rnrs/arithmetic/fixnums-6)
(require rnrs/base-6)

(define i2pi
  (* 2.0 pi +1.0i))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     Locals
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
