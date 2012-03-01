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

#!r6rs
(library (local fft)

 (export fft
         fft-inverse
         make-test-vector
         sanity-test)

 (import (rnrs base)
         (rnrs control)
         (rnrs arithmetic bitwise)
         (rnrs arithmetic fixnums)
         (rnrs io simple)
         (rnrs sorting))

;;; 2*pi*i

(define i2pi
  (* 2.0 (acos -1.0) +1.0i))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sanity test.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-test-vector n m)
  (let ((v (make-vector n 0.0)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (vector-set! v i (sin (/ (* 2.0 (acos -1.0) i) m))))
    v))

(define (sanity-test n)
  (let* ((v1 (make-test-vector n 16))
         (v2 (fft v1))
         (v3 (fft-inverse v2))
         (v4 (vector-map - v3 v1))
         (v5 (vector-map magnitude v4))
         (v6 (vector-sort > v5)))
    (if (> (vector-ref v6 0) 1e-6)
        (begin (display "Failed sanity test.")
               (newline)))))

) ; end of (local fft) library
