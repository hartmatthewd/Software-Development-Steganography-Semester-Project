(load "src/wavfile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Frequency Coder
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(struct coder (wavfile [overtone #:mutable] [channel #:mutable] [frequencies #:mutable] [samples #:mutable]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Constructors / Finalizer
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (make-decoder src)
    (initialize-coder (make-coder src null)))

;;;;;;;;;;;;;;;;;;
(define (make-encoder src dest len)
    (let [(coder (make-coder src dest))]
         (ensure-destination-large-enough? coder len)
         (write-wavfile-header (coder-wavfile coder))
         (initialize-coder coder)))

;;;;;;;;;;;;;;;;;;
(define (make-coder src dest)
    (coder (file->wavfile src dest) -1 0 null null))

;;;;;;;;;;;;;;;;;;
(define (initialize-coder coder)
    (get-next-samples coder)
    (parse-frequencies coder)
    coder)

;;;;;;;;;;;;;;;;;;
(define (finalize-coder coder)
    (write-current-frequencies coder)
    (write-current-samples coder)
    (finalize-wavfile (coder-wavfile coder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Coding Functions
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (code-next-frequency coder func)
    (set-coder-overtone! coder (add1 (coder-overtone coder)))
    (when (= (coder-overtone coder) (vector-length frequency-components-to-encode))
          (page-frequencies coder))
    (func (coder-frequencies coder) (vector-ref frequency-components-to-encode (coder-overtone coder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Utils
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (ensure-destination-large-enough? coder size)
    (let [(maxsize (get-max-payload-size coder))]
         (when (not (<= size maxsize))
               (error 'Failure (format "Payload is too large for the given audio carrier. Size: ~a bytes | Max Size: ~a bytes" size maxsize)))))

;;;;;;;;;;;;;;;;;;
(define (validate-payload-size coder size)
    (min size (get-max-payload-size coder)))

;;;;;;;;;;;;;;;;;;
(define (get-max-payload-size coder)
    (* (get-wavfile-max-payload-size (coder-wavfile coder)) (vector-length frequency-components-to-encode)))

;;;;;;;;;;;;;;;;;;
;;; Sanatize the frequency vactor to ensure that each sample is an exact integer
;;; (round off error in the fft can make them slightly off)
;;; samples - the vector of samples to sanitize

(define (sanitize-samples samples)
    (vector-map! (lambda (x) (min 32767 (max -32768 (exact (round (real-part x)))))) samples))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Frequencies
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (page-frequencies coder)
    (write-current-frequencies coder)
    (page-samples coder)
    (parse-frequencies coder)
    (set-coder-overtone! coder 0))

;;;;;;;;;;;;;;;;;;
(define (write-current-frequencies coder)
    (vector-set! (coder-samples coder) (coder-channel coder) (sanitize-samples (fft-inverse (coder-frequencies coder)))))

;;;;;;;;;;;;;;;;;;
(define (parse-frequencies coder)
    (set-coder-frequencies! coder (fft (vector-ref (coder-samples coder) (coder-channel coder)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Samples
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (page-samples coder)
    (set-coder-channel! coder (add1 (coder-channel coder)))
    (when (= (coder-channel coder) (vector-length (coder-samples coder)))
          (write-current-samples coder)
          (get-next-samples coder)
          (set-coder-channel! coder 0)))

;;;;;;;;;;;;;;;;;;
(define (write-current-samples coder)
    (write-samples (coder-samples coder) (coder-wavfile coder)))

;;;;;;;;;;;;;;;;;;
(define (get-next-samples coder)
    (set-coder-samples! coder (read-samples (coder-wavfile coder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
