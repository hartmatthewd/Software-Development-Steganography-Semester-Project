(load "src/wavfile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Frequency Coder
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(struct coder (wavfile [overtone #:mutable] [frequencies #:mutable] [samples #:mutable]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Constructors / Finalizer
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (make-decoder src)
    (make-encoder src null))

;;;;;;;;;;;;;;;;;;
(define (make-encoder src dest)
    (let [(coder (coder (file->wavfile src dest) -1 null null))]
         (get-next-frequencies coder)
         coder))

;;;;;;;;;;;;;;;;;;
(define (finalize-coder coder)
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
          (page-frequencies coder)
          (set-coder-overtone! coder 0))
    (func (coder-frequencies coder) (vector-ref frequency-components-to-encode (coder-overtone coder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Utils
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (ensure-destination-large-enough? coder size)
    (let [(maxsize (* (get-wavfile-max-payload-size (coder-wavfile coder)) (vector-length frequency-components-to-encode)))]
         (when (not (<= size maxsize))
               (error 'Failure (format "Payload is too large for the given audio carrier. Size: ~a bytes | Max Size: ~a bytes" size maxsize)))))

;;;;;;;;;;;;;;;;;;
(define (validate-payload-size coder size)
    (min size (* (get-wavfile-max-payload-size (coder-wavfile coder)) (vector-length frequency-components-to-encode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (page-frequencies coder)
    (write-current-frequencies coder)
    (get-next-frequencies coder))

;;;;;;;;;;;;;;;;;;
(define (write-current-frequencies coder)
    (vector-copy! (coder-samples coder) 0 (sanitize-samples (fft-inverse (coder-frequencies coder)))))

;;;;;;;;;;;;;;;;;;
(define (get-next-frequencies coder)
    (set-coder-samples! coder (get-next-samples (coder-wavfile coder)))
    (set-coder-frequencies! coder (fft (coder-samples coder))))

;;;;;;;;;;;;;;;;;;
;;; Sanatize the frequency vactor to ensure that each sample is an exact integer
;;; (round off error in the fft can make them slightly off)
;;; samples - the vector of samples to sanitize

(define (sanitize-samples samples)
    (vector-map! (lambda (x) (min 32767 (max -32768 (exact (round (real-part x)))))) samples))
