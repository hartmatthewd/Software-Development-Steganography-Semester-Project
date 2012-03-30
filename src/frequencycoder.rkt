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
    (let [(maxsize (* (get-wavfile-max-payload-size (coder-wavfile coder)) (vector-length frequency-components-to-encode)))]
         (when (not (<= size maxsize))
               (error 'Failure (format "Payload is too large for the given audio carrier. Size: ~a bytes | Max Size: ~a bytes" size maxsize)))))

;;;;;;;;;;;;;;;;;;
(define (validate-payload-size coder size)
    (min size (* (get-wavfile-max-payload-size (coder-wavfile coder)) (vector-length frequency-components-to-encode))))

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
