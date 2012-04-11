;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; frequencycontroller.rkt
;
; Controller to determine where to encode/decode bits
; to/from. Encoders/decoders should interface solely with 
; a controller instead of trying to interact directly with
; a wavfile.
;
; To encode 
;
;     Encoders should use first create a controller
;     via the following command:
;
;         (make-encoder src dest size)
;         
;             src  - file path of the carrier to encode into
;             dest - file path of where to write the encoded
;                    carrier to
;             size - size of the payload which is to be encoded
;                    into the carrier
;
;     Encoders should then use the following method to
;     encode each bit:
;
;         (code-next-frequency controller func)
;
;             controller - the controller to encode into
;             func       - the encoding function in the
;                          format (vector? real? -> void)
;
;     Encoders will need to finalize the controller before
;     returning with the following method:
;
;         (finalize-controller controller)
;
;             controller - the controller to finalize
;
; To decode
;
;     Decoders should use first create a controller
;     via the following command:
;
;         (make-decoder src)
;         
;             src  - file path of the carrier to decode
;
;     Decoders should then use the following method to
;     decode each bit:
;
;         (code-next-frequency controller func)
;
;             controller - the controller to encode into
;             func       - the decoding function in the
;                          format (vector? real? -> void)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "src/wavfile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Frequency Controller
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; output (output-port?) - an output port to write to (can be null)
; dest (string?) - a destination path to convert to when done encoding (can be null)
; wavfile (wavfile?) - the wavfile to encode to/decode from
; overtone (real?) - the index of the overtone to encode to/decode from
; channel (real?) - the index of the current channel to encode to/decode from
; frequencies (vector?) - the given samples vector in frequency domain 
; samples (vector?) - the vector of samples to encode to/decode from

(struct controller (output dest wavfile [overtone #:mutable] [channel #:mutable] 
                                        [frequencies #:mutable] [samples #:mutable]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Constructors / Finalizer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Given a path to a source file, returns a decoder for that file
; inputs
;     src (string?) - the source file
; outputs
;     controller?

(define (make-decoder src)
    (let [(s (if (is-wav? src) src (begin (mp3->wav src tmpsrc) tmpsrc)))]
         (initialize-controller (controller null null (file->wavfile s) -1 0 null null))))
               

;;;;;;;;;;;;;;;;;;
; Returns an encoder for the given carrier, who will output to the given destination, and whos payload will be 
;     of the given length
; inputs
;     src (string?) - the source file to decode from
;     dest (string?) - the path to the file to output the encoded message to
;     size (real?) - the size of the payload which will be encoded in the given src carrier
; outputs
;     controller?

(define (make-encoder src dest size)
    (let* [(iswav (is-wav? src))
           (s (if iswav src (begin (mp3->wav src tmpsrc) tmpsrc)))
           (wav (file->wavfile s))]
          (when all-or-nothing (ensure-wavfile-large-enough wav size))
          (let* [(o (if iswav (open-file-output-port dest) (open-file-output-port tmpdest)))
                 (d (if iswav null dest))
                 (c (controller o d  wav -1 0 null null))]
                (write-wavfile-header (controller-wavfile c) (controller-output c))
                (initialize-controller c))))

;;;;;;;;;;;;;;;;;;
; Initializes the various members of the given controller. Returns the same controller
; inputs
;     controller (controller?) - the controller to initialize
; outputs
;     controller?

(define (initialize-controller controller)
    (set-next-samples controller)
    (parse-frequencies controller)
    controller)

;;;;;;;;;;;;;;;;;;
; Finalizes the given controller, writing the remainder of its samples and tying up any loose ends
;     This must be called when encoding, decoding is optional
; inputs
;     controller (controller?) - the controller to finalize
; outputs
;     void

(define (finalize-controller controller)
    (when (is-encoder? controller)
          (write-current-frequencies controller)
          (write-current-samples controller)
          (pipe-and-finish controller)))

;;;;;;;;;;;;;;;;;;
; Pipe the last of the wavfile bytes to the output and tie up any loose ends
; inputs
;     controller (controller?) - the controller to finsh up
; outputs
;     void

(define (pipe-and-finish controller)
    (when (is-encoder? controller)
          (pipe-remaining-wavfile-bytes (controller-wavfile controller) (controller-output controller))
          (when (not (null? (controller-dest controller)))
                (wav->mp3 tmpdest (controller-dest controller)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Coding Function
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Codes the next frequency via the passed func
; inputs
;     controller (controller?) - the controller that will code the next frequency
;     func (vector? real? -> void) 
;         vector? - the vector of frequencies to controller to/from
;         real? - the index within the frequencies vector to code to/from
; outputs
;     void

(define (code-next-frequency controller func)
    (set-controller-overtone! controller (add1 (controller-overtone controller)))
    (when (= (controller-overtone controller) (vector-length frequency-components-to-encode))
          (page-frequencies controller))
    (func (controller-frequencies controller) (vector-ref frequency-components-to-encode 
                                                          (controller-overtone controller))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Frequencies
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Pages out the current frequencies of the given controller, retrieving the next set
; inputs
;     controller (controller?) - the controller whos frequencies to page
; outputs
;     void

(define (page-frequencies controller)
    (write-current-frequencies controller)
    (page-samples controller)
    (parse-frequencies controller)
    (set-controller-overtone! controller 0))

;;;;;;;;;;;;;;;;;;
; Writes the current frequencies of the given controller to the controller's wavfile
; inputs
;     controller (controller?) - the controller whos frequencies to write
; outputs
;     void

(define (write-current-frequencies controller)
    (vector-set! (controller-samples controller) 
                 (controller-channel controller) 
                 (sanitize-samples (fft-inverse (controller-frequencies controller)))))

;;;;;;;;;;;;;;;;;;
; Parse the given controllers samples into its frequencies
; inputs
;     controller (controller?) - the controller whos samples to parse into frequencies
; outputs
;     void

(define (parse-frequencies controller)
    (set-controller-frequencies! controller 
                                 (fft (vector-ref (controller-samples controller) (controller-channel controller)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Samples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Page out the given controller's samples and retrieve the next set
; inputs
;     controller (controller?) - the controller whos samples to page
; outputs
;     void

(define (page-samples controller)
    (set-controller-channel! controller (add1 (controller-channel controller)))
    (when (= (controller-channel controller) (vector-length (controller-samples controller)))
          (write-current-samples controller)
          (set-next-samples controller)
          (set-controller-channel! controller 0)))

;;;;;;;;;;;;;;;;;;
; Writes the given controller's samples to the controller's wavfile
; inputs
;     controller (controller?) - the controller whos samples to write
; outputs
;     void

(define (write-current-samples controller)
    (when (is-encoder? controller)
          (write-samples (controller-samples controller) 
                         (controller-wavfile controller) 
                         (controller-output controller))))

;;;;;;;;;;;;;;;;;;
; Sets the given controller's samples to the next set of samples according to the controller's wavfile
; inputs
;     controller (controller?) - the controller who's samples to set
; outputs
;     void

(define (set-next-samples controller)
    (with-handlers [((lambda (x) (eq? max-pages-exceeded x))
                     (lambda (x) (pipe-and-finish controller)
                                 (raise x)))]
                   (set-controller-samples! controller (read-samples (controller-wavfile controller)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Utils
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Ensures the given wavfile is large enough to encode the given number of bytes, errors otherwise
; inputs
;     wav (wavfile?) - the wavfile to be act as carrier
;     size (real?) - the number of bytes to be encoded
; outputs
;     void

(define (ensure-wavfile-large-enough wav size)
    (let [(maxsize (get-max-payload-size wav))]
         (when (> size maxsize)
               (error 'ERROR (string-append "Payload too large for target carrier. Size: "
                                            (number->string size)
                                            " bytes - Maxsize: "
                                            (number->string maxsize)
                                            " bytes")))))

;;;;;;;;;;;;;;;;;;
; Validates the given size to ensure it is possible to encode/decode it to/from the controller
;     If the size will fit, return it. Else return the max size that will fit
; inputs
;     controller (controller?) - the controller to return a valid payload size for
;     size (real?) - the suggested size of what will be coded
; outputs
;     real?

(define (validate-payload-size controller size)
    (min size (get-max-payload-size (controller-wavfile controller))))

;;;;;;;;;;;;;;;;;;
; Sanatize the frequency vactor to ensure that each sample is an exact integer
; inputs
;     samples (vector?) - the vector of samples to sanitize
; outputs
;     vector?

(define (sanitize-samples samples)
    (vector-map! (lambda (x) (min 32767 (max -32768 (exact (round (real-part x)))))) samples))

;;;;;;;;;;;;;;;;;;
; Returns true if the given controller is a decoder, false otherwise
; inputs
;     controller (controller?) - returns true if is a decoder, false otherwise
; outputs
;     boolean?

(define (is-decoder? controller)
    (null? (controller-output controller)))

;;;;;;;;;;;;;;;;;;
; Returns true if the given controller is a encoder, false otherwise
; inputs
;     controller (controller?) - returns true if is a encoder, false otherwise
; outputs
;     boolean?

(define (is-encoder? controller)
    (not (is-decoder? controller)))

;;;;;;;;;;;;;;;;;;
; Returns the maximum payload size that will fit in the given controller
; inputs
;     wav (wavfile?) - the wavfile who's maximum payload size to return
; outputs
;     real?

(define (get-max-payload-size wav)
    (* (get-wavfile-max-payload-size wav) (vector-length frequency-components-to-encode)))

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
