(define (run-wavfile-tests)
    (load "src/wavfile.rkt")

    (file->wavfile-test)
    (finalize-wavfile-test)
    (create-wavfile-from-port-test)
    (create-wavfile-test)
    (read-wavfile-header-test)
    (write-wavfile-header-test)
    (read-samples-test)
    (write-samples-test)
    (bytes->samples-test)
    (get-samples-for-channel-test)
    (samples->bytes-test)
    (write-bytes-for-channel-test)
    (get-wavfile-max-payload-size-test)
    (get-starting-byte-test)
    (get-next-byte-test)
    (create-wavfile-header-bytes-test)
    (is-big-endian?-test)
)

;;;;;;;;;;;;;;;;;;
;;; Creates a wavfile for testing purposes
(define (create-wavfile-for-testing endianess audioformat channels samplerate byterate blockalign bytespersample chunkstart chunksize)
        (create-wavfile null endianess audioformat channels samplerate byterate blockalign bytespersample chunkstart chunksize))

;;;;;;;;;;;;;;;;;;;;
;;; 1 second of stereo audio, all samples are 0
(define (create-test-wavfile)
    (create-wavfile-for-testing 'little 1 2 44100 176400 4 16 44 176444))

;;;;;;;;;;;;;;;;;;
(define (file->wavfile-test)
   ;(file->wavfile src dest)
   (display "No test cases for file->wavfile\n"))

;;;;;;;;;;;;;;;;;;
(define (finalize-wavfile-test)
   ;(finalize-wavfile wav)
   (display "No test cases for finalize-wavfile\n"))

;;;;;;;;;;;;;;;;;;
(define (create-wavfile-from-port-test)
   ;(create-wavfile-from-port in)
   (display "No test cases for create-wavfile-from-port\n"))

;;;;;;;;;;;;;;;;;;
(define (create-wavfile-test)
   (let [(wav (create-wavfile null 'little 1 2 44100 88200 4 16 44 176444))]
        (check-equal? (wavfile-bytesperpage wav) 2048) ;;; 512 samples * 2 bytes * 2 channels
        (check-equal? (wavfile-endianess wav) 'little)
        (check-equal? (wavfile-audioformat wav) 1)
        (check-equal? (wavfile-channels wav) 2)
        (check-equal? (wavfile-samplerate wav) 44100)
        (check-equal? (wavfile-byterate wav) 88200)
        (check-equal? (wavfile-blockalign wav) 4)
        (check-equal? (wavfile-bytespersample wav) 2)
        (check-equal? (wavfile-chunkstart wav) 44)
        (check-equal? (wavfile-chunksize wav) 176400)))

;;;;;;;;;;;;;;;;;;
(define (read-wavfile-header-test)
   ;(read-wavfile-header in)
   (display "No test cases for read-wavfile-header\n"))

;;;;;;;;;;;;;;;;;;
(define (write-wavfile-header-test)
   ;(write-wavfile-header wav)
   (display "No test cases for write-wavfile-header\n"))

;;;;;;;;;;;;;;;;;;
(define (read-samples-test)
   ;(read-samples wav)
   (display "No test cases for read-samples\n"))

;;;;;;;;;;;;;;;;;;
(define (write-samples-test)
   ;(write-samples samples wav)
   (display "No test cases for write-samples\n"))

;;;;;;;;;;;;;;;;;;
(define (bytes->samples-test)
    (let* [(wav (create-test-wavfile))
           (b (make-bytes (wavfile-bytesperpage wav)))
           (s1 (make-vector samples-per-fft))
           (s2 (make-vector samples-per-fft))]
          (bytes-set! b 0 1)
          (bytes-set! b 2 1)
          (bytes-set! b 4 0)
          (bytes-set! b 5 1)
          (bytes-set! b 6 1)
          (bytes-set! b 7 2)
          (vector-set! s1 0 1)
          (vector-set! s2 0 1)
          (vector-set! s1 1 256)
          (vector-set! s2 1 513)
          (check-equal? (vector s1 s2) (bytes->samples b wav))))
          

;;;;;;;;;;;;;;;;;;
(define (get-samples-for-channel-test)
   (let [(wav (create-test-wavfile))]
        (let [(control-samples (make-vector samples-per-fft))
              (bytes (make-bytes (wavfile-bytesperpage wav)))]
             (vector-set! control-samples 0 5)
             (vector-set! control-samples 1 256)
             (vector-set! control-samples 2 4)
             (bytes-set! bytes 0 5)
             (bytes-set! bytes 5 1)
             (bytes-set! bytes 8 4)
             (check-equal? (get-samples-for-channel bytes wav 0) control-samples))
        (let [(control-samples (make-vector samples-per-fft))
              (bytes (make-bytes (wavfile-bytesperpage wav)))]
             (vector-set! control-samples 0 5)
             (vector-set! control-samples 1 256)
             (vector-set! control-samples 2 4)
             (bytes-set! bytes 2 5)
             (bytes-set! bytes 7 1)
             (bytes-set! bytes 10 4)
             (check-equal? (get-samples-for-channel bytes wav 1) control-samples))))

;;;;;;;;;;;;;;;;;;
(define (samples->bytes-test)
   (let* [(wav (create-test-wavfile))
          (s1 (make-vector samples-per-fft))
          (s2 (make-vector samples-per-fft))
          (samples (vector s1 s2))]
         (vector-set! s1 0 65)
         (vector-set! s1 1 596)
         (vector-set! s1 2 67)
         (vector-set! s2 0 5)
         (vector-set! s2 1 4)
         (vector-set! s2 2 3)
         (let [(bytes (samples->bytes samples wav))
               (control-bytes (make-bytes (wavfile-bytesperpage wav)))]
              (bytes-set! control-bytes 0 65)
              (bytes-set! control-bytes 2 5)
              (bytes-set! control-bytes 4 84)
              (bytes-set! control-bytes 5 2)
              (bytes-set! control-bytes 6 4)
              (bytes-set! control-bytes 8 67)
              (bytes-set! control-bytes 10 3)
              (check-equal? bytes control-bytes))))


;;;;;;;;;;;;;;;;;;
(define (write-bytes-for-channel-test)
   (let* [(wav (create-test-wavfile))
          (test-bytes (make-bytes (wavfile-chunksize wav)))
          (samples (make-vector (wavfile-samplerate wav)))
          (control-bytes (make-bytes (wavfile-chunksize wav)))]
         (vector-set! samples 0 65)
         (vector-set! samples 1 596)
         (vector-set! samples 2 67)
         (bytes-set! control-bytes 0 65)
         (bytes-set! control-bytes 4 84)
         (bytes-set! control-bytes 5 2)
         (bytes-set! control-bytes 8 67)
         (write-bytes-for-channel (vector samples null) test-bytes wav 0)
         (check-equal? control-bytes test-bytes)
         (bytes-set! control-bytes 2 65)
         (bytes-set! control-bytes 6 84)
         (bytes-set! control-bytes 7 2)
         (bytes-set! control-bytes 10 67)
         (write-bytes-for-channel (vector null samples) test-bytes wav 1)
         (check-equal? control-bytes test-bytes)))

;;;;;;;;;;;;;;;;;;
(define (get-wavfile-max-payload-size-test)
   (check-equal? (get-wavfile-max-payload-size (create-test-wavfile)) 17))

;;;;;;;;;;;;;;;;;;
(define (get-starting-byte-test)
   (let [(wav (create-test-wavfile))]
        (check-equal? (get-starting-byte 0 wav) 0)
        (check-equal? (get-starting-byte 1 wav) 2)
        (check-equal? (get-starting-byte 2 wav) 4)
        (check-equal? (get-starting-byte 3 wav) 6)))

;;;;;;;;;;;;;;;;;;
(define (get-next-byte-test)
   (let [(wav (create-test-wavfile))]
        (check-equal? (get-next-byte 0 wav) 4)
        (check-equal? (get-next-byte 1 wav) 5)
        (check-equal? (get-next-byte 2 wav) 6)
        (check-equal? (get-next-byte 3 wav) 7)))

;;;;;;;;;;;;;;;;;;
(define (create-wavfile-header-bytes-test)
   (let [(iib (lambda (num numbytes) (integer->integer-bytes num numbytes #f)))]
        (let [(wf1 (create-wavfile-for-testing 'little 1 2 44100 176400 4 2 44 82)) ;data size of 42 chosen for testing
              (h1 (make-bytes 44))]
             (bytes-copy! h1 0 (string->bytes/latin-1 "RIFF"))
             (bytes-copy! h1 4 (iib (+ 36 (wavfile-chunksize wf1)) 4))
	     (bytes-copy! h1 8 (string->bytes/latin-1 "WAVEfmt "))
	     (bytes-copy! h1 16 (iib 16 4))
	     (bytes-copy! h1 20 (iib (wavfile-audioformat wf1) 2))
	     (bytes-copy! h1 22 (iib (wavfile-channels wf1) 2))
	     (bytes-copy! h1 24 (iib (wavfile-samplerate wf1) 4))
	     (bytes-copy! h1 28 (iib (wavfile-byterate wf1) 4))
	     (bytes-copy! h1 32 (iib (wavfile-blockalign wf1) 2))
	     (bytes-copy! h1 34 (iib (* 8 (wavfile-bytespersample wf1)) 2))
	     (bytes-copy! h1 36 (string->bytes/latin-1 "data"))
             (bytes-copy! h1 40 (iib (wavfile-chunksize wf1) 4))
	     (check-equal? (create-wavfile-header-bytes wf1) h1))
        (let [(wf2 (create-wavfile-for-testing 'little 1 7 8000 168000 21 3 44 87)) ;data size of 87 chosen for testing
              (h2 (make-bytes 44))]
	     (bytes-copy! h2 0 (string->bytes/latin-1 "RIFF"))
	     (bytes-copy! h2 4 (iib (+ 36 (wavfile-chunksize wf2)) 4))
	     (bytes-copy! h2 8 (string->bytes/latin-1 "WAVEfmt "))
	     (bytes-copy! h2 16 (iib 16 4))
	     (bytes-copy! h2 20 (iib (wavfile-audioformat wf2) 2))
	     (bytes-copy! h2 22 (iib (wavfile-channels wf2) 2))
	     (bytes-copy! h2 24 (iib (wavfile-samplerate wf2) 4))
	     (bytes-copy! h2 28 (iib (wavfile-byterate wf2) 4))
	     (bytes-copy! h2 32 (iib (wavfile-blockalign wf2) 2))
	     (bytes-copy! h2 34 (iib (* 8 (wavfile-bytespersample wf2)) 2))
	     (bytes-copy! h2 36 (string->bytes/latin-1 "data"))
             (bytes-copy! h2 40 (iib (wavfile-chunksize wf2) 4))
	     (check-equal? (create-wavfile-header-bytes wf2) h2))))

;;;;;;;;;;;;;;;;;;
(define (is-big-endian?-test)
   (let [(big-one (create-wavfile-for-testing 'big 0 0 0 0 0 0 0 0))
	 (not-big-one (create-wavfile-for-testing 'little 0 0 0 0 0 0 0 0))]
	(check-true (is-big-endian? big-one))
	(check-false (is-big-endian? not-big-one))))
