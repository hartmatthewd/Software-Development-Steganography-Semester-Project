(define (run-wavfile-tests)
   (display "tst/wavfile.rkt") (newline)
(file->wavfile-test)
(finalize-wavfile-test)
(create-wavfile-from-ports-test)
(create-wavfile-test)
(read-wavfile-header-test)
(write-wavfile-header-test)
(read-samples-test)
(write-samples-test)
(bytes->samples-test)
(get-samples-for-cahnnel-test)
(samples->bytes-test)
(write-bytes-for-channel-test)
(get-wavfile-max-payload-size-test)
(get-starting-byte-test)
(get-next-byte-test)
(create-wavfile-header-bytes-test)
(is-big-endian?-test)
;   (test-write-wavfile-bytes-for-channel)
;   (test-set-wavfile-samples-for-channel)
;   (test-write-wavfile-to-bytes)
;   (test-set-wavfile-samples)
)

(define (file->wavfile-test)
;(file->wavfile src dest)
(display "No test cases for file->wavfile\n"))

(define (finalize-wavfile-test)
;(finalize-wavfile wav)
(display "No test cases for finalize-wavfile\n"))

(define (create-wavfile-from-ports-test)
;(create-wavfile-from-ports in out dest)
(display "No test cases for create-wavfile-from-ports\n"))

(define (create-wavfile-test)
;(create-wavfile in out dest e af c sr br ba bps s cs)
(display "No test cases for create-wavfile\n"))

(define (read-wavfile-header-test)
;(read-wavfile-header in)
(display "No test cases for read-wavfile-header\n"))

(define (write-wavfile-header-test)
;(write-wavfile-header wav)
(display "No test cases for write-wavfile-header\n"))

(define (read-samples-test)
;(read-samples wav)
(display "No test cases for read-samples\n"))

(define (write-samples-test)
;(write-samples samples wav)
(display "No test cases for write-samples\n"))

(define (bytes->samples-test)
;(bytes->samples bytes wav)
(display "No test cases for bytes->samples\n"))

(define (get-samples-for-cahnnel-test)
;(get-samples-for-cahnnel bytes wav channel)
(display "No test cases for get-samples-for-cahnnel\n"))

(define (samples->bytes-test)
;(samples->bytes samples wav)
(display "No test cases for samples->bytes\n"))

(define (write-bytes-for-channel-test)
;(write-bytes-for-channel samples bytes wav channel)
(display "No test cases for write-bytes-for-channel\n"))

(define (get-wavfile-max-payload-size-test)
;(get-wavfile-max-payload-size wav)
(display "No test cases for get-wavfile-max-payload-size\n"))

(define (get-starting-byte-test)
;(get-starting-byte channel wav)
(display "No test cases for get-starting-byte\n"))

(define (get-next-byte-test)
;(get-next-byte byte wav)
(display "No test cases for get-next-byte\n"))

(define (create-wavfile-header-bytes-test)
;(create-wavfile-header-bytes wav)
;(wavfile-create input
;                output
;                dest
;                endianess
;                audioformat
;                channels
;                samplerate
;                byterate
;                blockalign
;                bytespersample
;                chunkstart
;                chunksize)
(let ([wf1 (create-wavfile 'in 'out 'dest 'end
                           1 2 44100
                           176400 4 2
                           42 'chst)] ;chunksize of 42 chosen for testing
      [wf2 (create-wavfile 'in 'out 'dest 'end
                           1 7 8000
                           168000 21 3
                           87 'chst)] ;chunksize of 87 chosen for testing
      [h1 (make-bytes 44)]
      [h2 (make-bytes 44)]
      [iib (lambda (num numbytes) (integer->integer-bytes num numbytes #f))])

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
      (check-equal? (create-wavfile-header-bytes wf1) h1)
      (check-equal? (create-wavfile-header-bytes wf2) h2)))

(define (is-big-endian?-test)
;(is-big-endian? wav)
(let ([big-one (wafile #f #f #f #f 'big #f #f #f #f #f #f #f #f)]
      [not-big-one (wafile #f #f #f #f 'xyzzy #f #f #f #f #f #f #f #f)])
     (check-true (is-big-endian? big-one))
     (check-false (is-big-endian? not-big-one))))

(define (test-write-wavfile-bytes-for-channel)
   (letrec* [(sample-wavfile (create-test-wavfile))
             (bytes (make-bytes (+ (wavfile-chunkstart sample-wavfile) (wavfile-chunksize sample-wavfile)) 1))
             (control (bytes-append (make-bytes 44 1)
                                    (make-bytes (wavfile-chunksize sample-wavfile) 0)))]
           (write-wavfile-bytes-for-channel bytes sample-wavfile 0)
           (check-false (bytes=? bytes control))
           (write-wavfile-bytes-for-channel bytes sample-wavfile 1)
           (check-true (bytes=? bytes control))))

(define (test-write-wavfile-to-bytes)
   (letrec* [(sample-wavfile (create-test-wavfile))
             (bytes (make-bytes (+ (wavfile-chunkstart sample-wavfile) (wavfile-chunksize sample-wavfile)) 1))
             (control (bytes-append (make-bytes 44 1)
                                    (make-bytes (wavfile-chunksize sample-wavfile) 0)))]
           (write-wavfile-to-bytes sample-wavfile bytes)
           (check-true (bytes=? bytes control))))

(define (test-set-wavfile-samples-for-channel)
   (letrec* [(sample-wavfile (create-test-wavfile))
             (bytes (make-bytes (+ (wavfile-chunkstart sample-wavfile) (wavfile-chunksize sample-wavfile)) 1))
             (left (make-vector 44100 257))
             (right (make-vector 44100 257))]
           (set-wavfile-samples-for-channel bytes sample-wavfile 0)
           (check-equal? left (vector-ref (wavfile-samples sample-wavfile) 0))
           (check-not-equal? right (vector-ref (wavfile-samples sample-wavfile) 1))
           (set-wavfile-samples-for-channel bytes sample-wavfile 1)
           (check-equal? left (vector-ref (wavfile-samples sample-wavfile) 0))
           (check-equal? right (vector-ref (wavfile-samples sample-wavfile) 1))))

(define (test-set-wavfile-samples)
   (letrec* [(sample-wavfile (create-test-wavfile))
             (bytes (make-bytes (+ (wavfile-chunkstart sample-wavfile) (wavfile-chunksize sample-wavfile)) 1))
             (left (make-vector 44100 257))
             (right (make-vector 44100 257))]
           (set-wavfile-samples bytes sample-wavfile)
           (check-equal? left (vector-ref (wavfile-samples sample-wavfile) 0))
           (check-equal? right (vector-ref (wavfile-samples sample-wavfile) 1))))
