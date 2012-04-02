(define (run-fileio-tests)
   (load "src/fileio.rkt")

   (open-file-input-port-test)
   (open-file-output-port-test)
   (read-bytes-from-file-test)
   (pipe-remaining-bytes-test)
   (read-file-into-bytestring-test)
   (write-bytestring-to-file-test)
   (finalize-input-port-test)
   (finalize-output-port-test)
   (is-wav?-test)
   (is-mp3?-test)
   (wav->mp3-test)
   (mp3->wav-test)
   (test-file-writer-reader))

(define (open-file-input-port-test)
   ;(open-file-input-port file)
   (display "No test cases for open-file-input-port\n"))

(define (open-file-output-port-test)
   ;(open-file-output-port file)
   (display "No test cases for open-file-output-port\n"))

(define (read-bytes-from-file-test)
   ;(read-bytes-from-file amt in-port)
   (display "No test cases for read-bytes-from-file\n"))

(define (write-bytes-to-file-test)
   ;(write-bytes-to-file bytes out-port)
   (display "No test cases for write-bytes-to-file\n"))

(define (pipe-remaining-bytes-test)
   ;(pipe-remaining-bytes in out)
   (display "No test cases for pipe-remaining-bytes\n"))

(define (read-file-into-bytestring-test)
   ;(read-file-into-bytestring file)
   (display "No test cases for read-file-into-bytestring\n"))

(define (write-bytestring-to-file-test)
   ;(write-bytestring-to-file bytestring file)
   (display "No test cases for write-bytestring-to-file\n"))

(define (finalize-input-port-test)
   ;(finalize-input-port input)
   (display "No test cases for finalize-input-port\n"))

(define (finalize-output-port-test)
   ;(finalize-output-port output)
   (display "No test cases for finalize-output-port\n"))

(define (is-wav?-test)
   ;(is-wav? file)
   (display "No test cases for is-wav?\n"))

(define (is-mp3?-test)
   ;(is-mp3? file)
   (check-false (is-mp3? "testwav.wav"))
   (check-true (is-mp3? "testmp3.mp3")))

(define (wav->mp3-test)
   ;(wav->mp3 wav mp3)
   (check-true (is-wav? "testwav.wav"))
   (check-false (is-wav? "testmp3.mp3")))

(define (mp3->wav-test)
   ;(mp3->wav mp3 wav)
   (display "No test cases for mp3->wav\n"))

(define (test-file-writer-reader)
   (display "No test cases for test-file-writer-reader\n"))
   ; Fileio should have no knowledge of wavfiles therefor they cannot be used for testing
   ;    (let [(sample-wavfile (create-test-wavfile))]
   ;         (vector-set! (vector-ref (wavfile-samples sample-wavfile) 0) 0 -44)
   ;         (vector-set! (vector-ref (wavfile-samples sample-wavfile) 0) 1 13533)
   ;         (wavfile->file sample-wavfile "/tmp/_test")
   ;         (let [(w (file->wavfile "/tmp/_test"))]
   ;              (check-equal? (wavfile-endianess sample-wavfile) (wavfile-endianess w))
   ;              (check-equal? (wavfile-audioformat sample-wavfile) (wavfile-audioformat w))
   ;              (check-equal? (wavfile-channels sample-wavfile) (wavfile-channels w))
   ;              (check-equal? (wavfile-samplerate sample-wavfile) (wavfile-samplerate w))
   ;              (check-equal? (wavfile-byterate sample-wavfile) (wavfile-byterate w))
   ;              (check-equal? (wavfile-blockalign sample-wavfile) (wavfile-blockalign w))
   ;              (check-equal? (wavfile-bytespersample sample-wavfile) (wavfile-bytespersample w))
   ;              (check-equal? (wavfile-chunkstart sample-wavfile) (wavfile-chunkstart w))
   ;              (check-equal? (wavfile-chunksize sample-wavfile) (wavfile-chunksize w))
   ;              (check-equal? (wavfile-samples sample-wavfile) (wavfile-samples w)))))
