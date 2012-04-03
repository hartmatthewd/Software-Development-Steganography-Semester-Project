;;;;;;;;;;;;;;;;;;
(define (run-fileio-tests)
   (load "src/fileio.rkt")

   (open-file-input-port-test)
   (open-file-output-port-test)
   (read-bytes-from-file-test)
   (write-bytes-to-file-test)
   (pipe-remaining-bytes-test)
   (finalize-input-port-test)
   (finalize-output-port-test)
   (bytes->file-test)
   (is-wav?-test)
   (is-mp3?-test)
   (wav->mp3-test)
   (mp3->wav-test))

;;;;;;;;;;;;;;;;;;
(define (open-file-input-port-test)
   ;(open-file-input-port file)
   (display "No test cases for open-file-input-port\n"))

;;;;;;;;;;;;;;;;;;
(define (open-file-output-port-test)
   ;(open-file-output-port file)
   (display "No test cases for open-file-output-port\n"))

;;;;;;;;;;;;;;;;;;
(define (read-bytes-from-file-test)
   ;(read-bytes-from-file amt in-port)
   (display "No test cases for read-bytes-from-file\n"))

;;;;;;;;;;;;;;;;;;
(define (read-file-into-bytestring-test)
   ;(write-bytes-to-file bytes out-port)
   (display "No test cases for write-bytes-to-file\n"))

;;;;;;;;;;;;;;;;;;
(define (write-bytes-to-file-test)
   ;(write-bytes-to-file bytes out-port)
   (display "No test cases for write-bytes-to-file\n"))

;;;;;;;;;;;;;;;;;;
(define (pipe-remaining-bytes-test)
   (copy-file testwav tmpsrc #t)
   (let [(in (open-file-input-port tmpsrc))
         (out (open-file-output-port tmpdest))]
        (pipe-remaining-bytes in out)
        (finalize-input-port in)
        (finalize-output-port out))
   (check-equal? (file->bytes tmpsrc) (file->bytes tmpdest)))

;;;;;;;;;;;;;;;;;;
(define (finalize-input-port-test)
   ;(finalize-input-port input)
   (display "No test cases for finalize-input-port\n"))

;;;;;;;;;;;;;;;;;;
(define (finalize-output-port-test)
   ;(finalize-output-port output)
   (display "No test cases for finalize-output-port\n"))

;;;;;;;;;;;;;;;;;;
(define (bytes->file-test)
   ; (bytes->file bytes file)
   (display "No test cases for bytes->file\n"))

;;;;;;;;;;;;;;;;;;
(define (is-wav?-test)
   (check-true (is-wav? testwav))
   (check-false (is-wav? testmp3)))

;;;;;;;;;;;;;;;;;;
(define (is-mp3?-test)
   (check-false (is-mp3? testwav))
   (check-true (is-mp3? testmp3)))

;;;;;;;;;;;;;;;;;;
(define (wav->mp3-test)
   (wav->mp3 testwav tmpsrc)
   (check-true (is-mp3? tmpsrc)))

;;;;;;;;;;;;;;;;;;
(define (mp3->wav-test)
   (mp3->wav testmp3 tmpsrc)
   (check-true (is-wav? tmpsrc)))
