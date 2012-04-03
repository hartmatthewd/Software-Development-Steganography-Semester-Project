(define (run-frequencycoder-tests)
   (load "src/frequencycoder.rkt")

   (make-decoder-test)
   (make-encoder-test)
   (make-coder-test)
   (initialize-coder-test)
   (finalize-coder-test)
   (code-next-frequency-test)
   (ensure-destination-large-enough-test)
   (validate-payload-size-test)
   (get-max-payload-size-test)
   (sanitize-samples-test)
   (page-frequencies-test)
   (write-current-frequencies-test)
   (parse-frequencies-test)
   (page-samples-test)
   (write-current-samples-test)
   (get-next-samples-test))

(define (make-decoder-test)
   ;(make-decoder src)
   (display "No test cases for make-decoder\n"))

(define (make-encoder-test)
   ;(make-encoder src dest len)
   (display "No test cases for make-encoder\n"))

(define (make-coder-test)
   ;(make-coder src dest)
   (display "No test cases for make-coder\n"))

(define (initialize-coder-test)
   ;(initialize-coder coder)
   (display "No test cases for initialize-coder\n"))

(define (finalize-coder-test)
   ;(finalize-coder coder)
   (display "No test cases for finalize-coder\n"))

(define (code-next-frequency-test)
   ;(code-next-frequency coder func)
   (display "No test cases for code-next-frequency\n"))

(define (ensure-destination-large-enough-test)
   ;(ensure-destination-large-enough coder size)
   (display "No test cases for ensure-destination-large-enough\n"))

(define (validate-payload-size-test)
   ;(validate-payload-size coder size)
   (display "No test cases for validate-payload-size\n"))

(define (get-max-payload-size-test)
   ;(get-max-payload-size coder)
   (display "No test cases for get-max-payload-size\n"))

(define (sanitize-samples-test)
   (check-equal? (sanitize-samples (vector 1 2 3 4 5))
                 (vector 1 2 3 4 5))
   (check-equal? (sanitize-samples (vector 153.99+5e-10i 50000 -50000 -343.342+234.342i 5))
                 (vector 154 32767 -32768 -343 5)))

(define (page-frequencies-test)
   ;(page-frequencies coder)
   (display "No test cases for page-frequencies\n"))

(define (write-current-frequencies-test)
   ;(write-current-frequencies coder)
   (display "No test cases for write-current-frequencies\n"))

(define (parse-frequencies-test)
   ;(parse-frequencies coder)
   (display "No test cases for parse-frequencies\n"))

(define (page-samples-test)
   ;(page-samples coder)
   (display "No test cases for page-samples\n"))

(define (write-current-samples-test)
   ;(write-current-samples coder)
   (display "No test cases for write-current-samples\n"))

(define (get-next-samples-test)
   ;(get-next-samples coder)
   (display "No test cases for get-next-samples\n"))
