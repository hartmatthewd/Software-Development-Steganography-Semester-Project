(define (run-frequencycontroller-tests)
   (load "src/frequencycontroller.rkt")

   (make-encoder-test)
   (make-decoder-test)
   (make-controller-test)
   (initialize-controller-test)
   (finalize-controller-test)
   (code-next-frequency-test)
   (ensure-destination-large-enough-test)
   (validate-payload-size-test)
   (get-max-payload-size-test)
   (sanitize-samples-test)
   (page-frequencies-test)
   (write-current-frequencies-test)
   (set-frequency-magnitude-order-test)
   (parse-frequencies-test)
   (page-samples-test)
   (write-current-samples-test)
   (get-next-samples-test))

(define (make-encoder-test)
   ;(make-encoder src)
   (display "No test cases for make-encoder\n"))

(define (make-decoder-test)
   ;(make-decoder src dest len)
   (display "No test cases for make-decoder\n"))

(define (make-controller-test)
   ;(make-controller src dest)
   (display "No test cases for make-controller\n"))

(define (initialize-controller-test)
   ;(initialize-controller controller)
   (display "No test cases for initialize-controller\n"))

(define (finalize-controller-test)
   ;(finalize-controller controller)
   (display "No test cases for finalize-controller\n"))

(define (code-next-frequency-test)
   ;(code-next-frequency controller func)
   (display "No test cases for code-next-frequency\n"))

(define (ensure-destination-large-enough-test)
   ;(ensure-destination-large-enough controller size)
   (display "No test cases for ensure-destination-large-enough\n"))

(define (validate-payload-size-test)
   ;(validate-payload-size controller size)
   (display "No test cases for validate-payload-size\n"))

(define (get-max-payload-size-test)
   ;(get-max-payload-size controller)
   (display "No test cases for get-max-payload-size\n"))

(define (sanitize-samples-test)
   (check-equal? (sanitize-samples (vector 1 2 3 4 5))
                 (vector 1 2 3 4 5))
   (check-equal? (sanitize-samples (vector 153.99+5e-10i 50000 -50000 -343.342+234.342i 5))
                 (vector 154 32767 -32768 -343 5)))

(define (page-frequencies-test)
   ;(page-frequencies controller)
   (display "No test cases for page-frequencies\n"))

(define (write-current-frequencies-test)
   ;(write-current-frequencies controller)
   (display "No test cases for write-current-frequencies\n"))

(define (set-frequency-magnitude-order-test)
   ;(set-frequency-magnitude-order controller)
   (display "No test cases for set-frequency-magnitude-order\n"))

(define (parse-frequencies-test)
   ;(parse-frequencies controller)
   (display "No test cases for parse-frequencies\n"))

(define (page-samples-test)
   ;(page-samples controller)
   (display "No test cases for page-samples\n"))

(define (write-current-samples-test)
   ;(write-current-samples controller)
   (display "No test cases for write-current-samples\n"))

(define (get-next-samples-test)
   ;(get-next-samples controller)
   (display "No test cases for get-next-samples\n"))
