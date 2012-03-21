(load "src/fileio.rkt")
(load "src/util.rkt")
(load "src/phase-coder.rkt")

(define (decode-payload-from-carrier carrier output)
    (write-bytestring-to-file (decode-payload-from-wav (file->wavfile carrier)) output))

(define (decode-payload-from-wav wav)
    (letrec* [(bytes (make-bytes 4))
              (curbyte (make-vector 8))
              (fill-byte (lambda (b c)
                                 (if (= b (bytes-length bytes))
                                          bytes
                                          (if (= c 8)
                                              (begin (bytes-set! bytes b (get-byte-from-bit-vector curbyte))
                                                     (fill-byte (+ b 1) 0))
                                              (begin (vector-set! curbyte c (decode-next-bit wav))
                                                     (fill-byte b (+ c 1)))))))]
             (fill-byte 0 0)))


(define (decode-next-bit wav)
    (let* [(samples (vector-ref (wavfile-samples wav) 0))
           (i (get-next-sample-index samples))
           (frequencies (fft (vector-copy samples i (+ i samples-per-fft))))]
          (get-bit-from-frequency (vector-ref frequencies (get-fundamental-frequency frequencies)))))
