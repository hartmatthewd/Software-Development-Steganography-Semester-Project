; Attepts to read the given file, returning a byte vector containing its contents.
; If the file does not exist for is empty, throws an error
(define (read-file-into-bytevector file)
   (if (file-exists? file)
       (let ((input (open-file-input-port file)))
          (let ((bv (get-bytevector-all input)))
            (if (eof-object? bv)
                (error (string-append "File " file " is empty"))
                bv)))
       (error (string-append "File " file " does not exist"))))

; Writes the given bytevector to a file of the given name
(define (write-bytevector-to-file filename bytevector)
   (let ((file (open-file-output-port filename)))
      (put-bytevector file bytevector)
      (close-port file)))
