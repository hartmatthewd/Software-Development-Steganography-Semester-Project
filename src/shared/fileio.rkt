; Attepts to read the given file, returning a bytestring containing the file's bytes
; If the file does not exist, throws an error
(define (read-file-into-bytestring file)
   (if (file-exists? file)
       (file->bytes file)
       (error (string-append "File " file " does not exist"))))

; Writes the given bytevector to a file of the given name
(define (write-bytestring-to-file bytestring file)
   (if (file-exists? file)
       (delete-file file)
       '())
   (display-to-file bytestring file))
