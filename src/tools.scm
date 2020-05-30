
(define (make-id)
  (number->string (random (expt 2 128)) 16))

(define* ((make-header user session version) msg-type)
  (scm->json-string `(("msg_id"   . ,(make-id))
                      ("username" . ,user)
                      ("session"  . ,session)
                      ("msg_type" . ,msg-type)
                      ("version"  . ,version))))

(define* (list->string ls #:optional (sep " "))
  (string-concatenate (map (lambda (x) (string-append (with-output-to-string (lambda () (write x))) sep)) ls)))

(define (colorize string)
  (string-append "\x1b[0;31m" string "\x1b[0;32m"))
