

;(define (get-signature signature-scheme key str-list)
;  (

(use-modules (ice-9 popen))
   (use-modules (ice-9 rdelim))
   
(let ((port (open-input-output-pipe "openssl dgst -sha256 -hmac aaa")))
  (display "aaa" port)
  (force-output port)
  (read port)
  )


   (let* ((p2c (pipe))
           (port (with-input-from-port (car p2c)
                   (lambda ()
                     (open-input-pipe "read line && echo $line | openssl dgst -sha256 -hmac aaa")))))
      (display "aaa" (cdr p2c))
      (force-output (cdr p2c))
      (let ((result (read port)))
	(close-port (cdr p2c))
	(close-pipe port)
	result))
