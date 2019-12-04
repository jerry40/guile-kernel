(define-module (hmac)
  #:use-module (gcrypt hmac)
  #:use-module (gcrypt base16)
  #:use-module (rnrs bytevectors)
  #:export (get-signature))

(define (get-signature key str)
  "Return a hexadecimal string containing the SHA256 HMAC of STR, a string,
with KEY, another string."
  (bytevector->base16-string
   (sign-data key (string->utf8 str)
              #:algorithm 'sha256)))
