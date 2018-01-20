(use-modules (simple-zmq)
	     (json)
	     (srfi srfi-1)
             (srfi srfi-13))
	     
;(include "zmq.scm")
(include "tools.scm")

(define DELIM "<IDS|MSG>")

(define KERNEL-INFO '(("protocol_version" . "5.3.0")
		      ("implementation" . "guile Jupyter kernel")
		      ("implementation_version" . "0.0.1")
		      ("language_info" .
		       (("name" . "guile")
			("version" . "2.0.0")
			("mimetype" . "application/x-scheme")
			("file_extension" . ".scm")
			("pygments_lexer" . "scheme")
			("codemirror_mode" . "scheme")))
		      ("banner" . "Guile kernel")
;;				 ("help-links" . (("" . "")))
		      ))

(define notebook-info (json->scm (open-input-file (car (last-pair (command-line))))))

(define (get-notebook-info-atom name) (hash-ref notebook-info name))

;; parse json values
(define notebook-info-control-port     (get-notebook-info-atom "control_port"))
(define notebook-info-shell-port       (get-notebook-info-atom "shell_port"))
(define notebook-info-transport        (get-notebook-info-atom "transport"))
(define notebook-info-signature-scheme (get-notebook-info-atom "signature_scheme"))
(define notebook-info-stdin-port       (get-notebook-info-atom "stdin_port"))
(define notebook-info-heartbeat-port   (get-notebook-info-atom "hb_port"))
(define notebook-info-ip               (get-notebook-info-atom "ip"))
(define notebook-info-iopub-port       (get-notebook-info-atom "iopub_port"))
(define notebook-info-key              (get-notebook-info-atom "key"))

;; execute counter closure
(define execute-counter (let ((x 1)) (lambda () (begin (set! x (+ x 1)) x))))

(define (create-address port) (string-append notebook-info-transport "://" notebook-info-ip ":" (number->string port)))

;; zeromq context
(define context (zmq-create-context))

;; adresses
(define addr-heartbeat (create-address notebook-info-heartbeat-port))
(define addr-shell     (create-address notebook-info-shell-port))
(define addr-control   (create-address notebook-info-control-port))
(define addr-iopub     (create-address notebook-info-iopub-port))
(define addr-stdin     (create-address notebook-info-stdin-port))

;; sockets
(define socket-heartbeat (zmq-create-socket context 'ZMQ_REP))
(define socket-shell     (zmq-create-socket context 'ZMQ_ROUTER))
(define socket-control   (zmq-create-socket context 'ZMQ_ROUTER))
(define socket-iopub     (zmq-create-socket context 'ZMQ_PUB))
(define socket-stdin     (zmq-create-socket context 'ZMQ_ROUTER))

;; useful lists
(define adresses         (list addr-heartbeat addr-shell addr-control addr-iopub addr-stdin))
(define sockets          (list socket-heartbeat socket-shell socket-control socket-iopub socket-stdin))

;; bind sockets to addressess
(for-each zmq-bind-socket sockets adresses)

;(define hb-message (zmq-msg-init))
;(define shell-message (zmq-msg-init))
;(define control-message (zmq-msg-init))
;(define iopub-message (zmq-msg-init))
;(define stdin-message (zmq-msg-init))


(define (send socket uuid header parent-header metadata content)
  (zmq-send-msg-parts socket (list uuid DELIM "" header parent-header metadata content)))

(define (pub header- state parent-header)
  (send socket-iopub ""	(header- "status") parent-header "{}" (scm->json-string `(("execution_state" . ,state)))))

(define (pub-busy header- parent-header)
  (pub header- "busy" parent-header))

(define (pub-idle header- parent-header)
  (pub header- "idle" parent-header))

(define (heartbeat-handler)
  (zmq-message-send socket-heartbeat (zmq-message-receive socket-heartbeat (zmq-msg-init)))
  (heartbeat-handler))

(define (general-handler socket)
  (let* ((parts (zmq-get-msg-parts socket))
	 (wire-uuid          (car parts))
	 (wire-delimiter     (cadr parts))
	 (wire-signature     (caddr parts))
	 (wire-header        (json-string->scm(list-ref parts 3)))
	 (wire-parent-header (list-ref parts 4))
	 (wire-metadata      (json-string->scm(list-ref parts 5)))
	 (wire-content       (json-string->scm(list-ref parts 6))))
    (let ((msg-type      (hash-ref wire-header "msg_type"))
	  (msg-username  (hash-ref wire-header "username"))
	  (msg-session   (hash-ref wire-header "session"))
	  (msg-version   (hash-ref wire-header "version")))
    (let ((header- (make-header msg-username msg-session msg-version)))
      (pub-busy header- (scm->json-string wire-header))
      ((dispatch msg-type) socket wire-uuid header- (scm->json-string wire-header) (scm->json-string wire-metadata) wire-content)
      (pub-idle header- (scm->json-string wire-header))
      )))
    (general-handler socket))


(define (reply-kernel-info-request socket uuid header- parent-header metadata content)
  (send socket uuid (header- "kernel_info_reply") parent-header metadata (scm->json-string KERNEL-INFO)))

(define (reply-execute-request socket uuid header- parent-header metadata content)
  (let ((code              (string-append "(begin " (hash-ref content "code") ")")) ;; make one s-expression from possible list
	(silent            (hash-ref content "silent"))
	(store-history     (hash-ref content "store_history"))
	(user-expressions  (hash-ref content "user_expressions"))
	(allow-stdin       (hash-ref content "allow_stdin"))
	(stop-on-error     (hash-ref content "stop_on_error"))
	(empty-object      (make-hash-table 1))
	(counter           (execute-counter)))
    (let ((err #f)
	  (err-key #f)
	  (evalue #f)
	  (stacktrace #f)
	  (result #f))
      (catch #t
	     ;; evaluate code
	     (lambda ()
		      (set! result (with-output-to-string (lambda () (write (eval (with-input-from-string code read) (interaction-environment)))))))
	     ;; get error message in case of an exception
	     (lambda (key . parameters)
	       (set! err #t) 
	       (set! err-key (with-output-to-string (lambda () (display key))))
	       (set! evalue (with-output-to-string (lambda () (display parameters))))
	       (set! stacktrace (list (colorize err-key) evalue))
	       )
	     ;; Capture the stack here:
;	     (lambda (key . parameters)
	       ;;(set! stacktrace (map colorize (make-stack #t))))
;	       (set! stacktrace (list (car parameters) err-key evalue))
					;	       )
	     )
      (send socket-iopub uuid (header- "execute_input") parent-header metadata (scm->json-string `(("code" . ,code)
												   ("execution_count" . ,counter))
												 ))
      (when err
	    (send socket-iopub uuid (header- "error") parent-header metadata (scm->json-string `(("ename" . ,err-key)
												 ("evalue" . ,evalue)
												 ("traceback" . ,stacktrace)
												 )))
	    (send socket uuid (header- "execute_reply") parent-header metadata (scm->json-string `(("status" . "error")
												   ("execution_count" . ,counter)
												   ("ename" . ,err-key)
												   ("evalue" . ,evalue)
												   ("traceback" . ,stacktrace)
												   ("payload" . [])
												   ("user_expressions" . ,empty-object)
												   ))))
      (unless err
	      (send socket uuid (header- "execute_reply") parent-header metadata (scm->json-string `(("status" . "ok")
												     ("execution_count" . ,counter)
												     ("payload" . [])
												     ("user_expressions" . ,empty-object)
												     )))
	  
	    (send socket-iopub uuid (header- "execute_result") parent-header metadata (scm->json-string `(("data" .  (("text/plain" . ,result)))
													  ("metadata" . ,empty-object)
													  ("execution_count" . ,counter))
													))))))

(define (shutdown socket uuid header- parent-header metadata content)
  (for-each zmq-close-socket sockets)
  (zmq-destroy-context context)
  (quit))


(define dispatch-route
  `(("kernel_info_request" . ,reply-kernel-info-request)
    ("execute_request"     . ,reply-execute-request)
    ("shutdown_request"    . ,shutdown)
    ))

(define (dispatch msg-type)
  (let ((res (assoc-ref dispatch-route msg-type)))
    (unless res
	    (display "\n----------------------->")
	    (display msg-type)
	    (display "\n"))
    res))


 (general-handler socket-shell)

;(parallel
; (heartbeat-handler)
; (general-handler socket-shell)
; (general-handler socket-control)
;; (general-handler socket-iopub)
; (general-handler socket-stdin)
; )

;(par-map general-handler sockets messages)

;(display "Waiting")

;(while #t)

;(display addr-heartbeat)
;(zmq-get-socket-type 'ZMQ_PUSH)
;(define s (zmq-create-socket context 'ZMQ_REQ))
;(zmq-bind-socket s addr-heartbeat)
;(zmq-bind-socket socket-heartbeat addr-heartbeat) 
;(define m (zmq-message-receive socket-heartbeat))
;(display "Ok!!")
;(display (pointer->string (zmq-message-content m)))
;(zmq-message-send socket-heartbeat (zmq-msg-init))
;(define m (zmq-message-receive socket-heartbeat))
					;(display "Ok!!")

;(display addr-heartbeat)
;(define s (zmq-create-socket context 'ZMQ_REP))
;(zmq-bind-socket s addr-heartbeat) ; "tcp://127.0.0.1:5555")
;(define m (zmq-msg-init))
;(while #t
;       (display "Listening:")
;       (zmq-message-receive s m)
;       (display "Ok!!")
;       (display (pointer->string (zmq-message-content m)))
;       (zmq-message-send s m)
;       )
