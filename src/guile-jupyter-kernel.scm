(use-modules (simple-zmq)
	     (json)
             (hmac)
	     (srfi srfi-1)
             (srfi srfi-13)
			 (ice-9 hash-table))

(include "tools.scm")

(define DELIM "<IDS|MSG>")

(define KERNEL-INFO '(("protocol_version" . "5.3.0")
		      ("implementation" . "guile Jupyter kernel")
		      ("implementation_version" . "0.0.2")
		      ("language_info" .
		       (("name" . "guile")
			("version" . "2.0.0")
			("mimetype" . "application/x-scheme")
			("file_extension" . ".scm")
			("pygments_lexer" . "scheme")
			("codemirror_mode" . "scheme")))
		      ("banner" . "Guile kernel")
		      ("help_links" . (("GitHub" . "https://github.com/jerry40/guile-kernel")))
		      ))

(define notebook-info (json->scm (open-input-file (car (last-pair (command-line))))))

(define (get-notebook-info-atom name) (assoc-ref notebook-info name))

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

;; execution counter closure
(define execution-counter (let ((x 0)) (lambda () (begin (set! x (+ x 1)) x))))

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
(define socket-heartbeat (zmq-create-socket context ZMQ_REP))
(define socket-shell     (zmq-create-socket context ZMQ_ROUTER))
(define socket-control   (zmq-create-socket context ZMQ_ROUTER))
(define socket-iopub     (zmq-create-socket context ZMQ_PUB))
(define socket-stdin     (zmq-create-socket context ZMQ_ROUTER))

;; useful lists
(define adresses         (list addr-heartbeat addr-shell addr-control addr-iopub addr-stdin))
(define sockets          (list socket-heartbeat socket-shell socket-control socket-iopub socket-stdin))

;; bind sockets to addressess
(for-each zmq-bind-socket sockets adresses)

(define (send socket uuid header parent-header metadata content)  
    (let ((signature (get-signature notebook-info-key
				    (string-append header
						   parent-header
						   metadata
						   content))))
      (zmq-send-msg-parts-bytevector socket
				     (list uuid
					   (string->bv DELIM)
					   (string->bv signature)
					   (string->bv header)
					   (string->bv parent-header)
					   (string->bv metadata)
					   (string->bv content)))))

(define (pub header- state parent-header)
    (send socket-iopub
	  #vu8(65)
	  (header- "status")
	  parent-header
	  "{}"
	  (scm->json-string `(("execution_state" . ,state)))))

(define (pub-busy header- parent-header)
    (pub header- "busy" parent-header))

(define (pub-idle header- parent-header)
    (pub header- "idle" parent-header))

(define (heartbeat-handler)
    (zmq-message-send-bytevector socket-heartbeat
				 (zmq-message-receive-bytevector socket-heartbeat
								 (zmq-msg-init)))
  (heartbeat-handler))

(define (json-or-empty alist)
	(if (eqv? '() alist)
		"{}"
		(scm->json-string alist)))

(define (general-handler socket)
    (let* ((parts (zmq-get-msg-parts-bytevector socket))
	   (wire-uuid          (car parts))
	   (wire-delimiter     (bv->string (cadr parts)))
	   (wire-signature     (bv->string (caddr parts)))
	   (wire-header        (json-string->scm (bv->string (list-ref parts 3))))
	   (wire-parent-header (bv->string (list-ref parts 4)))
	   (wire-metadata      (json-string->scm (bv->string (list-ref parts 5))))
	   (wire-content       (json-string->scm (bv->string (list-ref parts 6)))))
      (let ((msg-type      (assoc-ref wire-header "msg_type"))
	    (msg-username  (assoc-ref wire-header "username"))
	    (msg-session   (assoc-ref wire-header "session"))
	    (msg-version   (assoc-ref wire-header "version")))
	(let ((header- (make-header msg-username msg-session msg-version)))
	  (pub-busy header- (json-or-empty wire-header))
	  ((dispatch msg-type) socket wire-uuid header- (json-or-empty wire-header) (json-or-empty wire-metadata) wire-content)
	  (pub-idle header- (json-or-empty wire-header))
	  )))
  (general-handler socket))

;; unknown request type, ignore it
(define (ignore-request socket uuid header- parent-header metadata content) #f)

;; send kernel-info
(define (reply-kernel-info-request socket uuid header- parent-header metadata content)
    (send socket
	  uuid
	  (header- "kernel_info_reply")
	  parent-header
	  metadata
	  (scm->json-string KERNEL-INFO)))

(define (reply-execute-request socket uuid header- parent-header metadata content)
    (let ((code              (string-append    "(begin " (assoc-ref content "code") ")")) ;; make one s-expression from possible list
	  (silent            (assoc-ref content "silent"))
	  (store-history     (assoc-ref content "store_history"))
	  (user-expressions  (assoc-ref content "user_expressions"))
	  (allow-stdin       (assoc-ref content "allow_stdin"))
	  (stop-on-error     (assoc-ref content "stop_on_error"))
	  (empty-object      (make-hash-table 1))
	  (counter           (execution-counter)))
      (let ((err #f)
	    (err-key #f)
	    (evalue #f)
	    (stacktrace #f)
	    (result #f)
	    (send- (lambda (socket msg-type content) (send socket uuid (header- msg-type) parent-header metadata (json-or-empty content))))
	    )
	(catch #t
	  ;; evaluate code + replace #undefined in case an expression returned nothing
	  (lambda ()
	    (let* ((str (with-output-to-string
			    (lambda ()
			      (write
			       (eval (with-input-from-string code read) (interaction-environment))))))
		   (tail (string-suffix-length "#<unspecified>" str)))
	      (set! result (if (> tail 0)
			       (string-drop-right str tail)
			       str))))

	  ;; get error message in case of an exception
	  (lambda (key . parameters)
	    (set! err #t) 
	    (set! err-key (with-output-to-string (lambda () (display key))))
	    (set! evalue (with-output-to-string (lambda () (display parameters))))
	    (set! stacktrace (vector err-key evalue)))
	  ;; Capture the stack here:
	  )
	  
	(send- socket-iopub "execute_input" `(("code" . ,code)
					      ("execution_count" . ,counter)))	     
	(when err
	  (send- socket-iopub "error"       `(
		  				  ("ename" . ,err-key)
					      ("evalue" . ,evalue)
					      ("traceback" . ,stacktrace)
						))
	  (send- socket      "execute_reply" `(("status" . "error")
					       ("execution_count" . ,counter)
					       ("ename" . ,err-key)
					       ("evalue" . ,evalue)
					       ("traceback" . ,stacktrace)
						   )))
	(unless err
	  (send- socket       "execute_reply"   `(("status" . "ok")
						  ("execution_count" . ,counter)
						  ))
	  (unless (string-null? result)
	    (send- socket-iopub "execute_result"  `(("data" .  (("text/plain" . ,(with-output-to-string (lambda () (display result))))))
						    ("execution_count" . ,counter))))))))

(define (shutdown socket uuid header- parent-header metadata content)
    (for-each zmq-close-socket sockets)
  (zmq-destroy-context context)
  (quit))

(define dispatch-route
    `(("kernel_info_request" . ,reply-kernel-info-request)
      ("execute_request"     . ,reply-execute-request)
      ("shutdown_request"    . ,shutdown)
      ("comm_info_request"   . ,ignore-request)
      ))

(define (dispatch msg-type)
    (let ((res (assoc-ref dispatch-route msg-type)))
      (unless res
	(display
	 (string-append "\n(WW) unknown message type: " msg-type "\n\n")))
      (if res res ignore-request)))

(general-handler socket-shell)
