(in-package :pofo)

(defun resolve-address (hostname-or-ipaddress &aux (host hostname-or-ipaddress))
  (declare (address-spec host))
  (if (stringp host)
      (first (sb-bsd-sockets:host-ent-addresses 
              (sb-bsd-sockets:get-host-by-name host)))
    host))

;; TODO: logger
(defmacro with-server ((server host port &key (listen-number 30)) &body body)
  (let ((addr (gensym)))
    `(let ((,addr (resolve-address ,host))
           (,server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
     (setf (sb-bsd-sockets:sockopt-reuse-address ,server) t)
     (sb-bsd-sockets:socket-bind ,server ,addr ,port)
     (sb-bsd-sockets:socket-listen ,server ,listen-number)
     (unwind-protect 
         (locally ,@body)
       (sb-bsd-sockets:socket-close ,server)))))

(defmacro with-client ((client host port) &body body)
  (let ((addr (gensym)))
    `(let ((,addr (resolve-address ,host))
           (,client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
       (sb-bsd-sockets:socket-connect ,client ,addr ,port)
       (unwind-protect
           (locally ,@body)
         (sb-bsd-sockets:socket-close ,client)))))

(defmacro with-stream ((stream socket) &body body)
  `(let ((,stream (sb-bsd-sockets:socket-make-stream ,socket 
                                                     :input t :output t
                                                     :serve-events t
                                                     :element-type 'octet)))
     (unwind-protect
         (locally ,@body)
       (close ,stream))))

;; NOTE: multi-thread version
;; TODO: single-thread版も試したい
(defmacro do-accept ((client server) &body body)
  `(loop
    (let ((,client (sb-bsd-sockets:socket-accept ,server)))
      (sb-thread:make-thread
       (lambda ()
         (unwind-protect
             (locally ,@body)
           (sb-bsd-sockets:socket-close ,client)))))))
