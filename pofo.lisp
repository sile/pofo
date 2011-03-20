(in-package :pofo)

(defun resolve-address (hostname-or-ipaddress &aux (host hostname-or-ipaddress))
  (declare (host-spec host))
  (if (stringp host)
      (first (sb-bsd-sockets:host-ent-addresses 
              (sb-bsd-sockets:get-host-by-name host)))
    host))

(defmacro with-server ((server host port &key (listen-number 30)) &body body)
  `(let ((,server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
     (setf (sb-bsd-sockets:sockopt-reuse-address ,server) t)
     (sb-bsd-sockets:socket-bind ,server (resolve-address ,host) ,port)
     (sb-bsd-sockets:socket-listen ,server ,listen-number)
     (unwind-protect 
         (locally ,@body)
       (sb-bsd-sockets:socket-close ,server))))

(defun make-client-socket (host port)
  (let ((client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect client (resolve-address host) port)
    client))
                
(defun make-socket-stream (socket)
  (sb-bsd-sockets:socket-make-stream socket 
                                     :input t :output t
                                     :serve-events t
                                     :element-type 'octet))

(declaim (inline do-accept-impl))
(defun do-accept-impl (server worker-number body-fn)
  (let* ((accepted-socket-queue (sb-concurrency:make-queue))
         (worker-threads
          (loop REPEAT worker-number
                COLLECT
                (sb-thread:make-thread
                 (lambda (&aux (*registered* (make-hash-table :test #'eq)))
                   (unwind-protect
                       (loop FOR client = (sb-concurrency:dequeue accepted-socket-queue)
                             DO (when client
                                  (funcall body-fn client))
                                (sb-sys:serve-all-events 0.005))
                     (loop FOR clean-up-function BEING THE HASH-VALUES OF *registered*
                           DO (funcall clean-up-function))))))))
    (unwind-protect
        (loop FOR client = (sb-bsd-sockets:socket-accept server)
              DO (sb-concurrency:enqueue client accepted-socket-queue))
      (progn
        (dolist (worker worker-threads)
          (when (sb-thread:thread-alive-p worker)
            (sb-thread:terminate-thread worker)))
        (dolist (client (sb-concurrency:list-queue-contents accepted-socket-queue))
          (sb-bsd-sockets:socket-close client))))))

(defmacro do-accept ((client server &key worker-number) &body body)
  `(do-accept-impl ,server ,worker-number
                   (lambda (,client) 
                     (handler-case 
                      (locally ,@body)
                      (error ()
                        (sb-bsd-sockets:socket-close ,client))))))

(defmacro register (stream handler &body body)
  (let ((fd (gensym)))
    `(setf ,handler
           (sb-sys:add-fd-handler
            (sb-sys:fd-stream-fd ,stream)
            :input
            (lambda (,fd)
              (declare (ignorable ,fd))
              ,@body)))))

(declaim (inline handle-input-event))
(defun handle-input-event (from-stream to-stream &key on-forward on-backward)
  (let ((from-handler)
        (to-handler))
    (labels ((clean-up ()
               (remhash #'clean-up *registered*)
               (sb-sys:remove-fd-handler from-handler)
               (sb-sys:remove-fd-handler to-handler)
               (close from-stream)
               (close to-stream))
             (funcall-with-clean-up (fn stream)
               (handler-case 
                (if (not (listen stream))
                    (clean-up)
                  (progn (funcall fn) t))
                (error ()
                  (clean-up)))))
      (setf (gethash #'clean-up *registered*) #'clean-up)
      (register from-stream from-handler (funcall-with-clean-up on-forward from-stream))
      (register   to-stream   to-handler (funcall-with-clean-up on-backward to-stream)))))

(declaim (inline stream-forward))
(defun stream-forward (in out)
  (loop WHILE  (listen in)
        DO (write-byte (read-byte in) out))
  (force-output out))

(defun forward (from-host from-port to-host to-port &key thread (workers 1))
  (declare (host-spec from-host to-host)
           (positive-fixnum from-port to-port)
           (positive-fixnum workers))
  (flet ((main ()
           (with-server (proxy from-host from-port)
             (do-accept (from proxy :worker-number workers)
               (let* ((to (make-client-socket to-host to-port))
                      (from-stream (make-socket-stream from))
                      (to-stream   (make-socket-stream to)))
                 (declare #.*fastest*)
                 (handle-input-event from-stream to-stream
                   :on-forward  (lambda () (stream-forward from-stream to-stream))
                   :on-backward (lambda () (stream-forward to-stream from-stream))))))))
    (if (not thread)
        (main)
      (sb-thread:make-thread #'main))))
