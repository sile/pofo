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

(defun make-client-socket (host port)
  (let ((client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect client (resolve-address host) port)
    client))
                
(defun make-socket-stream (socket)
  (sb-bsd-sockets:socket-make-stream socket 
                                     :input t :output t
                                     :serve-events t
                                     :element-type 'octet))

(defmacro do-accept ((client server map) &body body)
  (let ((worker-thread (gensym))
        (queue (gensym)))
    `(let* ((,queue (sb-concurrency:make-queue))
            (,worker-thread
             (sb-thread:make-thread
              (lambda () 
                (unwind-protect
                  (loop FOR ,client = (sb-concurrency:dequeue ,queue)
                        DO (when ,client
                             (print (list :client ,client) *error-output*)
                             (locally ,@body))
                        (sb-sys:serve-all-events 0.1))
                  (maphash (lambda (k close-all)
                             (declare (ignore k))
                             (funcall close-all))
                           ,map))))))
       (unwind-protect
           (loop FOR ,client = (sb-bsd-sockets:socket-accept ,server)
                 DO
                 (sb-concurrency:enqueue ,client ,queue))
         (progn
           (when (sb-thread:thread-alive-p ,worker-thread)
             (sb-thread:terminate-thread ,worker-thread))
           (dolist (,client (sb-concurrency:list-queue-contents ,queue))
             (sb-bsd-sockets:socket-close ,client)))))))

(defstruct info
  client
  client-stream
  server
  server-stream
  client-handler
  server-handler)

(defun close-info (info)
  (with-slots (client server client-handler server-handler) info
    (sb-bsd-sockets:socket-close client)
    (sb-bsd-sockets:socket-close server)
    (sb-sys:remove-fd-handler client-handler)
    (sb-sys:remove-fd-handler server-handler)
    ))

(defmacro register (stream handler &body body)
  (let ((fd (gensym)))
    `(setf ,handler
           (sb-sys:add-fd-handler
            (sb-sys:fd-stream-fd ,stream)
            :input
            (lambda (,fd)
              (declare (ignorable ,fd))
              ,@body)))))

(defun stream-forward (in out)
  (while (listen in)
    (write-byte (read-byte in) out))
  (force-output out))

(defmacro forward-loop ((from to map) &key forward backward)
  `(let* ((h-from)
          (h-to))
     (labels ((close-all ()
                (remhash #'close-all ,map)
                (print (list :close ,from ,to) *error-output*)
                (force-output *error-output*)
                (sb-sys:remove-fd-handler h-from)
                (sb-sys:remove-fd-handler h-to)
                (close ,from)
                (close ,to)))
     (setf (gethash #'close-all ,map) #'close-all)
     (print :reg1 *error-output*)
     (register ,from h-from (handler-case 
                             (if (not (listen ,from))
                                 (close-all)
                               ,forward)
                             (error (c)
                               (format *error-output* "DEBUG:~A~%" c)
                               (close-all))))
     (print (list :reg2 h-from) *error-output*)
     (register ,to   h-to   (handler-case 
                             (if (not (listen ,to))
                                 (close-all)
                               ,backward)
                             (error (c)
                               (format *error-output* "DEBUG:~A~%" c)
                               (close-all))))
     (print (list :reg-finish h-to) *error-output*)
     (force-output *error-output*)
     )))
  
(defun test2 (&aux (map (make-hash-table :test #'eq)))
  (with-server (proxy "localhost" 8008)
    (do-accept (client proxy map)
      (print :enter *error-output*)
      (handler-case 
       (let* ((server (make-client-socket "localhost" 5432))
              (cstm (make-socket-stream client))
              (sstm (make-socket-stream server)))
         (print :init *error-output*)
         (forward-loop (cstm sstm map)
           :forward  (stream-forward cstm sstm)
           :backward (stream-forward sstm cstm)))
       (error (C)
         (print (list :retne c) *error-output*)
         (sb-bsd-sockets:socket-close client))))))

