(in-package :pofo)

(defun forward (in out)
  (while (listen in)
    (write-byte (read-byte in) out))
  (force-output out))
  
(defmacro set-input-handler (stream registered-stream-count &body body)
  (let ((fd (gensym))
        (handler (gensym)))
    `(let ((,handler))
       (incf ,registered-stream-count)
       (setf ,handler
             (sb-sys:add-fd-handler
              (sb-sys:fd-stream-fd ,stream)
              :input
              (lambda (,fd)
                (declare (ignore ,fd))
                (if (not (listen ,stream))
                    (progn (sb-sys:remove-fd-handler ,handler)
                           (decf ,registered-stream-count))
                  (while (listen ,stream)
                    ,@body))))))))

(defmacro do-wait-for-input ((inputable-stream &rest streams) &body body)
  (let ((registered-stream-count (gensym)))
    `(let ((,registered-stream-count 0))
       ,@(mapcar (lambda (stream)
                   `(let ((,inputable-stream ,stream))
                      (set-input-handler ,inputable-stream ,registered-stream-count 
                        ,@body)))
                 streams)
       (while (plusp ,registered-stream-count)
         (sb-sys:serve-all-events)
         (sleep 0.01)))))

;; TODO: socketが切断された場合のハンドリング追加
(defun start-forward (&key from-host from-port to-host to-port thread)
  (declare (address-spec from-host to-host)
           (positive-fixnum from-port to-port))
  (flet ((main ()
           (with-server (proxy from-host from-port)
             (do-accept (client proxy)
               (with-client (server to-host to-port)
                 (with-stream (client-stream client)
                   (with-stream (server-stream server)
                     (do-wait-for-input (in client-stream server-stream)
                       (if (eq in client-stream)
                           (forward client-stream server-stream)
                         (forward server-stream client-stream))))))))))
    (if thread
        (sb-thread:make-thread (lambda () (main)))
      (main))))
