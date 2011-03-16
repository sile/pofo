(in-package :pofo)

(defun forward (in out)
  (while (listen in)
    (write-byte (read-byte in) out))
  (force-output out))
  
