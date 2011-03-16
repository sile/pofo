(in-package :pofo)

(defmacro while (exp &body body)
  `(loop WHILE ,exp
         DO
         (locally ,@body)))
