(defpackage pofo
  (:use :common-lisp)
  (:export foward))
(in-package :pofo)

(deftype octet () '(unsigned-byte 3))
(deftype address-spec () '(or (vector t 4) string))

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
