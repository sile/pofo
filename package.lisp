(defpackage pofo
  (:use :common-lisp)
  (:export start-forward))
(in-package :pofo)

(deftype octet () '(unsigned-byte 3))
(deftype address-spec () '(or (vector t 4) string))
(deftype positive-fixnum () '(integer 0 #.most-positive-fixnum))
(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))

