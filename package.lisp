(defpackage pofo
  (:use :common-lisp)
  (:export forward))
(in-package :pofo)

(deftype octet () '(unsigned-byte 3))
(deftype host-spec () '(or (vector t 4) string))
(deftype port-number () '(mod #x10000))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *registered*)