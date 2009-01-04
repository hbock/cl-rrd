;;;; cl-rrd - Common Lisp bindings to cl-rrd
;;;; Copyright (C) 2008 Harry Bock <harry@oshean.org>

;;;; This file is part of cl-rrd.

;;;; cl-rrd is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

;;;; cl-rrd is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-rrd; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
(in-package :cl-rrd)

(defun escape (string)
  "Escape a string suitable for legend and time arguments to rrdtool commands."
  (with-output-to-string (out)
    (loop :for i from 0 :below (length string)
       :for char = (elt string i)
       :do (case char
	     ((#\:) (write-string "\\:" out))
	     (otherwise (write-char char out))))))

(defun def-string (defname rrdfile ds-name cf &key (step nil) (start nil) (end nil) (reduce nil))
  (declare (type symbol defname ds-name cf))
  (strcat
   (format nil "DEF:~a=~a:~a:~a" (to-variable-name defname) rrdfile
	   (to-variable-name ds-name) cf)
   (if start (format nil ":start=~a" (escape (to-string start))) "")
   (if end (format nil ":end=~a" (escape (to-string end))) "")
   (if step (format nil ":step=~a" step) "")
   (if reduce (format nil ":reduce=~a" reduce) "")))

(defun vdef-string (name rpn)
  (declare (type symbol name))
  (format nil "VDEF:~a=~a" (to-variable-name name) (compile-rpn rpn)))

(defun cdef-string (name rpn)
  (declare (type symbol name))
  (format nil "CDEF:~a=~a" (to-variable-name name) (compile-rpn rpn)))