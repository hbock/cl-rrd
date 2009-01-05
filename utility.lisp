;;;; cl-rrd - Common Lisp bindings to librrd2
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

(defun to-string (value)
  (etypecase value
    (integer (format nil "~d" value))
    (real   (format nil "~6$" value))
    (symbol (string value))
    (string value)))

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun flatten (tree)
  (loop :for element :in tree
     :if (listp element) :append (flatten element)
     :else :collect element))

(defun unix-time (&optional (time (get-universal-time)))
  (declare (type integer time))
  (- time +unix-epoch+))