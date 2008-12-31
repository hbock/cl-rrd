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

;;; Some definitions:
;;; DS: Data source
;;; DST: Data source type.
;;; RRA: Round robin archive.
;;; PDP: Primary data point.
;;; CF: consolidating function.

(defclass rrd ()
  ((file :initarg :file
	 :accessor rrd-file
	 :initform "")
   (start :initarg :start
	  :accessor rrd-start
	  :initform 0)
   (step :initarg :step
	 :accessor rrd-step)
   (data-sources :initarg :data-sources
		 :accessor rrd-data-sources)
   (archives :initarg :archives
	     :accessor rrd-archives)))

(defclass rra ()
  ((cf-name :initarg :cf-name)
   (xff :initarg :xff)
   (steps :initarg :steps)
   (rows :initarg :rows)))

(defmacro define-database (name (&key (start nil startp) (step nil stepp))
			   &rest spec-list)
  (let ((filename (format nil "~a.rrd" (to-variable-name name)))
	ds-list rra-list)
    (dolist (spec spec-list)
      (case (first spec)
	(:data-source
	 (push (apply #'ds-string (rest spec)) ds-list))
	(:archive
	 (push (apply #'rra-string (rest spec)) rra-list))))
    filename))

(defun ds-string (name type &rest dst-spec)
  "Create a string used by RRD to define a data source (DS)."
  (declare (type symbol name type))
  (let ((ds-string (format nil "DS:~a:~a:" (to-variable-name name) type)))
    (ecase type
      (:compute
       (strcat ds-string (compile-rpn (first dst-spec))))
      ((:gauge :counter :derive :absolute)
       (destructuring-bind (heartbeat &optional (min 0) max) dst-spec
	 (when (and max (> min max))
	   (error "Minimum DS value (~d) is greater than declared maximum (~d)!" min max))
	 (strcat ds-string (format nil "~d~@[:~d~]~@[:~d~]" heartbeat min max)))))))

(defun rra-string (cf &key (xff 0.5) (steps 1) rows)
  (declare (type real xff)
	   (type (integer 1 *) steps rows))
  (unless (and (> 1.0 xff) (< 0.0 xff))
    (error "XFF must be between 0 and 1 (inclusive)."))
  (let ((valid-cf-names (list :average :min :max :last)))
    (unless (member cf valid-cf-names)
      (error "Invalid consolidation function specified. Expected one of ~{~a~^, ~}." valid-cf-names)))
  (format nil "RRA:~a:~f:~d:~d" (string cf) (float xff) steps rows))

(defun clear-errors ()
  (%rrd-clear-error))

(defun create (filename ds-list rra-list &key (start nil startp) (step nil stepp))
  "Create a new RRD database."
  (assert (or (not stepp) (> step 0)))
  (assert (or (not startp) (> start 0)))

  (clear-errors)
  (librrd-call #'%rrd-create
	       (cons (namestring filename)
		     (append
		      (when startp (list "--start" (to-string start)))
		      (when stepp (list "--step" (to-string step)))
		      ds-list rra-list))))

(defun update (filename update-list)
  (declare (type list update-list))
  (clear-errors)
  (librrd-call #'%rrd-update
	       (cons (namestring filename)
		     (append
		      (loop :for (time value) :in update-list
			 :collect (format nil "~d:~a" time value))))))
