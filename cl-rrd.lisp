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

(defclass database ()
  ((file :initarg :file
	 :accessor rrd-file
	 :initform #P"")
   (start :initarg :start
	  :accessor rrd-start
	  :initform 0)
   (step :initarg :step
	 :accessor rrd-step)
   (data-sources :initarg :data-sources
		 :accessor rrd-data-sources
		 :initform nil)
   (archives :initarg :archives
	     :accessor rrd-archives
	     :initform nil)))

(defclass archive ()
  ((cf-name :initarg :cf-name)
   (xff :initarg :xff)
   (steps :initarg :steps)
   (rows :initarg :rows)))

(defmethod create ((rrd database))
  (with-slots (file start step data-sources archives) rrd
    (librrd-call
     #'%rrd-create
     (cons (namestring file)
	   (append
	    (when (slot-boundp rrd 'start)
	      (list "--start" (to-string (slot-value rrd 'start))))
	    (when (slot-boundp rrd 'step)
	      (list "--step" (to-string (slot-value rrd 'step))))	    
	    data-sources archives)))))

(defmethod update ((rrd database) update-list &key (template nil))
  (declare (type list template))
  (with-slots (file) rrd
    (librrd-call
     #'%rrd-update
     (cons (namestring file)
	   (append
	    (when template
	      (list "--template" (update-template-string template)))
	    (loop :for update-spec :in update-list
	       :collect (update-value-string update-spec)))))))

(defun update-template-string (template-list)
  (format nil "~{~a~^:~}"
	  (loop :for data-source :in template-list
	     :collect (to-variable-name data-source))))

(defun update-value-string (update-spec)
  (destructuring-bind (time &rest values) update-spec
    (declare (type (or keyword integer) time))
    (format nil "~a:~{~a~^:~}" (if (eql time :now) "N" (to-string time))
	    (mapcar #'to-string
		    (substitute-if "U" (lambda (value) (eql value :unknown)) values)))))

(defmacro with-database (name (filename &key (start nil startp) (step nil stepp)
					(if-does-not-exist :create)
					(if-exists :update)) spec-list &body body)
  `(let ((,name (make-instance 'database :file ,filename :start ,start :step ,step)))
     (with-slots (data-sources archives) ,name
       ,@(loop :for spec :in spec-list
	    :collect
	    (case (first spec)
	      (:data-source
	       `(push ,(apply #'ds-string (rest spec)) data-sources))
	      (:archive
	       `(push ,(apply #'rra-string (rest spec)) archives))))
       (setf data-sources (nreverse data-sources))
       (setf archives (nreverse archives))
       (if (probe-file ,filename)
	   ,(ecase if-exists
		   (:supersede `(create ,name))
		   (:update)
		   (:error (error 'file-error "RRD file ~a already exists.")))
	   ,(ecase if-does-not-exist
		   (:create `(create ,name))
		   (:error (error 'file-error "RRD file ~a does not exist."))))
       ,@body)))

(defun ds-string (name type &rest dst-spec)
  "Create a string used by RRD to define a data source (DS)."
  (declare (type symbol name type))
  (let ((ds-string (format nil "DS:~a:~a:" (to-variable-name name) type)))
    (ecase type
      (:compute
       (strcat ds-string (compile-rpn (first dst-spec))))
      ((:gauge :counter :derive :absolute)
       (destructuring-bind (heartbeat &optional min max) dst-spec
	 ;; (when (and max (>= min max))
	 ;;   (error "Minimum DS value (~a) is not less than specified maximum (~a)." min max))
	 (strcat ds-string (format nil "~d~@[:~d~]~@[:~d~]" heartbeat min max)))))))

(defun rra-string (consolidation-function &key (xff 0.5) (steps 1) rows)
  (declare (type real xff)
	   (type (integer 1 *) steps rows))
  (unless (and (> 1.0 xff) (< 0.0 xff))
    (error "XFF must be between 0 and 1 (inclusive)."))
  (ecase consolidation-function
    ((:average :min :max :last)
     (format nil "RRA:~a:~f:~d:~d"
	     (string-upcase consolidation-function) (float xff) steps rows))))
