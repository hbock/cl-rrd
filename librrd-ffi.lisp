;;;; cl-rrdtool - Common Lisp bindings to cl-rrdtool
;;;; Copyright (C) 2008 Brandon Edens <brandon@cs.uri.edu>

;;;; This file is part of cl-rrdtool.

;;;; cl-rrdtool is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

;;;; cl-rrdtool is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-rrdtool; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
(in-package :cl-rrd)

(cffi:define-foreign-library librrd
  (:unix (:or "librrd.so.2" "librrd.so"))
  (t (:default "librrd")))
(cffi:use-foreign-library librrd)

(cffi:defcfun ("rrd_create" %rrd-create) :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("rrd_update" %rrd-update) :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("rrd_graph" %rrd-graph) :int
  (argc :int)
  (argv :pointer)
  (printout :pointer)
  (width :pointer)
  (height :pointer)
  (stream :pointer)
  (ymin :pointer)
  (ymax :pointer))

;;; librrd error functions.
(cffi:defcfun ("rrd_clear_error" %rrd-clear-error) :void)
(cffi:defcfun ("rrd_test_error" %rrd-test-error) :int)
(cffi:defcfun ("rrd_get_error" %rrd-get-error) :string)


;;; Functions we haven't really used yet.
(cffi:defcfun ("rrd_dump" %rrd-dump) :int
  (argc :int)
  (argv :pointer))
(cffi:defcfun ("rrd_fetch" %rrd-fetch) :int
  (argc :int)
  (argv :pointer))
(cffi:defcfun "rrd_last" :int
  (argc :int)
  (argv :pointer))
(cffi:defcfun "rrd_resize" :int
  (argc :int)
  (argv :pointer))
(cffi:defcfun "rrd_restore" :int
  (argc :int)
  (argv :pointer))
(cffi:defcfun "rrd_tune" :int
  (argc :int)
  (argv :pointer))

;;; SBCL for some reason sets odd floating point traps that make FFI calls blow up.
;;; This macro abstracts away the SBCL details.
(defmacro without-fp-traps (&body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-sbcl
  `(progn
     ,@body))

(defun foreign-alloc-list (list)
  "Take a list of strings and allocate an array of C-style strings."
  (declare (type list list))
  (cffi:foreign-alloc :string :initial-contents list :null-terminated-p t))

(defun rrd-call (cfun parameters &optional &key (debug t))
  "Call a standard RRD library function with specified parameters.
For use with RRD functions that take argc/argv type parameters only."
  (declare (type function cfun)
	   (type list parameters))
  (%rrd-clear-error)
  (let* ((argv (cons "dummy" parameters))
	 (argc (length argv))
	 (foreign-argv (foreign-alloc-list argv))
	 foreign-return)
    (unwind-protect
	 (progn
	   (when debug
	     (format t "call ~a with ~d arguments~%~{~a~^~%~}~%" cfun argc argv))
	   (without-fp-traps
	     (setf foreign-return (funcall cfun argc foreign-argv)))
	   (unless (zerop (%rrd-test-error))
	     (error "RRD library error: ~a" (%rrd-get-error)))
	   
	   foreign-return)
      (cffi:foreign-free foreign-argv))))

;;; TODO: try to consolidate RRD-CALL and RRD-CALL-GRAPH
(defun rrd-call-graph (parameters)
  "Call the rrd_graph library function."
  (let* ((rrd-argv (cons "graph" parameters))
	 (rrd-argc (length rrd-argv))
	 (foreign-argv (foreign-alloc-list rrd-argv))
	 foreign-return)
    (unwind-protect
	 (cffi:with-foreign-objects
	     ((prdata :pointer)
	      (xsize :int)
	      (ysize :int)
	      (ymin :double)
	      (ymax :double))
	   (%rrd-clear-error)
	  
	   ;; This is equivalent to:
	   ;; char **prdata = NULL;
	   ;; which will make librrd allocate the graph surface.
	   (setf (cffi:mem-ref prdata :pointer) (cffi:null-pointer))
      
	   ;; rrd_graph(argc, argv, &prdata, &xsize, &ysize, NULL, &ymin, &ymax);
	   (without-fp-traps
	     (setf foreign-return
		   (%rrd-graph rrd-argc foreign-argv
			       prdata xsize ysize
			       (cffi:null-pointer)
			       ymin ymax)))
      
	   ;; Free the graph surface pointer.
	   (cffi:foreign-free (cffi:mem-ref prdata :pointer))
      
	   (unless (zerop (%rrd-test-error))
	     (error "RRD library error: ~a" (%rrd-get-error)))

	   ;; Return all values filled in by rrd_graph
	   (values foreign-return
		   (cffi:mem-ref xsize :int)
		   (cffi:mem-ref ysize :int)
		   (cffi:mem-ref ymin :double)
		   (cffi:mem-ref ymax :double)))
      ;; Ensure the argument list is always cleaned up.
      (cffi:foreign-free foreign-argv))))
