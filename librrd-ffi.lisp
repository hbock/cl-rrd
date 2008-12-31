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
  (:unix (:or "../rrdtool-1.2.27/src/.libs/librrd.so.2" "librrd.so.2" "librrd.so"))
  (t (:default "librrd")))

(cffi:use-foreign-library librrd)

(cffi:defcfun ("rrd_clear_error" %rrd-clear-error) :void)

(cffi:defcfun ("rrd_create" %rrd-create) :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("rrd_dump" %rrd-dump) :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("rrd_fetch" %rrd-fetch) :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun "rrd_get_error" :string)

(cffi:defcfun ("rrd_graph" %rrd-graph) :int
  (argc :int)
  (argv :pointer)
  (printout :pointer)
  (width :pointer)
  (height :pointer)
  (stream :pointer)
  (ymin :pointer)
  (ymax :pointer))

(cffi:defcfun "rrd_last" :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun "rrd_resize" :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun "rrd_restore" :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun "rrd_test_error" :int)

(cffi:defcfun "rrd_tune" :int
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("rrd_update" %rrd-update) :int
  (argc :int)
  (argv :pointer))

(defun rrd-create (args)
  (let ((tmp (pushnew "dummy" args)))
    (sb-int:with-float-traps-masked (:invalid :divide-by-zero)
      (%rrd-create
       (length tmp)
       (cffi:foreign-alloc :string
                           :initial-contents tmp
                           :null-terminated-p t)))))

(defun rrd-fetch (args)
  (let ((tmp (pushnew "dummy" args)))
    (sb-int:with-float-traps-masked (:invalid :divide-by-zero)
      (%rrd-fetch
       (length tmp)
       (cffi:foreign-alloc :string
                           :initial-contents tmp
                           :null-terminated-p t)))))
(defun rrd-graph (args)
  (let ((tmp (pushnew "dummy" args)))
   (cffi:with-foreign-objects
       ((prdata :pointer)
        (xsize :int)
        (ysize :int)
        (ymin :double)
        (ymax :double))
     (setf (mem-aref xsize :int) 800)
     (setf (mem-aref ysize :int) 800)
     (sb-int:with-float-traps-masked (:invalid :divide-by-zero)
       (%rrd-graph
        (length tmp)
        (cffi:foreign-alloc :string
                            :initial-contents tmp
                            :null-terminated-p t)
        prdata
        xsize
        ysize
        (null-pointer)
        ymin
        ymax)))))

(defun rrd-update (args)
  (let ((tmp (pushnew "dummy" args)))
    (%rrd-update
       (length tmp)
       (cffi:foreign-alloc :string
                           :initial-contents tmp
                           :null-terminated-p t))))

