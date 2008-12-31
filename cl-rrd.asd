;;;; cl-rrd - Common Lisp bindings to cl-rrd
;;;; Copyright (C) 2008 Brandon Edens <brandon@cs.uri.edu>
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
(in-package :cl-user)

(defpackage :cl-rrd-asd
  (:use :cl :asdf))
(in-package :cl-rrd-asd)

(defvar *cl-rrd-version* "0.1")
(export '*cl-rrd-version*)

(asdf:defsystem cl-rrd
  :name "cl-rrd"
  :author "Brandon Edens <brandon@cs.uri.edu>"
  :version "0.1"
  :maintainer "Brandon Edens <brandon@cs.uri.edu>"
  :description "Common Lisp bindings to librrd2"
  :long-description ""
  :depends-on (:cffi)
  :serial t
  :components
  ((:file "packages")
   (:file "specials")
   (:file "librrd-ffi")
   (:file "rpn")
   (:file "cl-rrd")))