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
(in-package :cl-user)

(defpackage :cl-rrd
  (:nicknames :rrd)
  (:use :common-lisp)
  (:export :database
	   ;; macros
	   :with-database
	   ;; functions
	   :create
	   :update
	   :generate-graph
	   ;; useful lower-level utilities
	   :compile-rpn))
