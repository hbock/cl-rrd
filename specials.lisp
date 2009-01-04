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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar *rpn-operator-map* (make-hash-table)
    "Hash table holding RPN operators and their respective arity.")
  (defvar *rpn-special-value-list* ()
    "Hash table holding special RPN values.")

  (defmacro def-rpn-special-values (special-list)
    `(eval-when (:compile-toplevel :execute :load-toplevel)
       (dolist (special ,special-list)
	 (or (keywordp special) (error "Specified special value (~a) is not a keyword." special))
	 (push special *rpn-special-value-list*))
       (delete-duplicates *rpn-special-value-list*)))
  
  (defmacro def-rpn-operators (operator-list arity)
    `(eval-when (:compile-toplevel :execute :load-toplevel)
       (dolist (operator ,operator-list)
	 (or (symbolp operator) (error "Specified operator (~a) is not a symbol." operator))
	 (setf (gethash operator *rpn-operator-map*) ,arity)))))

(defconstant +unix-epoch+ 2208988800)