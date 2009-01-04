;;;; cl-rrd - Common Lisp bindings to cl-rrd
;;;; Copyright (C) 2008 Brandon Edens <brandon@cs.uri.edu>
;;;; Copyright (C) 2008 Harry Bock <harry@oshean.org

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

;;; Some simple usage examples.

;; (rrd-create
;;       (list *rrd-test-file*
;;             "--start" "920804400"
;;             "DS:speed:COUNTER:600:U:U"
;;             "RRA:AVERAGE:0.5:1:24"
;;             "RRA:AVERAGE:0.5:6:10p=300"))

;; rrdtool create packets.rrd DS:incoming:GAUGE:600:0:5
;;                            DS:outgoing:GAUGE:600:0:5
;;                            DS:total:COMPUTE:incoming,outgoing,+
;;                            
(defun make-graph ()
  (with-database packets ("packets.rrd" :start 0 :step 300)
      ((:data-source incoming :gauge 600 0 5)
       (:data-source outgoing :gauge 600 0 5)
       (:data-source total :compute (+ incoming outgoing))
       (:archive :average :rows 48))
    (update packets '((1231052709 5) (1231052909 6)) :template '(outgoing incoming))))

;;; Some examples for using the RPN compiler.

;;; "mydata,8,*"
(rrd:compile-rpn '(* mydata 8))
;;; "pred,dev,2,*,+"
(rrd:compile-rpn '(+ pred (* dev 2)))
;;; "pred,dev,2,radius,SIN,MIN,*,+"
(rrd:compile-rpn '(+ pred (* dev (min 2 (sin radius)))))

;; (rrd-graph (list *rrd-image-file "--start" "920804400" "--end" "920808000" "DEF:myspeed=/tmp/test.rrd:speed:AVERAGE" "LINE2:myspeed#FF0000"))