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

;; This will execute the equivalent to the following rrdtool command:

;; rrdtool create packets.rrd DS:incoming:GAUGE:600:0:5
;;                            DS:outgoing:GAUGE:600:0:5
;;                            DS:total:COMPUTE:incoming,outgoing,+
(defun make-graph ()
  (with-database packets ("packets.rrd" :start 0 :step 300)
      ((:data-source incoming :gauge 600 0 5)
       (:data-source outgoing :gauge 600 0 5)
       (:data-source total :compute (+ incoming outgoing))
       (:archive :average :rows 48))
    (update packets '((1231052709 5) (1231052909 6)) :template '(outgoing incoming))))

(defun test-1 ()
  (with-database testing ("rrd-test.rrd" :start (unix-time) :step 300)
      ((:data-source linear :gauge 600 0 6.283)
       (:data-source sine :compute (sin linear))
       (:archive :average :rows 50))
    (loop with time = (unix-time)
       :for i :from 0 :below 50
       :collect (list (+ time (* 300 (1+ i))) (* (/ i 50) 2 pi)) :into update-list
       :finally (update testing update-list))
    ;; (generate-graph "test.png" (:start 1231129446 :end 1231144146 :vertical-label "Y")
    ;; 	((:def linear-out testing linear :average)
    ;; 	 (:def sine-out testing sine :average)
    ;; 	 (:line2 linear-out "#FF0000")))
    ))

;; rrdtool graphv test.png --start 1231129446 --end 1231144146
;; 'DEF:mylinear=rrd-test.rrd:linear:AVERAGE' 'DEF:mysine=rrd-test.rrd:sine:AVERAGE'
;; 'LINE2:mylinear#FF0000' 'LINE2:mysine#00FF00'

;;; Some examples for using the RPN compiler.

;;; "mydata,8,*"
(rrd:compile-rpn '(* mydata 8))
;;; "pred,dev,2,*,+"
(rrd:compile-rpn '(+ pred (* dev 2)))
;;; "pred,dev,2,radius,SIN,MIN,*,+"
(rrd:compile-rpn '(+ pred (* dev (min 2 (sin radius)))))