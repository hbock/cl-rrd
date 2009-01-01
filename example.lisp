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

;;; Basic example of the cl-rrd functionality.

(defparameter *rrd-test-file* "/tmp/test.rrd")
(defparameter *rrd-image-file* "/tmp/test.png")

;; (rrd-create
;;       (list *rrd-test-file*
;;             "--start" "920804400"
;;             "DS:speed:COUNTER:600:U:U"
;;             "RRA:AVERAGE:0.5:1:24"
;;             "RRA:AVERAGE:0.5:6:10p=300"))

(defun make-graph ()
  (with-database packets ("packets.rrd" :start 0 :step 300)
      ((:data-source incoming :gauge 600 0 5)
       (:data-source outgoing :gauge 600 0 5)
       (:data-source total :compute (+ incoming outgoing))
       (:archive :average :rows 48))
    (format t "Graphing!")))

;;; "mydata,8,*"
(rrd:compile-rpn '(* mydata 8))
;;;
(rrd:compile-rpn '(+ pred (* dev 2)))
;;; "pred,dev,2,radius,SIN,MIN,*,+"
(rrd:compile-rpn '(+ pred (* dev (min 2 (sin radius)))))

;; (rrd-update (list *rrd-test-file* "920804700:12345" "920805000:12357" "920805300:12363"))
;; (rrd-update (list *rrd-test-file* "920805600:12363" "920805900:12363" "920806200:12373"))
;; (rrd-update (list *rrd-test-file* "920806500:12383" "920806800:12393" "920807100:12399"))
;; (rrd-update (list *rrd-test-file* "920807400:12405" "920807700:12411" "920808000:12415"))
;; (rrd-update (list *rrd-test-file* "920808300:12420" "920808600:12422" "920808900:12423"))

;; (rrd-graph (list *rrd-image-file "--start" "920804400" "--end" "920808000" "DEF:myspeed=/tmp/test.rrd:speed:AVERAGE" "LINE2:myspeed#FF0000"))