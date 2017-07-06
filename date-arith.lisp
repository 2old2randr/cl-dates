;;; Copyright (c) 2017, Sudhir Shenoy.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-dates)

;; Date comparisons - strip off time component
(defun date= (&rest dates)
  (every (lambda (a b) (= (jday-number a) (jday-number b))) dates (cdr dates)))

(defun date/= (&rest dates)
  (notevery (lambda (a b) (= (jday-number a) (jday-number b))) dates (cdr dates)))

(defun date> (&rest dates)
  (every (lambda (a b) (> (jday-number a) (jday-number b))) dates (cdr dates)))

(defun date< (&rest dates)
  (every (lambda (a b) (< (jday-number a) (jday-number b))) dates (cdr dates)))

(defun date>= (&rest dates)
  (every (lambda (a b) (>= (jday-number a) (jday-number b))) dates (cdr dates)))

(defun date<= (&rest dates)
  (every (lambda (a b) (<= (jday-number a) (jday-number b))) dates (cdr dates)))

;; Date arithmetic

(defun date+ (date days)
  "Advance date by given number of days"
  (+ date days))

(defun date- (date days)
  "Retreat date by given number of days"
  (- date days))

(defun date-diff (dt1 dt2)
  "Return (positive) number of days between two dates"
  (abs (- (jday-number dt1) (jday-number dt2))))

(defun nth-day-of-week (date dow n)
  "Returns the nth day of the week e.g., second Saturday of the month in which date falls.
If n is large enough to make the date fall in a future month, the last valid day in
the month is returned."
  (multiple-value-bind (yy mm dd h m s) (date->ymd date)
    (declare (ignore dd))
    (let ((dt (loop for dd = (ymd->date yy mm 1 h m s) then (1+ dd)
		    until (eq dow (day-of-week dd))
		    finally (return dd))))
      (if (< n 2)
	  dt
	  (dotimes (i (1- n) dt)
	    (let ((next-dt (+ 7 dt)))
	      (multiple-value-bind (yy1 mm1 dd1 h1 m1 s1) (date->ymd next-dt)
		(declare (ignore yy1 dd1 h1 m1 s1))
		(if (/= mm1 mm)
		    (return dt)
		    (setf dt next-dt)))))))))

(defun first-of-next-month (date)
  "Returns date for 1st of the following month"
  (multiple-value-bind (yy mm dd h m s) (date->ymd date)
    (declare (ignore dd))
    (if (= mm 12)
	(ymd->date (1+ yy) 1 1 h m s)
	(ymd->date yy (1+ m) 1 h m s))))

(defun last-day-of-month (date)
  "Returns last day in curent month"
  (1- (first-of-next-month date)))

(defun last-day-of-prev-month (date)
  "Returns last day of previous month"
  (multiple-value-bind (yy mm dd h m s) (date->ymd date)
    (declare (ignore dd))
    (1- (ymd->date yy mm 1 h m s))))
