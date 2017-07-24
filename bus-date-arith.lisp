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

;;;; Bus-Date-Arith.lisp - Date arithmetic and movement functions that
;;;; operate as per a holiday calendar. Computed dates are forced to be
;;;; on business days, i.e., not weekends or holidays.

(in-package :cl-dates)

(defun workday-number (date calendar)
  "Return the business day number in the year (out of approx. 252)
If date is not a business day, the number of working days prior to
that date is returned"
  (let ((year (date->ymd date)))
    (loop for dt = (ymd->date year 1 1) then (1+ dt)
	  until (date> dt date)
	  counting (business-day-p date calendar))))

(defun add-days-return-bus-day (date calendar days)
  "Add days to date and if the result is not a business day,
advance the date to a business day in the same direction"
  (let ((plus-minus (if (< days 0) #'- #'+)))
    (loop for dt = (+ date days) then (funcall plus-minus dt 1)
	  until (business-day-p dt calendar)
	  finally (return dt))))

(defun next-workday (date calendar)
  "Return the next business day after date"
  (add-days-return-bus-day date calendar +1))

(defun prev-workday (date calendar)
  "Return the business day immediately before date"
  (add-days-return-bus-day date calendar -1))

(defun add-business-days (date calendar days)
  (let ((fn (if (< days 0) #'prev-workday #'next-workday)))
    (loop for i from 1 upto (abs days)
	  do (setf date (funcall fn date calendar))
	  finally (return date))))

(defun diff-workdays (date1 date2 calendar)
  "Counts the number of business days between two dates.
Dates may be in either order, returned count is always positive.
The earlier date is not included in the count and the later date
is included only when it is a business day"
  (when (date< date2 date1)
    (rotatef date1 date2))
  (loop for dt = (next-workday date1 calendar) then (next-workday dt calendar)
	until (date> dt date2)
	counting t))

(defun first-workday-of-month (date calendar)
  "First business day of month that date is in"
  (next-workday (last-day-of-prev-month date) calendar))

(defun last-workday-of-month (date calendar)
  "Last business day of month that date falls in"
  (let ((dt (last-day-of-month date)))
    (if (business-day-p dt calendar)
	dt
	(prev-workday dt calendar))))

(defun last-workday-of-prev-month (date calendar)
  "Last business day of month before the one that date falls in"
  (let ((dt (last-day-of-prev-month date)))
    (if (business-day-p dt calendar)
	dt
	(prev-workday dt calendar))))

(defun adjust-date (date calendar &optional (roll-convention :modified-following))
  "Move date if it falls on a holiday or weekend according to roll convention.
Roll convention can be one of-
:preceding - date is moved to the previous business day
:following - date is moved to the next business day
:modified-following - date is moved to the next business day unless that falls in
the next month in which case it is moved to the previous day"
  (cond ((business-day-p date calendar) date)
	((eq roll-convention :preceding) (prev-workday date calendar))
	((eq roll-convention :following) (next-workday date calendar))
	((eq roll-convention :modified-following)
	 (let ((dt (next-workday date calendar)))
	   (multiple-value-bind (yy mm) (date->ymd dt)
	     (declare (ignore yy))
	     (multiple-value-bind (yy1 mm1) (date->ymd date)
	       (declare (ignore yy1))
	       (if (= mm mm1)
		   dt
		   (prev-workday date calendar))))))
	(t (error "Unknown roll convention ~s" roll-convention))))
