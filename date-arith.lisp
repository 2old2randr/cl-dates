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

(defun nth-day-of-week (date dow n)
  "Returns the nth day of the week e.g., second Saturday of the month in which date falls.
If n is large enough to make the date fall in a future month, the last valid day in
the month is returned."
  (multiple-value-bind (yy mm dd) (date->ymd date)
    (declare (ignore dd))
    (let ((dt (loop for dd = (ymd->date yy mm 1) then (1+ dd)
		    until (eq dow (day-of-week dd))
		    finally (return dd))))
      (if (< n 2)
	  dt
	  (dotimes (i (1- n) dt)
	    (let ((next-dt (+ 7 dt)))
	      (multiple-value-bind (yy1 mm1 dd1) (date->ymd next-dt)
		(declare (ignore yy1 dd1))
		(if (/= mm1 mm)
		    (return dt)
		    (setf dt next-dt)))))))))

(defun first-of-next-month (date)
  "Returns date for 1st of the following month"
  (multiple-value-bind (yy mm dd) (date->ymd date)
    (declare (ignore dd))
    (if (= mm 12)
	(ymd->date (1+ yy) 1 1)
	(ymd->date yy (1+ mm) 1))))

(defun last-day-of-month (date)
  "Returns last day in curent month"
  (1- (first-of-next-month date)))

(defun last-day-of-prev-month (date)
  "Returns last day of previous month"
  (multiple-value-bind (yy mm dd) (date->ymd date)
    (declare (ignore dd))
    (1- (ymd->date yy mm 1))))

(defun date+ (date days)
  "Advance date by given number of days"
  (+ date days))

(defun date- (date days)
  "Retreat date by given number of days"
  (- date days))

(defun add-years (date years &key eom-rule)
  "Add number of years to date (subtract if negative) - see add-months"
  (add-months date (* 12 years) :eom-rule eom-rule))

(defun add-months (date months &key eom-rule)
  "Add number of months to date (negative number will decrement date).
The returned date will have the same day as the given date unless that is not a valid
date in which case the day will be adjusted appropriately. E.g., adding 6 months
to March 31 will return September 30.

If eom-rule is specified, the returned date will be a month-end whenever the given
date is a month-end. eom-rule can be either :eom-normal or :eom-no-leap-day. In the
latter case, February 28th is considered to be the end of month even in leap years."
  (setf months (truncate months))	; ensure integer
  (let ((yrs (truncate (/ months 12)))
	(mths (rem months 12))
	(is-eom (= date (last-day-of-month date))))
    (multiple-value-bind (yy mm dd) (date->ymd date)
      (when (and (eq eom-rule :eom-no-leap-day) (= mm 2) (= dd 28))
	(setf is-eom t))
      (incf yy yrs)
      (incf mm mths)
      (if (> mm 12)
	  (setf mm (- mm 12)
		yy (1+ yy))
	  (when (< mm 1)		; negative increment
	    (setf mm (+ mm 12)
		  yy (1- yy))))
      (loop for d = dd then (1- d)
	    while (not (valid-date-p yy mm d))
	    finally (return (cond ((and is-eom (eq eom-rule :eom-normal))
				   (last-day-of-month (ymd->date yy mm d)))
				  ((and is-eom (eq eom-rule :eom-no-leap-day))
				   (if (= mm 2)
				       (ymd->date yy mm 28)
				       (last-day-of-month (ymd->date yy mm d))))
				  (t (ymd->date yy mm d))))))))

(defun diff-days (dt1 dt2)
  "Return absolute number of days between two dates"
  (abs (- (jday-number dt1) (jday-number dt2))))

(defun diff-years (dt1 dt2 day-count-convention
		   &key termination-date frequency (is-last-coupon nil))
  "Return number of years between two dates according to a given day count convention.
The dates may be in either order and the returned value is always positive.

termination-date is needed only when the day count convention is 30E/360 (ISDA) and
the later date is the last date of February.

frquency is needed when the Actual/Actual (ISMA) day convention is used. In this case,
for irregular periods, by default, it is assumed to be the front stub. If the dates are
with reference to the last interest period, is-last-coupon should be T.

Supported day conventions are:
:actual-actual-fixed :act-act-fixed
:actual-actual :act-act :actual-365 :act-365
:actual-actual-isma :act-act-isma
:actual-actual-afb :act-act-afb
:actual-365-l :act-365-l
:actual-365-nl :act-365-nl :nl-365 :actual-365-jgb :act-365-jgb
:actual-360 :act-360
:30a-360 :30-360-isda :30-360-muni :bond-basis :360-360
:30e-360 :30-360-isma
:30e+-360
:30e-360-isda :30-360-german
:30u-360 :30-360-us"
  (when (< dt2 dt1)
    (rotatef dt1 dt2))
  (multiple-value-bind (y1 m1 d1) (date->ymd dt1)
    (multiple-value-bind (y2 m2 d2) (date->ymd dt2)
      (ccase day-count-convention
	((:actual-actual-fixed :act-act-fixed)
	 (/ (diff-days dt1 dt2) 365))
	((:actual-actual :act-act :actual-365 :act-365)
	 (let ((whole-years (- y2 y1 1))
	       (days-1 (diff-days dt1 (ymd->date (1+ y1) 1 1)))
	       (days-2 (diff-days dt2 (ymd->date y2 1 1))))
	   (+ whole-years (/ days-1 (if (leap-year-p y1) 366 365))
	      (/ days-2 (if (leap-year-p y2) 366 365)))))
	((:actual-actual-isma :act-act-isma)
	 (unless (member frequency '(1 2 3 4 6) :test #'=)
	   (error "Invalid frequency ~a" frequency))
	 (let ((dt3 (add-months dt1 (/ 12 frequency) :eom-rule :eom-normal))
	       (days (diff-days dt1 dt2)))
	   (if (= dt2 dt3)
	       ;; regular coupon period
	       (/ days (* frequency days))
	       (if is-last-coupon
		   ;; irregular last period - count days forward from dt1
		   (cond
		     ((< dt2 dt3)	; short period
		      (/ days (* frequency (diff-days dt1 dt3))))
		     (t			; long final period
		      (let* ((dt4 (add-months dt3 (/ 12 frequency) :eom-rule :eom-normal))
			     (regular-days (diff-days dt1 dt3))
			     (notional-days (diff-days dt3 dt4))
			     (actual-days (diff-days dt3 dt2)))
			(+ (/ regular-days (* frequency regular-days))
			   (/ actual-days (* frequency notional-days))))))
		   ;; irregular front stub
		   (cond
		     ((< dt2 dt3)	; short stub
		      (let ((dt0 (add-months dt2 (- (/ 12 frequency)))))
			(/ days (* frequency (diff-days dt0 dt2)))))
		     (t 		; long stub
		      (let* ((dt4 (add-months dt2 (- (/ 12 frequency))))
			     (dt0 (add-months dt4 (- (/ 12 frequency))))
			     (regular-days (diff-days dt4 dt2))
			     (notional-days (diff-days dt0 dt4))
			     (actual-days (diff-days dt1 dt4)))
			(+ (/ regular-days (* frequency regular-days))
			   (/ actual-days (* frequency notional-days))))))))))
	((:actual-actual-afb :act-act-afb)
	 (let ((num-years 0))
	   (when (> y2 y1)
	     ;; count number of whole years backwards from end date
	     (loop for temp-y2 = (1- y2) then (1- temp-y2)
		   for temp-dt = (ymd->date temp-y2 m2
					    ;; If date rolls on Feb 28, use Feb 29 in leap years
					    (if (and (leap-year-p temp-y2) (= 2 m2) (= 28 d2))
						29 d2))
		   while (> temp-dt dt1) do (incf num-years)
		   finally (setf y2 (1+ temp-y2)
				 dt2 (if (and (leap-year-p y2) (= m2 2) (= d2 28))
					 (ymd->date y2 2 29)
					 (ymd->date y2 m2 d2)))))
	   (incf num-years (/ (diff-days dt1 dt2)
			      ;; denominator is 366 if Feb 29 is between dt1 and dt2 or
			      ;; if dt1 is Feb 29
			      (if (or (and (= m1 2) (= d1 29))
				      (and (leap-year-p y1) (< dt1 (ymd->date y1 2 29) dt2))
				      (and (leap-year-p y2) (< dt1 (ymd->date y2 2 29) dt2)))
				  366 365)))))
	((:actual-365-l :act-365-l)
	 (/ (diff-days dt1 dt2) (if (leap-year-p y2) 366 365)))
	((:actual-365-nl :act-365-nl :nl-365 :actual-365-jgb :act-365-jgb)
	 (if (= y1 y2)
	     (let ((days (diff-days dt1 dt2)))
	       (when (and (leap-year-p y1)
			  (or (and (= m1 2) (= d1 29))
			      (< dt1 (ymd->date y1 2 29) dt2)))
		 (decf days))
	       (/ days 365))
	     (let ((days (* 365 (- y2 y1 1))) ; days in intervening years without Feb 29s
		   (days-1 (diff-days dt1 (ymd->date (1+ y1) 1 1)))
		   (days-2 (diff-days dt2 (ymd->date y2 1 1))))
	       (when (and (leap-year-p y1) (< m1 3))
		 (decf days-1))
	       (when (and (leap-year-p y2) (> m2 2))
		 (decf days-2))
	       (/ (+ days days-1 days-2) 365))))
	((:actual-360 :act-360)
	 (/ (diff-days dt1 dt2) 360))
	((:30a-360 :30-360-isda :30-360-muni :bond-basis :360-360)
	 (when (= d1 31)
	   (setf d1 30))
	 (when (and (= d1 30) (= d2 31))
	   (setf d2 30))
	 (/ (+ (- d2 d1) (* 30 (- m2 m1)) (* 360 (- y2 y1))) 360))
	((:30e-360 :30-360-isma)
	 (when (= d1 31)
	   (setf d1 30))
	 (when (= d2 31)
	   (setf d2 30))
	 (/ (+ (- d2 d1) (* 30 (- m2 m1)) (* 360 (- y2 y1))) 360))
	(:30e+-360
	 (when (= d1 31)
	   (setf d1 30))
	 (when (= d2 31)
	   (setf d2 1
		 m2 (1+ m2))
	   (when (> m2 12)
	     (setf m2 1
		   y2 (1+ y2))))
	 (/ (+ (- d2 d1) (* 30 (- m2 m1)) (* 360 (- y2 y1))) 360))
	((:30e-360-isda :30-360-german)
	 (if (= d1 31)
	     (setf d1 30)
	     (when (or (and (leap-year-p y1) (= m1 2) (= d1 29))
		       (and (not (leap-year-p y1)) (= m1 2) (= d1 28)))
		 (setf d1 30)))
	 (if (= d2 31)
	     (setf d2 30)
	     (when (or (and (leap-year-p y2) (= m2 2) (= d2 29))
		       (and (not (leap-year-p y2)) (= m2 2) (= d2 28)))
	       (unless (and (not (null termination-date)) (date= dt2 termination-date))
		 (setf d2 30))))
	 (/ (+ (- d2 d1) (* 30 (- m2 m1)) (* 360 (- y2 y1))) 360))
	((:30u-360 :30-360-us)
	 (when (and (or (and (leap-year-p y2) (= m2 2) (= d2 29))
			(and (not (leap-year-p y2)) (= m2 2) (= d2 28)))
		    (or (and (leap-year-p y1) (= m1 2) (= d1 29))
			(and (not (leap-year-p y1)) (= m1 2) (= d1 28))))
	   (setf d2 30))
	 (when (or (and (leap-year-p y1) (= m1 2) (= d1 29))
		   (and (not (leap-year-p y1)) (= m1 2) (= d1 28)))
	   (setf d1 30))
	 (when (and (= d2 31) (or (= d1 30) (= d1 31)))
	   (setf d2 30))
	 (when (= d1 31)
	   (setf d1 30))
	 (/ (+ (- d2 d1) (* 30 (- m2 m1)) (* 360 (- y2 y1))) 360))))))
