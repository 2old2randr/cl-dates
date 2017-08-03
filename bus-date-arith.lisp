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
	  counting (business-day-p dt calendar))))

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

(defun add-workdays (date calendar days)
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
If the roll convention is NIL or the date is a business day, the date is returned.
Otherwise, the date is moved as per the roll convention:
:preceding - date is moved to the previous business day
:following - date is moved to the next business day
:modified-following - date is moved to the next business day unless that falls in
the next month in which case it is moved to the previous day"
  (cond ((or (null roll-convention) (business-day-p date calendar)) date)
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

(defun generate-schedule (start-date maturity period calendar
			  &key (rule :normal-back)
			       (roll-convention :modified-following)
			       (maturity-roll :following)
			       (eom-rule :nil)
			       (first-coupon-date nil)
			       (penultimate-coupon-date nil))
  "Generate a list of coupon dates between start-date and maturity (both inclusive).
If paramters supplied are invalid, NIL and an error string are returned as two values.

Parameters:
start-date - Effective date (may be passed as nil if rule is :normal-back)
maturity - Termination date
period - Coupon period in months. If period is 0, no intermediate coupon dates are generated.
rule - The date generation rule (by default, this is backwards from maturity date)
    :normal-back - Roll on maturity or penultimate coupon backawards (default)
    :normal-front - Roll on start date or first coupon forwards
    :imm-dates - All coupon dates will fall on IMM dates (3rd Wednesday of month)
    :cds-dates - Coupons are generated on CDS dates (20th of Mar/Jun/Sep/Dec). If start
                 date or maturity are not CDS dates, the CDS date before/after is included.
    :zero - Zero coupon (only start date and maturity are returned)
roll-convention - Convention for adjusting coupon dates that fall on non-business days
                  (:modified-following by default)
maturity-roll - Roll convention for maturity date specifically (NIL by default)
eom-rule - Whether dates should fall on month-ends when roll date is on a month-end (NIL by default)
first-coupon-date - Date of first coupon (for specifying stubs at front)
penultimate-coupon-date - Date of next to last coupon (for specifying stubs at the end)"

  ;; Interpolate start date if not specified and generating dates backward
  (when (null start-date)
    (unless (and (null first-coupon-date) (eq rule :normal-back))
      (return-from generate-schedule
	(values nil "Start date cannot be null unless normal backward geeneration with no first coupon date")))
    (let ((term-date (or penultimate-coupon-date maturity)))
      (add-years term-date (- (1+ (/ (date- term-date (todays-date)) 366))))))

  ;; Check all parameters for sanity
  (unless (member rule '(:normal-front :normal-back :imm-dates :cds-dates :zero))
    (return-from generate-schedule (values nil (format nil "Unknown rule ~s" rule))))
  (unless (and (date< start-date maturity) (integerp period) (>= period 0))
    (return-from generate-schedule
      (values nil "Maturity must be after start date; Period must be an integer >=0")))
  (cond ((eq rule :cds-dates)
	 (unless (and (null first-coupon-date) (null penultimate-coupon-date) (null eom-rule))
	   (return-from generate-schedule
	     (values nil "Cannot have first/last coupon dates or EOM rule for CDS schedule"))))
	((eq rule :imm-dates)
	 (unless (or (null first-coupon-date) (imm-date-p first-coupon-date))
	   (values nil "First coupon date must be an IMM date for IMM schedule"))
	 (unless (or (null penultimate-coupon-date) (imm-date-p penultimate-coupon-date))
	   (values nil "Last coupon date must be an IMM date for IMM schedule"))
	 (unless (null eom-rule)
	   (values nil "IMM schedule cannot have EOM rule")))
	((or (eq rule :normal-front) (eq rule :normal-back))
	 (unless (or (null first-coupon-date)
		     (date< start-date first-coupon-date maturity))
	   (values nil "First coupon date must be between start date and maturity"))
	 (unless (or (null penultimate-coupon-date)
		     (date< start-date penultimate-coupon-date maturity))
	   (values nil "Last coupon date must be between start date and maturity"))
	 (when (and first-coupon-date penultimate-coupon-date)
	   (unless (date< first-coupon-date penultimate-coupon-date)
	     (values nil "Last coupon date must be after first coupon date")))))

  ;; Generate a series of unadjusted dates
  (let (cpn-dates)
    (cond ((or (eq rule :zero) (= period 0)) (setf cpn-dates (list start-date maturity)))
	  ((or (eq rule :normal-front) (eq rule :normal-back))
	   ;; Normal swaps / bond schedule - usually backwards from maturity
	   (let ((first-date (if (eq rule :normal-back) maturity start-date))
		 (roll-date (if (eq rule :normal-back)
				(or penultimate-coupon-date maturity)
				(or first-coupon-date start-date)))
		 (last-cpn-date (if (eq rule :normal-back)
				    (or first-coupon-date start-date)
				    (or penultimate-coupon-date maturity)))
		 (last-date (if (eq rule :normal-back) start-date maturity))
		 (sign-fn (if (eq rule :normal-back) #'- #'identity))
		 (term-cond (if (eq rule :normal-back) #'date< #'date>)))
	     (push first-date cpn-dates)
	     (when (date/= first-date roll-date)
	       (push roll-date cpn-dates))
	     (loop for i = 1 then (1+ i)
		   for dt = (add-months roll-date (* i (funcall sign-fn period))
					:eom-rule eom-rule)
		   until (funcall term-cond dt last-cpn-date)
		   do (push dt cpn-dates))
	     (unless (date= (adjust-date (car cpn-dates) calendar roll-convention)
			    (adjust-date last-cpn-date calendar roll-convention))
	       (push last-cpn-date cpn-dates))
	     (unless (date= (adjust-date (car cpn-dates) calendar roll-convention)
			    (adjust-date last-date calendar roll-convention))
	       (push last-date cpn-dates))
	     ;; dates should be in ascending order
	     (when (eq rule :normal-front)
	       (setf cpn-dates (nreverse cpn-dates)))
	     (when (and eom-rule (date>= roll-date (last-workday-of-month roll-date calendar)))
	       ;; Month-end rolls - move all dates to month end except start date
	       ;; Maturtity date is moved only if maturity-roll is specified as non-null
	       (setf cpn-dates (mapcar (lambda(dt) (cond ((date= dt start-date) dt)
							 ((date= dt maturity)
							  (if (null maturity-roll)
							      dt
							      (last-workday-of-month dt calendar)))
							 ((null roll-convention)
							  (last-day-of-month dt))
							 (t (last-workday-of-month dt calendar))))
				       cpn-dates))
	       ;; Remove any dates that are outside start-date to maturity range after EOM adj
	       (let ((last-date (car (last cpn-dates))))
		 (setf cpn-dates (remove-if (lambda(dt) (date> dt last-date)) cpn-dates
					    :from-end t :count 1)))
	       (when (and (>= (length cpn-dates) 2) (date< (second cpn-dates) (car cpn-dates)))
		 (setf (second cpn-dates) (car cpn-dates)
		       cpn-dates (cdr cpn-dates))))))
	  ((eq rule :cds-dates)
	   (labels ((cds-date (date dir)
		      "Return previous/next CDS date depending on sign of dir"
		      (multiple-value-bind (yy mm dd) (date->ymd date)
			(declare (ignore dd))
			(let ((result-date (ymd->date yy mm 20)))
			  (if (and (minusp dir) (date> result-date date))
			      (setf result-date (add-months result-date -1))
			      (if (and (plusp dir) (date< result-date date))
				  (setf result-date (add-months result-date +1))))
			  (multiple-value-bind (yy mm dd) (date->ymd result-date)
			    (declare (ignore yy dd))
			    (when (/= 0 (mod mm 3))
			      (let ((mths-incr (if (minusp dir) (- (mod mm 3)) (- 3 (mod mm 3)))))
				(setf result-date (add-months result-date mths-incr)))))
			  result-date))))
	     ;; Closest CDS date prior to start date if it isn't one
	     (push (cds-date start-date -1) cpn-dates)
	     ;; roll on the CDS dates to maturity
	     (loop for dt = (cds-date (car cpn-dates) +1) then (cds-date (1+ dt) +1)
		   until (date> dt maturity)
		   do (push dt cpn-dates))
	     (unless (date= (adjust-date (car cpn-dates) calendar roll-convention)
			    (adjust-date maturity calendar roll-convention))
	       ;; maturity was not a CDS date - include next CDS date
	       (push (cds-date maturity +1) cpn-dates))
	     (setf cpn-dates (nreverse cpn-dates))))
	  ((eq rule :imm-dates)
	   (let ((roll-date (or first-coupon-date start-date))
		 (max-date (or penultimate-coupon-date maturity)))
	     (push start-date cpn-dates)
	     (when first-coupon-date
	       (push first-coupon-date cpn-dates))
	     (loop for i = 1 then (1+ i)
		   for dt = (add-months roll-date (* i period))
		   until (date> dt max-date)
		   do (push (nth-day-of-week dt :wednesday 3) cpn-dates))
	     (unless (date= (car cpn-dates) max-date)
	       (push max-date cpn-dates))
	     (push maturity cpn-dates)	; Possibly duplicate date
	     (setf cpn-dates (nreverse cpn-dates)))))
    ;; Ensure all dates fall on business days
    (setf cpn-dates (mapcar (lambda(dt) (if (date= dt maturity)
					    (adjust-date dt calendar maturity-roll)
					    (adjust-date dt calendar roll-convention)))
			    cpn-dates))
    ;; Remove any duplicate dates and return (list is in ascending order of dates)
    (remove-duplicates cpn-dates)))
