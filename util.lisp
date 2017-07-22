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

;;;; Util.lisp - small utility functions used across the cl-dates package.
;;;; E.g., functions to convert symbols for day of week, date convention,
;;;; etc to strings for printing

(in-package :cl-dates)

(defun parse-number (string)
  "Returns a number if string is numeric (ignoring surrounding white-space) else NIL"
  (handler-case (values (parse-integer string))
    (parse-error ()
      (multiple-value-bind (integer num-chars) (parse-integer string :junk-allowed t)
	(let ((len (length string)))
	  (cond ((or (>= num-chars len)
		     (and (= num-chars (1- len))
			  (char= (char string num-chars) #\.)))
		 integer)
		((char/= (char string num-chars) #\.) nil)
		(t (handler-case
		       (multiple-value-bind (fraction frac-chars)
			   (parse-integer string :start (1+ num-chars))
			 (if (zerop fraction)
			     integer
			     (coerce (+ integer
					(/ fraction (expt 10 (- frac-chars num-chars 1))))
				     'double-float)))
		     (parse-error () nil)))))))))

(defun leap-year-p (year)
  "True if given year is a leap year"
  (if (zerop (mod year 100))
      (zerop (mod year 400))
      (zerop (mod year 4))))

(defun hms->day-fraction (hh mm ss)
  "Return fraction of day for given time counting up from midnight"
  (/ (+ (* hh 3600) (* mm 60) ss) 86400))

(defun day-fraction->hms (day-frac)
  "Hour, minute, seconds given day fraction (midnight = 0)"
  (let* ((secs (* day-frac 86400))
	 (hh (floor (/ secs 3600))))
    (setf secs (- secs (* hh 3600)))
    (let* ((mm (floor (/ secs 60)))
	   (ss (- secs (* mm 60))))
      (values hh mm ss))))

(defparameter +weekdays+ '((:monday . ("Mon" "Monday")) (:tuesday . ("Tue" "Tuesday" "Tues"))
			   (:wednesday . ("Wed" "Wednesday")) (:thursday . ("Thu" "Thursday" "Thurs"))
			   (:friday . ("Fri" "Friday")) (:saturday . ("Sat" "Saturday"))
			   (:sunday . ("Sun" "Sunday"))))
(defun dow->string (dow)
  "String representation of day of week"
  (third (assoc dow +weekdays+)))

(defun three-letter-dow (dow)
  "Three letter abbreviation of day of week"
  (second (assoc dow +weekdays+)))

(defun str-to-weekday (str)
  "Return weekday given string (full spelling or abbreviation"
  (loop for i from 0 below 7
	do (let ((list (elt +weekdays+ i)))
	     (when (member str (cdr list) :test #'string-equal)
	       (return-from str-to-weekday (car list))))))

(defparameter +months+ '((nil) ("Jan" "January") ("Feb" "February") ("Mar" "March") ("Apr" "April")
			 ("May" "May") ("Jun" "June") ("Jul" "July") ("Aug" "August")
			 ("Sep" "September" "Sept") ("Oct" "October") ("Nov" "November")
			 ("Dec" "December")))
(defun month->string (mm)
  "Month name as string given month number"
  (second (elt +months+ mm)))

(defun three-letter-month (mm)
  "Three letter month abbreviation given month number"
  (car (elt +months+ mm)))

(defun str-to-month (str)
  "Month number (1-12) given month name or abbreviation"
  (loop for i from 1 to 12
	do (when (member str (elt +months+ i) :test #'string-equal)
	     (return-from str-to-month i))))

(defun day-count->string (day-count-convention)
  "Return string description of day count convention"
  (case day-count-convention
    ((:actual-actual-fixed :act-act-fixed) "Actual/Actual (Fixed)")
    ((:actual-actual :act-act :actual-365 :act-365) "Actual/Actual (ISDA)")
    ((:actual-actual-isma :act-act-isma) "Actual/Actual (ISMA)")
    ((:actual-actual-afb :act-act-afb)  "Actual/Actual (AFB)")
    ((:actual-365-l :act-365-l) "Actual/365L")
    ((:actual-365-nl :act-365-nl :nl-365 :actual-365-jgb :act-365-jgb) "NL/365")
    ((:actual-360 :act-360) "Actual/360")
    ((:30a-360 :30-360-isda :30-360-muni :bond-basis :360-360) "30A/360")
    ((:30e-360 :30-360-isma) "30E/360")
    (:30e+-360 "30E+/360")
    ((:30e-360-isda :30-360-german) "30/360 (German)")
    ((:30u-360 :30-360-us) "30/360 (US)")
    (otherwise (concatenate 'string "Unknown day count convention: " (string day-count-convention)))))
  

(defun eom-rule->string (rule)
  "Return string description of EOM rule"
  (case rule
    (:eom-normal "EOM")
    (:eom-no-leap-day "EOM (No Leap)")
    (otherwise "")))
