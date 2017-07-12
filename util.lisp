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
  (/ (+ (* hh 3600) (* mm 60) ss) 86400))

(defun day-fraction->hms (day-frac)
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
  (third (assoc dow +weekdays+)))

(defun three-letter-dow (dow)
  (second (assoc dow +weekdays+)))

(defun str-to-weekday (str)
  (loop for i from 0 below 7
	do (let ((list (elt +weekdays+ i)))
	     (when (member str (cdr list) :test #'string-equal)
	       (return-from str-to-weekday (car list))))))

(defparameter +months+ '((nil) ("Jan" "January") ("Feb" "February") ("Mar" "March") ("Apr" "April")
			 ("May" "May") ("Jun" "June") ("Jul" "July") ("Aug" "August")
			 ("Sep" "September" "Sept") ("Oct" "October") ("Nov" "November")
			 ("Dec" "December")))
(defun month->string (mm)
  (second (elt +months+ mm)))

(defun three-letter-month (mm)
  (car (elt +months+ mm)))

(defun str-to-month (str)
  (loop for i from 1 to 12
	do (when (member str (elt +months+ i) :test #'string-equal)
	     (return-from str-to-month i))))

(defparameter +time-suffixes+ '((:hour . ("h" "hr" "hrs" "hour" "hours"))
				(:minute . ("m" "min" "mins" "minute" "minutes"))
				(:second . ("s" "sec" "secs" "second" "seconds"))))
(defun str-to-hms (str)
  (loop for i from 0 below 3
	do (let ((list (elt +time-suffixes+ i)))
	     (when (member str (cdr list) :test #'string-equal)
	       (return-from str-to-hms (car list))))))

(defun str-to-ampm (str)
  (cond ((member str '("a.m." "a.m" "am" "morning") :test #'string-equal) :am)
	((member str '("p.m." "p.m" "pm" "evening" "afternoon") :test #'string-equal) :pm)
	(t nil)))

(defun str-is-day-suffix (str)
  (not (null (member str '("st" "nd" "rd" "th") :test #'string-equal))))

(defun str-to-relative-dow (str)
  (cond ((member str '("last" "prev" "previous") :test #'string-equal) :prev)
	((member str '("next" "coming") :test #'string-equal) :next)
	((string-equal str "this") :closest)
	(t nil)))

(defparameter +date-words+ '(("today" . 0) ("tomorrow" . 1) ("yesterday" . -1)))
(defun str-to-relative-date (str)
  (cdr (find str +date-words+ :test #'string-equal :key #'car)))
