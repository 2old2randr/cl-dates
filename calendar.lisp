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

;;;; Calendar.lisp - Holiday calendar class and associated methods to
;;;; classify dates as business days / holidays / weekends

(in-package :cl-dates)

(defparameter +default-weekend+ '(:saturday :sunday))

(defparameter +known-centres+
  ;;    Centre              Hol function     discriminant
  (list :Australia   (list #'aus-holidays    :settlement)
	:Sydney      (list #'aus-holidays    :settlement)
	:AUD         (list #'aus-holidays    :settlement)
	:Eurex       (list #'german-holidays :Eurex)
	:Frankfurt   (list #'german-holidays :Eurex)
	:Euwax       (list #'german-holidays :Euwax)
	:Stuttgart   (list #'german-holidays :Euwax)
	:Germany     (list #'german-holidays :settlement)
	:DEM         (list #'german-holidays :settlement) ; Deutsche Mark doesn't exist but still ...
	:Tokyo       (list #'japan-holidays  :settlement)
	:Japan       (list #'japan-holidays  :settlement)
	:JPY         (list #'japan-holidays  :settlement)
	:Zurich      (list #'swiss-holidays  :settlement)
	:Switzerland (list #'swiss-holidays  :settlement)
	:CHF         (list #'swiss-holidays  :settlement)
	:TARGET      (list #'target-holidays :settlement)
	:EUR         (list #'target-holidays :settlement)
	:London      (list #'uk-holidays     :settlement)
	:UK          (list #'uk-holidays     :settlement)
	:GBP         (list #'uk-holidays     :settlement)
	:US          (list #'us-holidays     :settlement)
	:USD         (list #'us-holidays     :settlement)
	:NYSE        (list #'us-holidays     :NYSE)
	:NY          (list #'us-holidays     :NYSE)
	:UST         (list #'us-holidays     :bonds)
	:US-Bond     (list #'us-holidays     :bonds)))

;; base class - no holidays other than weekends
(defclass calendar ()
  ((name :initarg :name :accessor name)
   (weekend :initarg :weekend :accessor weekend)))

(defmethod print-object ((cal calendar) stream)
  (print-unreadable-object (cal stream :type t)
    (format stream "Name: ~a; Weekend: ~a; No holidays defined" (name cal) (weekend cal))))

;; The main class - holds holidays corresponding to a single location / trading centre
;; Holidays are lazily loaded as and when required
(defclass calendar-single (calendar)
  ((centre :initarg :centre :accessor centre) ; trading centre
   (holidays :initarg :holidays :accessor holidays) ; hash table with dates as keys
   (holidays-min-year :accessor holidays-min-year)  ; earliest year for which hols are loaded
   (holidays-max-year :accessor holidays-max-year))) ; last year for which hols are loaded

(defmethod print-object ((cal calendar-single) stream)
  (print-unreadable-object (cal stream :type t)
    (format stream "Name: ~a (Centre: ~a); Weekend: ~a; Holidays for ~d-~d"
	    (name cal) (centre cal) (weekend cal)
	    (holidays-min-year cal) (holidays-max-year cal))))

;; Container for multiple calendars
;; A date is deemed to be a holiday if it is a holiday in any of the contained calendars
(defclass calendar-union (calendar)
  ((calendars :initarg :calendars :accessor calendars))) ; list of calendars

(defmethod print-object ((cal calendar-union) stream)
  (print-unreadable-object (cal stream :type t)
    (format stream "Name: ~a; ~a"
	    (name cal) (calendars cal))))

(defun make-calendar (centres &key (name nil) (weekend (copy-list +default-weekend+))
				   (holidays nil) (base-year 0))
  "Create a new holiday calendar. Holidays are populated from a holiday computation
function (if available) and holidays explicitly specified (if any).

The 'centres' parameter may be
	a keyword - an object of type calendar-single will be returned
	a list of keywords - an object of type calendar-union is returned, each contained calendar
			     will be of type calendar-single
        any other object - an object of type calendar will be returned.
The keyword parameters are:
name - a name used when printing the calendar (not used by any code)
weekend - the days comprising the weekend (Sat/Sun by default)
holidays - a list of dates that are holidays. This is useful when special holidays are declared
	   or when there is no function available to generate holidays.
base-year - Year around which holidays should be generated initially (current year if not specified).
            Holidays are generated from base year - 2 to base year + 3."
  (typecase centres
    (keyword (multiple-value-bind (hols from-year to-year) (get-holiday-hash centres base-year)
		(let ((cal (make-instance 'calendar-single :name name :centre centres
							   :weekend weekend :holidays hols)))
		  (setf (holidays-min-year cal) from-year
			(holidays-max-year cal) to-year)
		  (unless name
		    (setf (name cal) (string centres)))
		  (when holidays
		    (dolist (hol holidays)
		      (add-holiday cal hol)))
		  cal)))
    ((and list (not null)) (let ((cal (make-instance 'calendar-union :name name
								     :calendars nil)))
			     (unless name
			       (setf (name cal) (format nil "~{~a~^+~}" centres)))
			     (dolist (centre centres)
			       (push (make-calendar centre :name nil :weekend weekend
							   :holidays holidays
							   :base-year base-year)
				     (calendars cal)))
			     cal))
    (t (when (null name)
	 (setf name "Null Calendar"))
     (make-instance 'calendar :name name :weekend weekend))))

(defun known-centres ()
  "Return centres for which holiday auto-generation is supported"
  (loop for x on +known-centres+ by #'cddr
	collect (car x)))

(defun get-holidays-for-centre (centre year)
  "Return a list of holidays for the given centre and year. NIL is returned if
there is no available function to generate holidays"
  (let ((spec (getf +known-centres+ centre)))
    (if spec
	(funcall (first spec) year :which (second spec))
	nil)))

(defun get-holiday-hash (centre base-year)
  "Return a hash table with holidays as keys. Holidays are generated from base=year - 2
to base-year + 3."
  (when (or (not (integerp base-year)) (<= base-year 1950))
    (setf base-year (date->ymd (todays-date))))
  (let* ((from-year (- base-year 2))
	 (to-year (+ base-year 3))
	 (hols (loop for yy from from-year upto to-year
		     collect (get-holidays-for-centre centre yy)))
	 (hash (make-hash-table :test 'equal)))
    (dolist (hols-for-year hols)
      (dolist (hol hols-for-year)
	(setf (gethash hol hash) 1)))
    (values hash from-year to-year)))

(defmethod has-holidays-for-year ((cal calendar) (year integer))
  "Have holidays been generated for the current year? Always true since this
class does not store holidays"
  t)

(defmethod has-holidays-for-year ((cal calendar-single) (year integer))
  "Have holidays been generated for the current year?"
  (<= (holidays-min-year cal) year (holidays-max-year cal)))

(defmethod has-holidays-for-year ((cal calendar-union) (year integer))
  "Have holidays been generated for the current year? Check every sub-calendar"
  (every #'identity (mapcar (lambda(x) (has-holidays-for-year x year)) (calendars cal))))

(defmethod update-calendar ((cal calendar) (year integer))
  cal)

(defmethod update-calendar ((cal calendar-single) (year integer))
  "Extend generated holidays to include given year. All intervening years
are also generated so that the range of years is continuous"
  (let ((from-year (min (- year 2) (1+ (holidays-max-year cal))))
	(to-year (max (+ year 3) (1- (holidays-min-year cal)))))
    (loop for yy from from-year upto to-year
	  when (or (< yy (holidays-min-year cal))
		   (> yy (holidays-max-year cal)))
	    do (let ((hols (get-holidays-for-centre (centre cal) yy)))
		 (dolist (hol hols)
		   (setf (gethash hol (holidays cal)) 1))))
    (setf (holidays-min-year cal) (min (holidays-min-year cal) from-year)
	  (holidays-max-year cal) (max (holidays-max-year cal) to-year)))
  cal)

(defmethod update-calendar ((cal calendar-union) (year integer))
  "Extend generated holidays to include given year in all sub-calendars"
  (dolist (c (calendars cal))
    (update-calendar c year))
  cal)

(defmethod weekend-p (date (weekend-days list))
  "Return true if day of week of the given date is on a weekend"
  (member (day-of-week date) weekend-days))

(defmethod weekend-p (date (cal calendar))
  "Return true if day of week of the given date is on a weekend"
  (weekend-p date (weekend cal)))

(defmethod weekday-p (date (weekend-days list))
  "Return true if day of week of the given date is not on a weekend"
  (not (weekend-p date weekend-days)))

(defmethod weekday-p (date (cal calendar))
  "Return true if day of week of the given date is not on a weekend"
  (weekday-p date (weekend cal)))

(defmethod holiday-p (date (cal calendar))
  "True if date is a holiday in this calendar"
  (declare (ignore date cal))
  nil)

(defmethod holiday-p (date (cal calendar-single))
  "True if date is a holiday in this calendar"
  (let ((year (date->ymd date)))
    (unless (has-holidays-for-year cal year)
      (update-calendar cal year)))
  (not (null (gethash (jday-number date) (holidays cal)))))

(defmethod holiday-p (date (cal calendar-union))
  "True if date is a holiday in any contained calendar"
  (some #'identity (lambda (cal) (holiday-p date cal)) (calendars cal)))

(defmethod business-day-p (date (cal calendar))
  "True if date is not a holiday or weekend in the given calendar"
  (and (weekday-p date cal)
       (not (holiday-p date cal))))

(defmethod add-holiday ((cal calendar-single) date)
  "Mark given date as a holiday in the calendar"
  (let ((year (date->ymd date)))
    (unless (<= (holidays-min-year cal) year (holidays-max-year cal))
      (update-calendar cal year)))
  (setf (gethash (jday-number date) (holidays cal)) 2))

(defmethod remove-holiday ((cal calendar-single) date)
  "Remove given date from list of holidays in the calendar"
  (remhash (jday-number date) (holidays cal)))
