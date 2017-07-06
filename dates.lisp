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

(defun jday-number (date)
  "Returns the Julian day number for the given Julian date"
  (floor (+ date 1/2)))

(defparameter +days-of-week+ #(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))
(defun day-of-week (date)
  "Returns the day of week on which the given Julian date falls"
  (aref +days-of-week+ (mod (jday-number date) 7)))

(defun ymd->date (yy mm dd &optional (hour 0) (min 0) (sec 0) zone)
  "Return the Julian date corresponding to the given date and time. No
compensation is made for the Gregorian Calendar introduction.
Note that the Julian date is an integer when the time is noon so a
date without time (midnight) will have a fractional part of 0.5.

If timezone is specified, it should be either an alphabetic code e.g., \"IST\"
or a numeric offset in fractions of an hour e.g., +5.5"
  (let* ((a (floor (* 1/12 (- 14 mm))))
	 (b (- yy a))
	 (c (floor (/ b 100)))
	 (day-frac (hms->day-fraction hour min sec))
	 (jdate (+ (floor (* 30.6001d0 (+ (* a 12) mm 1)))
		   (- (floor (* 365.25d0 (+ b 4716))) 1524)
		   (floor (/ c 4))
		   (- c)
		   2
		   dd
		   (- day-frac 0.5d0)))
	 (offset (zone-to-offset zone)))
    (- jdate (/ offset 24))))

(defun date->ymd (date)
  "Returns 6 values corresponding to the given datetime-
year, month, day, hour, minute, second. Second may be a floating
point value but the first five are aways integers"
  (let* ((jd (jday-number date))
	 (e (floor (/ (- jd 1867216.25d0) 36524.25d0)))
	 (f (+ 1 jd e (- (floor (/ e 4)))))
	 (g (+ f 1524))
	 (h (floor (/ (- g 122.1d0) 365.25d0)))
	 (i (floor (* 365.25d0 h)))
	 (j (floor (/ (- g i) 30.6001d0)))
	 (dd (- g i (floor (* j 30.6001d0))))
	 (mm (- j (* 12 (floor (/ j 14))) 1))
	 (yy (+ h (floor (* 1/12 (- 14 mm))) -4716))
	 (day-frac (multiple-value-bind (day frac) (truncate date)
		     (declare (ignore day))
		     (if (>= frac 0.5d0)
			 ;; midnight to noon
			 (- frac 0.5d0)
			 (+ frac 0.5d0)))))
    (multiple-value-bind (h m s) (day-fraction->hms day-frac)
      (values yy mm dd h m s))))

(defun valid-date-p (yy mm dd)
  "Check that year, month and day form a valid calendar date"
  (multiple-value-bind (y m d hr mn sc) (date->ymd (ymd->date yy mm dd))
    (declare (ignore hr mn sc))
    (and (= yy y) (= mm m) (= dd d))))

(defun valid-time-p (h m s)
  "Check that hour, minute and second are valid numbers"
  (and (<= 0 h 23) (<= 0 m 59) (>= s 0) (< s 60)))

(defun todays-date ()
  "Return current date-time (UTC)"
  (multiple-value-bind (s m h dd mm yy dw dst zone)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore h m s dw dst zone))
    (ymd->date yy mm dd)))

(defun todays-datetime (&optional zone)
  "Return current date-time (UTC)"
  (let ((offset (zone-to-offset zone)))
    (multiple-value-bind (s m h dd mm yy dw dst zone)
	(decode-universal-time (get-universal-time) 0)
      (declare (ignore dw dst zone))
      (- (ymd->date yy mm dd h m s) (/ offset 24)))))

(defun date->javascript-time (date)
  "Convert a datetime to the corresponding time value in Javascript"
  (let ((days (- date 2440587.5d0)))	; days since 1-Jan-1970
    (truncate (* days 86400 1000))))	; number of milliseconds

(defun easter-day (yy &optional (want-gregorian-date nil))
  "Returns the date for Easter Sunday in the given year.
Accurate until approx. 4000 CE
Returns a Julian date if want-gregorian-date is NIL. Otherwise,
it returns year, month, day, hour, minute and second as 6 values"
  (let* ((century (floor (/ yy 100)))
	 (remain-19 (mod yy 19))
	 (temp (+ (floor (* 1/2 (- century 15)))
		  202
		  (* -11 remain-19))))
    ;; calculate date of Paschal moon
    (cond ((member century '(21 24 25 27 28 29 30 31 32 34 35 38))
	   (decf temp))
	  ((member century '(33 36 37 39 40))
	   (decf temp 2)))
    (setf temp (mod temp 30))
    (let ((temp-a (+ temp 21)))
      (when (or (= 29 temp)
		(and (= 28 temp) (> remain-19 10)))
	(decf temp-a))
      ;; find next Sunday
      (let* ((temp-b (mod (- temp-a 19) 7))
	     (temp-c (mod (- 40 century) 4)))
	(when (= temp-c 3)
	  (incf temp-c))
	(when (> temp-c 1)
	  (incf temp-c))
	(setf temp (mod yy 100))
	(let* ((temp-d (mod (+ temp (floor (/ temp 4))) 7))
	       (temp-e (1+ (mod (- 20 temp-b temp-c temp-d) 7)))
	       (dd (+ temp-a temp-e))
	       (mm 3))
	  (when (> dd 31)
	    (setf dd (- dd 31)
		  mm 4))
	  (if want-gregorian-date
	      (values yy mm dd)
	      (ymd->date yy mm dd)))))))

(defun vernal-equinox (yy &optional (want-gregorian-date nil))
  "Return UTC date-time of the vernal (spring) equinox for the given year.
Returns a Julian date if want-gregorian-date is NIL. Otherwise,
it returns year, month, day, hour, minute and second as 6 values"
  (calc-equinox-or-solstice-date 1 yy want-gregorian-date))
(defun summer-solstice (yy &optional (want-gregorian-date nil))
  "Return UTC date-time of the summer solstice for the given year.
Returns a Julian date if want-gregorian-date is NIL. Otherwise,
it returns year, month, day, hour, minute and second as 6 values"
  (calc-equinox-or-solstice-date 2 yy want-gregorian-date))
(defun autumnal-equinox (yy &optional (want-gregorian-date nil))
  "Return UTC date-time of the autumnal equinox for the given year.
Returns a Julian date if want-gregorian-date is NIL. Otherwise,
it returns year, month, day, hour, minute and second as 6 values"
  (calc-equinox-or-solstice-date 3 yy want-gregorian-date))
(defun winter-solstice (yy &optional (want-gregorian-date nil))
  "Return UTC date-time of the winter solstice for the given year.
Returns a Julian date if want-gregorian-date is NIL. Otherwise,
it returns year, month, day, hour, minute and second as 6 values"
  (calc-equinox-or-solstice-date 4 yy want-gregorian-date))

;; Formulae are from Chapter 27 of Jan Meeus' Astronomical Algorithms
;; Valid for years between 1000-3000
;; Accurate to within a minute from 1950-2050
(defun calc-equinox-or-solstice-date (which yy want-gregorian-date)
  (let* ((mf (/ (- yy 2000) 1000))
	 (date (cond ((= which 1) (+ 2451623.80984d0
				     (* mf 365242.37404d0)
				     (* mf mf 0.05169d0)
				     (* mf mf mf -0.00411d0)
				     (* mf mf mf mf -0.00057d0)))
		     ((= which 2) (+ 2451716.56767d0
				     (* mf 365241.62603d0)
				     (* mf mf 0.00325d0)
				     (* mf mf mf 0.00888d0)
				     (* mf mf mf mf -0.00030d0)))
		     ((= which 3) (+ 2451810.21715d0
				     (* mf 365242.01767d0)
				     (* mf mf -0.11575d0)
				     (* mf mf mf -0.00337d0)
				     (* mf mf mf mf 0.00078d0)))
		     ((= which 4) (+ 2451900.05952d0
				     (* mf 365242.74049d0)
				     (* mf mf -0.06223d0)
				     (* mf mf mf -0.00823d0)
				     (* mf mf mf mf 0.00032d0)))
		     (t (error "Invalid param which: ~a" which)))))
    (setf date (apply-correction-factors yy date))
    (if want-gregorian-date
	(date->ymd date)
	date)))

(defun cos-degrees (deg)
  (cos (/ (* deg pi) 180d0)))

;; Jan Meeus' Astronomical Algorithms, Chapter 27
(defun apply-correction-factors (yy jd)
  (let* ((t0 (/ (- jd 2451545d0) 36525d0))
	 (w (- (* 35999.373d0 t0) 2.47d0))
	 (dl (+ 1.0 (* 0.0334d0 (cos-degrees w))
		(* 0.0007d0 (cos-degrees (* 2.0d0 w)))))
	 (s (periodic-24 t0)))
    ;; get correct time in TDT (terrestrial dynamic time)
    (incf jd (/ (* 0.00001d0 s) dl))
    ;; apply correction TDT -> UTC
    (tdt-to-utc yy jd)))

;; Jan Meeus' Astronomical Algorithms, Chapter 27
(defparameter +periodic-24-a+ #(485d0 203d0 199d0 182d0 156d0 136d0 77d0 74d0 70d0
				58d0 52d0 50d0 45d0 44d0 29d0 18d0 17d0 16d0 14d0
				12d0 12d0 12d0 9d0 8d0))
(defparameter +periodic-24-b+ #(324.96d0 337.23d0 342.08d0 27.85d0 73.14d0 171.52d0
				222.54d0 296.72d0 243.58d0 119.81d0 297.17d0 21.02d0
				247.54d0 325.15d0 60.93d0 155.12d0 288.79d0 198.04d0
				199.76d0 95.39d0 287.11d0 320.81d0 227.73d0 15.45d0))
(defparameter +periodic-24-c+ #(1934.136d0 32964.467d0 20.186d0 445267.112d0 45036.886d0
				22518.443d0 65928.934d0 3034.906d0 9037.513d0 33718.147d0
				150.678d0 2281.226d0 29929.562d0 31555.956d0 4443.417d0
				67555.328d0 4562.452d0 62894.029d0 31436.921d0 14577.848d0
				31931.756d0 34777.259d0 1222.114d0 16859.074d0))
(defun periodic-24 (t0)
  (loop for i from 0 below 24
	summing (* (aref +periodic-24-a+ i)
		   (cos-degrees (+ (aref +periodic-24-b+ i)
				   (* t0 (aref +periodic-24-c+ i)))))))

;; TDT -> UTC conversion: from Meeus' Astronomical Algorithms, Chapter 10
;; Applies affsets in seconds to convert from Terrestrial Dynamic Time to UTC
;; 
;; Offsets are directly available for even-numbered years between 1620-2002.
;; Offsets for odd-numbered years are linearly interpolated.
;; Offsets for years before 1620 and after 2002 (upto 2100) are obtained by
;; applying interpolation formulae.
;; 2000 and 2002 year data is from NASA and others from Meeus.
(defparameter +tdt-offsets+
  #(121 112 103 95 88 82 77 72 68 63 60 56 53 51 48 46 44 42 40 38 ; 1620-1658
    35 33 31 29 26 24 22 20 18 16 14 12 11 10 9 8 7 7 7 7	   ; 1660-1698
    7 7 8 8 9 9 9 9 9 10 10 10 10 10 10 10 10 11 11 11		   ; 1700-1738
    11 11 12 12 12 12 13 13 13 14 14 14 14 15 15 15 15 15 16 16	   ; 1740-1778
    16 16 16 16 16 16 15 15 14 13 13.1 12.5 12.2 12 12 12 12 12 12 11.9 ; 1780-1818
    11.6 11 10.2 9.2 8.2 7.1 6.2 5.6 5.4 5.3 5.4 5.6 5.9 6.2 6.5   ; 1820-1848
    6.8 7.1 7.3 7.5 7.6 7.7 7.3 6.2 5.2 2.7 1.4 -1.2 -2.8 -3.8 -4.8 ; 1850-1878
    -5.5 -5.3 -5.6 -5.7 -5.9 -6 -6.3 -6.5 -6.2 -4.7 -2.8 -0.1 2.6 5.3 7.7 ; 1880-1908
    10.4 13.3 16 18.2 20.2 21.1 22.4 23.5 23.8 24.3 24 23.9 23.9 23.7 24 ; 1910-1938
    24.3 25.3 26.2 27.3 28.2 29.1 30 30.7 31.4 32.2 33.1 34 35 36.5 38.3 ; 1940-1968
    40.2 42.2 44.5 46.5 48.5 50.5 52.5 53.8 54.9 55.8 56.9 58.3 60 61.6 63 ; 1970-1998
    63.8 64.3))				                           ; 2000-2002
(defparameter +offsets-first-year+ 1620)
(defparameter +offsets-last-year+ (+ +offsets-first-year+
				     (* 2 (1- (length +tdt-offsets+)))))
(defun tdt-to-utc (yy jd)
  (let* ((yt (/ (- yy 2000) 100d0))	; centuries from epoch
	 (delta-t			; calculated offset in seconds
	   (cond ((<= +offsets-first-year+ yy +offsets-last-year+)
		  ;; correction directly available from table
		  (if (evenp yy)
		      ;; lookup directly
		      (aref +tdt-offsets+ (* 1/2 (- yy +offsets-first-year+)))
		      ;; interpolate
		      (* 1/2 (+ (aref +tdt-offsets+ (* 1/2 (- (1+ yy) +offsets-first-year+)))
				(aref +tdt-offsets+ (* 1/2 (- (1- yy) +offsets-first-year+)))))))
		 ((< yy 948)
		  (+ 2177d0 (* 497d0 yt) (* 44.1d0 yt yt)))
		 ((< yy 947 +offsets-first-year+)
		  (+ 102d0 (* 102d0 yt) (* 25.3d0 yt yt)))
		 ((<= 2000 yy 2100)
		  (+ 102d0 (* 102d0 yt) (* 25.3d0 yt yt)
		     (* 0.37d0 (- yy 2100))))
		 ;; no adjustment available for years later than 2100
		 (t 0))))
    ;; decrement date by offset seconds (as a fraction of a day)
    (decf jd (hms->day-fraction 0 0 delta-t))))
