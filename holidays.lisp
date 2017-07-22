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

;;;
;;; holidays.lisp - Functions to generate holidays in various countries
;;; for any given year.
;;;
(in-package :cl-dates)

(defun german-holidays (year &key (which :settlement))
  "German holidays for settlement, Eurex and Euwax"
  (let ((hols (list (ymd->date year 1 1)))) ; New year's day
    (let ((easter (easter-day year)))
      (push (- easter 2) hols)		 ; Good Friday
      (push (+ easter 1) hols)		 ; Easter Monday
      (when (or (eq which :settlement) (eq which :euwax))
	(push (+ easter 39) hols))	 ; Ascension day
      (when (eq which :settlement)
	(push (+ easter 50) hols)	 ; Whit Monday
	(push (+ easter 60) hols)))	 ; Corpus Christi
    (push (ymd->date year 5 1) hols)	 ; May Day
    (when (eq which :settlement)
      (push (ymd->date year 10 3) hols)) ; National day
    (push (ymd->date year 12 24) hols)   ; Xmas eve
    (push (ymd->date year 12 25) hols)   ; Christmas
    (push (ymd->date year 12 26) hols)   ; Boxing day
    (push (ymd->date year 12 31) hols)   ; New year's eve
    ;; Remove any dates falling on weekends
    (setf hols (remove-if-not (lambda(dt) (weekday-p dt '(:saturday :sunday))) hols))
    (remove-duplicates (sort (mapcar #'jday-number hols) #'<))))

(defun japan-holidays (year &key (which :settlement))
  "Japan holidays for settlement (which = nil returns only national holidays)"
  (labels ((move-sun-next-bday (date-list)
	     (let ((moved-dates nil)
		   (dates (mapcar #'jday-number date-list)))
	       (dolist (dt dates)
		 (if (not (eq :sunday (day-of-week dt)))
		     (push dt moved-dates) ; Mon-Sat - no adjustment
		     (let ((moved-date (loop for x = (1+ dt) then (1+ x)
					     until (not (member x dates :test #'=))
					     finally (return x))))
		       (push moved-date moved-dates))))
	       moved-dates)))
    (let ((hols (list (ymd->date year 1 1)))
	  (ae (+ 9/24 (autumnal-equinox year)))) ; time in JST needed since date may change
      ;; Coming of Age day
      (if (< 1947 year 2000)
	  (push (ymd->date year 1 15) hols)
	  (push (nth-day-of-week (ymd->date year 1 1) :monday 2) hols))
      (when (> year 1966)
	(push (ymd->date year 2 11) hols))	 ; Foundation day
      (push (+ 9/24 (vernal-equinox year)) hols) ; Spring equinox
      (push (ymd->date year 4 29) hols)	; Showa day - start of Golden Week
      (when (>= year 1948)
	(push (ymd->date year 5 3) hols)) ; Constitution Memorial day
      (when (>= year 1989)
	(push (ymd->date year 5 4) hols)) ; Greenery day
      (when (>= year 1948)
	(push (ymd->date year 5 5) hols)) ; Children's day
      ;; Marine day
      (if (< 1995 year 2003)
	  (push (ymd->date year 7 26) hols)
	  (when (>= year 2003)
	    (push (nth-day-of-week (ymd->date year 7 1) :monday 3) hols)))
      (when (>= year 2016)
	;; Mountain day
	(push (ymd->date year 8 11) hols))
      ;; Respect for the Aged day
      (if (< 1965 year 2003)
	  (push (ymd->date year 9 15) hols)
	  (when (>= year 2003)
	    (push (nth-day-of-week (ymd->date year 9 1) :monday 3) hols)))
      ;; Silver week - if a single day between AE and RA day, that day is also a holiday
      (when (and (>= year 2003)
		 (date< (car hols) ae)
		 (= 2 (diff-days (car hols) ae)))
	(push (1+ (car hols)) hols))
      (push ae hols)			; Autumn equinox

      ;; Health and Sports day
      (if (< 1965 year 2000)
	  (push (ymd->date year 10 10) hols)
	  (when (>= year 2000)
	    (push (nth-day-of-week (ymd->date year 10 1) :monday 2) hols)))
      (when (>= year 1948)
	(push (ymd->date year 11 3) hols) ; Culture day
	(push (ymd->date year 11 23) hols)) ; Labor Thanksgiving day
      (when (>= year 1989)
	(push (ymd->date year 12 23) hols)) ; Emperor's birthday

      ;; Above holidays are movable from Sun to Mon
      (setf hols (move-sun-next-bday hols))

      ;; Add bank holidays (not adjusted)
      (when (eq which :settlement)
	(push (ymd->date year 1 2) hols)
	(push (ymd->date year 1 3) hols)
	(push (ymd->date year 12 31) hols))

      ;; Remove any dates falling on weekends
      (setf hols (remove-if-not (lambda(dt) (weekday-p dt '(:saturday :sunday))) hols))

      ;; One-off special holidays
      (cond ((= year 1959) (push (ymd->date year 4 10) hols)) ; Royal wedding - Akihito
	    ((= year 1989) (push (ymd->date year 4 10) hols)) ; Showa Emperor funeral
	    ((= year 1990) (push (ymd->date year 4 10) hols)) ; Heisei enthronement
	    ((= year 1993) (push (ymd->date year 4 10) hols))) ; Royal wedding - Naruhito
      (remove-duplicates (sort (mapcar #'jday-number hols) #'<)))))

(defun swiss-holidays (year &key (which nil))
  "Swiss holidays for settlement"
  (declare (ignore which))
  (let ((hols (list (ymd->date year 1 1)))) ; New year's day
    (push (ymd->date year 1 2) hols)	; Bercholdstag
    (let ((easter (easter-day year)))
      (push (- easter 2) hols)		; Good Friday
      (push (+ easter 1) hols)		; Easter Monday
      (push (+ easter 39) hols)		; Ascension day
      (push (+ easter 50) hols))	; Whit Monday
    (push (ymd->date year 5 1) hols)	; May Day
    (push (ymd->date year 8 1) hols)	; National day
    (push (ymd->date year 12 25) hols)	; Christmas
    (push (ymd->date year 12 26) hols)	; St. Stephen's day
    ;; Remove any dates falling on weekends
    (setf hols (remove-if-not (lambda(dt) (weekday-p dt '(:saturday :sunday))) hols))
    (remove-duplicates (sort (mapcar #'jday-number hols) #'<))))

(defun target-holidays (year &key (which nil))
  "TARGET holidays for settlement"
  (declare (ignore which))
  (let ((hols (list (ymd->date year 1 1)))) ; New year's day
    (when (>= year 2000)
      (let ((easter (easter-day year)))
	(push (- easter 2) hols)	  ; Good Friday
	(push (+ easter 1) hols))	  ; Easter Monday
      (push (ymd->date year 5 1) hols)	  ; May Day
      (push (ymd->date year 12 25) hols)  ; Christmas
      (push (ymd->date year 12 26) hols)) ; Day of goodwill
    ;; Remove any dates falling on weekends
    (setf hols (remove-if-not (lambda(dt) (weekday-p dt '(:saturday :sunday))) hols))
    (when (or (= year 1998) (= year 1999) (= year 2001))
      (push (ymd->date year 12 31) hols)) ; New year's eve
    (remove-duplicates (sort (mapcar #'jday-number hols) #'<))))

(defun uk-holidays (year &key (which nil))
  "UK holidays for settlement and stock exchange"
  (declare (ignore which))
  (let (hols)
    ;; New year's day - move to Monday if on weekend
    (let* ((ny-day (ymd->date year 1 1))
	   (dow (day-of-week ny-day)))
      (cond ((eq dow :saturday) (push (ymd->date year 1 3) hols))
	    ((eq dow :sunday) (push (ymd->date year 1 2) hols))
	    (t (push ny-day hols))))
    (let ((easter (easter-day year)))
      (push (- easter 2) hols)		; Good Friday
      (push (+ easter 1) hols)		; Easter Monday
      (when (<= year 1967)
	(push (+ easter 50) hols)))	; Whit Monday - replaced by spring bank hol
    ;; Early May bank holiday (1st Mon of May)
    (push (nth-day-of-week (ymd->date year 5 1) :monday 1) hols)
    ;; Spring bank holiday (last Mon of May)
    (when (>= year 1971)
      (push (nth-day-of-week (ymd->date year 5 1) :monday 5) hols))
    ;; Summer bank holiday (last Mon of Aug)
    (push (nth-day-of-week (ymd->date year 8 1) :monday 5) hols)
    ;; Christmas & Boxing day - moved to a weekday if on weekend
    (let* ((xmas (ymd->date year 12 25))
	   (dow (day-of-week xmas)))
      (cond ((eq dow :friday) (progn (push xmas hols)
				     (push (+ xmas 3) hols)))
	    ((eq dow :saturday) (progn (push (+ xmas 2) hols)
				       (push (+ xmas 3) hols)))
	    ((eq dow :sunday) (progn (push (+ xmas 1) hols)
				     (push (+ xmas 2) hols)))
	    (t (progn (push xmas hols)
		      (push (+ xmas 1) hols)))))
    ;; One-off holidays
    (when (= year 1999)
      ;; Millenium eve
      (push (ymd->date year 12 31) hols))
    (when (= year 2002)
      ;; Golden jubilee
      (push (ymd->date year 6 3) hols)
      (push (ymd->date year 6 4) hols))
    (when (= year 2011)
      ;; Royal wedding
      (push (ymd->date year 4 29) hols))
    (when (= year 2012)
      ;; Diamond jubilee
      (push (ymd->date year 6 4) hols)
      (push (ymd->date year 6 5) hols))
    (remove-duplicates (sort (mapcar #'jday-number hols) #'<))))

(defun us-holidays (year &key (which :settlement))
  "US holidays for settlement, NYSE or Bonds"
  (labels ((move-sat-sun-to-fri-mon (date)
	     (let ((dow (day-of-week date)))
	       (cond ((eq dow :saturday) (1- date))
		     ((eq dow :sunday) (1+ date))
		     (t date)))))
    (let (hols)
      ;; New year's day - move to Monday if on Sunday (Saturday is ignored
      ;; since holiday will fall in previous year)
      (let* ((ny-day (ymd->date year 1 1))
	     (dow (day-of-week ny-day)))
	(cond ((eq dow :sunday) (push (1+ ny-day) hols))
	      ((not (eq dow :saturday)) (push ny-day hols))))
      (when (and (eq which :settlement)
		 (eq :friday (day-of-week (ymd->date year 12 31))))
	;; NY eve is a settlement holiday if next year's NY day is on Sat
	(push (ymd->date year 12 31) hols))
      (when (or (and (> year 1982) (or (eq which :settlement) (eq which :bonds)))
		(and (> year 1997) (eq which :nyse)))
	;; MLK day - 3rd Mon of Jan
	(push (nth-day-of-week (ymd->date year 1 1) :monday 3) hols))
      ;; President's day
      (if (> year 1970)
	  ;; 3rd Mon of Feb from 1971
	  (push (nth-day-of-week (ymd->date year 2 1) :monday 3) hols)
	  ;; Feb 22 (adjusted) before
	  (push (move-sat-sun-to-fri-mon (ymd->date year 2 22)) hols))
      (when (or (eq which :nyse) (eq which :bonds))
	;; Good Friday - only for stock and bond markets
	(push (- (easter-day year) 2) hols))
      ;; Memorial day
      (if (> year 1970)
	  ;; last Mon of May from 1971
	  (push (nth-day-of-week (ymd->date year 5 1) :monday 5) hols)
	  ;; May 30 (adjusted) before
	  (push (move-sat-sun-to-fri-mon (ymd->date year 5 30)) hols))
      ;; Independence day
      (push (move-sat-sun-to-fri-mon (ymd->date year 7 4)) hols)
      ;; Labor day - 1st Mon of Sep
      (push (nth-day-of-week (ymd->date year 9 1) :monday 1) hols)
      (when (not (eq which :nyse))
	;; Columbus day - 2nd Mon of Oct
	(when (> year 1970)
	  (push (nth-day-of-week (ymd->date year 10 1) :monday 2) hols))
	;; Veterans day
	(if (< 1970 year 1978)
	    ;; 4th Mon of Oct, 1971-1977
	    (push (nth-day-of-week (ymd->date year 10 1) :monday 4) hols)
	    ;; 11th Nov (adjusted) otherwise
	    (push (move-sat-sun-to-fri-mon (ymd->date year 11 11)) hols)))
      (when (and (eq which :nyse) (<= 1968 year 1980) (zerop (mod year 4)))
	;; Presidential election day
	(push (nth-day-of-week (ymd->date year 11 1) :tuesday 1) hols))
      ;; Thanksgiving day - 4th Thu of Nov
      (push (nth-day-of-week (ymd->date year 11 1) :thursday 4) hols)
      ;; Christmas
      (push (move-sat-sun-to-fri-mon (ymd->date year 12 25)) hols)

      ;; NYSE special one-off holidays
      (when (eq which :nyse)
	(cond ((= year 1954) (push (ymd->date year 12 24) hols)) ; Xmas eve
	      ((= year 1956) (push (ymd->date year 12 24) hols)) ; Xmas eve
	      ((= year 1958) (push (ymd->date year 12 26) hols)) ; Day after Xmas
	      ((= year 1961) (push (ymd->date year 5 29) hols)) ; Day before Decoration day
	      ((= year 1963) (push (ymd->date year 11 25) hols)) ; Kennedy funeral
	      ((= year 1965) (push (ymd->date year 12 24) hols)) ; Xmas eve
	      ((= year 1968) (progn (push (ymd->date year 4 9) hols) ; MLK mourning
				    ;; Paperwork crisis - 4 day week with Wed closed
				    (let ((eoy (ymd->date year 12 31)))
				      (loop for dt = (ymd->date year 6 12) then (1+ dt)
					    until (date> dt eoy)
					    when (eq (day-of-week dt) :wednesday)
					      do (push dt hols)))
				    (push (ymd->date year 7 5) hols))) ; Bicentennial
	      ((= year 1969) (progn (push (ymd->date year 2 10) hols) ; Heavy snow
				    (push (ymd->date year 3 31) hols) ; Eisenhower funeral
				    (push (ymd->date year 7 21) hols))) ; Moon landing
	      ((= year 1972) (push (ymd->date year 12 28) hols)) ; Truman funeral
	      ((= year 1973) (push (ymd->date year 1 25) hols)) ; Johnson funeral
	      ((= year 1977) (push (ymd->date year 7 14) hols)) ; N. America blackout
	      ((= year 1985) (push (ymd->date year 9 27) hols)) ; Hurricane Gloria
	      ((= year 1994) (push (ymd->date year 4 27) hols)) ; Nixon funeral
	      ((= year 2001) (progn (push (ymd->date year 9 11) hols) ; 9/11 WTC attack
				    (push (ymd->date year 9 12) hols)
				    (push (ymd->date year 9 13) hols)
				    (push (ymd->date year 9 14) hols)))
	      ((= year 2004) (push (ymd->date year 6 11) hols)) ; Reagan funeral
	      ((= year 2007) (push (ymd->date year 1 2) hols)) ; Ford funeral
	      ((= year 2012) (progn (push (ymd->date year 10 29) hols) ; Hurricane Sandy
				    (push (ymd->date year 10 30) hols)))))
      (remove-duplicates (sort (mapcar #'jday-number hols) #'<)))))
