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

(defparameter +white-space+ '(#\space #\tab #\return #\linefeed #\newline))
(defun white-space-p (char)
  (member char +white-space+ :test #'char=))

(defun tokenize (str)
  (let ((token-list nil)
	(state nil)
	(token nil)
	(seen-alpha nil))
    (labels ((split-list (test list &aux (start list) (end list))
	       (loop while (and end (setq start (member-if-not test end)))
		     collect (ldiff start (setq end (member-if test start)))))
	     (emit-token ()
	       (if (and (member state '(:in-alpha-num :in-num-alpha))
			(or seen-alpha
			    (> (count #\. token) 1)
			    (char= #\. (car token)))
			;; ugly special casing for a.m./p.m. strings
			(not (equal token '(#\. #\m #\. #\a)))
			(not (equal token '(#\m #\. #\a)))
			(not (equal token '(#\. #\m #\. #\p)))
			(not (equal token '(#\m #\. #\p))))
		   (progn
		     ;; separate "23.jun.2017" => "23" "." "jun" "." "2017"
		     ;; any trailing period will be dropped
		     (setf token (nreverse token))
		     (loop for tk on (split-list #'(lambda(x) (char= x #\.)) token)
			   do (progn
				(push (coerce (car tk) 'string) token-list)
				(unless (null (cdr tk))
				  (push "." token-list)))))
		   (push (coerce (nreverse token) 'string) token-list))
	       ;; reset state
	       (setf state nil seen-alpha nil token nil)))
      (do ((i 0 (1+ i))
	   (len-str (length str)))
	  ((>= i len-str))
	(let ((ch (schar str i)))
	  (case state
	    ((nil)
	     (push ch token)
	     (setf state (cond ((alpha-char-p ch) :in-alpha)
			       ((digit-char-p ch) :in-num)
			       ((white-space-p ch) :in-space)
			       (t :in-separators))))
	    (:in-separators
	     (if (or (alphanumericp ch) (white-space-p ch))
		 (progn
		   (decf i)
		   (emit-token))
		 (push ch token)))
	    (:in-space
	     (when (not (white-space-p ch))
	       ;; throw away white space and reset state
	       (decf i)
	       (setf token nil state nil)))
	    (:in-alpha
	     (setf seen-alpha t)
	     (cond ((alpha-char-p ch) (push ch token))
		   ((char= ch #\.)
		    (push ch token)
		    (setf state :in-alpha-num))
		   (t (decf i)
		      (emit-token))))
	    (:in-num
	     (cond ((digit-char-p ch) (push ch token))
		   ((char= ch #\.)
		    (push ch token)
		    (setf state :in-num-alpha))
		   (t (decf i)
		      (emit-token))))
	    (:in-alpha-num
	     (setf seen-alpha t)
	     (cond ((or (alpha-char-p ch) (char= ch #\.)) (push ch token))
		   ((and (digit-char-p ch) token (char= #\. (car token)))
		    (push ch token)
		    (setf state :in-num-alpha))
		   (t (decf i)
		      (emit-token))))
	    (:in-num-alpha
	     (cond ((or (digit-char-p ch) (char= ch #\.)) (push ch token))
		   ((and (alpha-char-p ch) token (char= #\. (car token)))
		    (push ch token)
		    (setf state :in-alpha-num))
		   (t(decf i)
		     (emit-token)))))))
      (when (and token (not (member state '(:in-space :in-separators))))
	(emit-token)))
    (nreverse token-list)))

(defun advance-date (dt dow dir)
  (if (eq dir :closest)
      (let ((next (advance-date dt dow :next))
	    (prev (advance-date dt dow :prev)))
	(if (< (- dt prev) (- next dt))
	    prev
	    next))
      (let ((inc (if (eq dir :next) 1 -1)))
	(loop
	      (if (eq (day-of-week dt) dow)
		  (return dt)
		  (incf dt inc))))))

(defun make-four-digit-year (year)
  (if (> year 99)
      year
      (multiple-value-bind (s m h dd mm yy dw dst zone)
	  (decode-universal-time (get-universal-time) 0)
	(declare (ignore s m h dd mm dw dst zone))
	(let ((century (* 100 (truncate (/ yy 100)))))
	  (incf year century)
	  (if (>= (abs (- year yy)) 50)
	      (if (< year yy)
		  (+ year 100)
		  (- year 100))
	      year)))))

;; decide between yy/mm and mm/yy
(defun assign-yy-mm (list precedence)
  (cond ((> (car list) 12) (values (car list) (cadr list)))
	((> (cadr list) 12) (values (cadr list) (car list)))
	((eq precedence :ymd) (values (car list) (cadr list)))
	(t (values (cadr list) (car list)))))

;; decide between yy/dd and dd/yy
(defun assign-yy-dd (list precedence)
  (cond ((> (car list) 31) (values (car list) (cadr list)))
	((> (cadr list) 31) (values (cadr list) (car list)))
	((eq precedence :ymd) (values (car list) (cadr list)))
	(t (values (cadr list) (car list)))))

;; decide between dd/mm and mm/dd
(defun assign-mm-dd (list precedence)
  (cond ((> (car list) 12) (values (cadr list) (car list)))
	((> (cadr list) 12) (values (car list) (cadr list)))
	((or (eq precedence :ymd) (eq precedence :mdy))
	 (values (car list) (cadr list)))
	(t (values (cadr list) (car list)))))

;; decide order in which day, month and year appear given 2 or 3 numbers
(defun assign-yy-mm-dd (list precedence)
  (if (= 2 (length list))
      (cond ((and (> (car list) 12) (> (cadr list) 12))
	     (multiple-value-bind (yy dd) (assign-yy-dd list precedence)
	       (values yy nil dd)))
	    (t (multiple-value-bind (yy mm) (assign-yy-mm list precedence)
		 (values yy mm nil))))
      (let (tmp)
	(cond ((every #'(lambda(x) (<= x 12)) list)
	       ;; 01/02/03
	       (cond ((eq precedence :mdy) (values (third list) (first list) (second list)))
		     ((eq precedence :dmy) (values (third list) (second list) (first list)))
		     (t (values (first list) (second list) (third list)))))
	      ((setf tmp (find-if #'(lambda(x) (> x 31)) list))
	       ;; 12/5/55
	       (setf list (remove tmp list :count 1))
	       (multiple-value-bind (mm dd) (assign-mm-dd list precedence)
		 (values tmp mm dd)))
	      ((= 1 (count-if #'(lambda(x) (<= x 12)) list))
	       (setf tmp (find-if #'(lambda(x) (<= x 12)) list)
		     list (remove tmp list :count 1))
	       (multiple-value-bind (yy dd) (assign-yy-dd list precedence)
		 (values yy tmp dd)))
	      (t ;; 5/6/27
	       (cond ((eq precedence :ymd)
		      (multiple-value-bind (mm dd) (assign-mm-dd (cdr list) precedence)
			(values (car list) mm dd)))
		     ((eq precedence :dmy)
		      (multiple-value-bind (yy mm) (assign-yy-mm (cdr list) precedence)
			(values yy mm (car list))))
		     (t (multiple-value-bind (yy dd) (assign-yy-dd (cdr list) precedence)
			  (values yy (car list) dd)))))))))

;; struct to hold values during parse
(defstruct (date-components (:conc-name dt-))
  dow yr mth day hr min sec tz)

(defparameter +date-separators+ '("." "/" "-"))

(defun string->date (string &key (reference-date (todays-date)) (precedence :ymd))
  (setf string (string-downcase (string-trim +white-space+ string)))
  (unless (member precedence '(:ymd :dmy :mdy))
    (error "invalid precedence ~a" precedence))
  (macrolet ((when-null-set (var field value)
	       `(when (null (,field ,var)) (setf (,field ,var) ,value))))
    (let* ((res (make-date-components))
	   (tokens (tokenize string))
	   (num-tok (length tokens))
	   (skipped-tokens nil)		; tokens that were ignored
	   (relative-dow nil)		; when day of week is qualified by this/next/prev
	   (date-comps nil)		; uncertain whether day/month/year
	   (num-date-comps 0)		; length of date-comps + date components in res
	   (time-parsed nil))		; whether all of h/m/s have been parsed
      (do ((i 0 (1+ i)))
	  ((>= i num-tok))
	(let* ((token (elt tokens i))
	       (len (length token))
	       (next-tok (if (>= (1+ i) num-tok) nil (elt tokens (1+ i))))
	       (prev-tok (if (> i 0) (elt tokens (1- i)) nil))
	       (num (parse-number token))
	       tmp)
	  (when (numberp num)
	    (cond
	      ;; e.g. yyyymmddThhmmss or yyyymmddThh:mm:ss
	      ((and (= num-date-comps 3)
		    (or (= len 2) (= len 4) (= len 6))
		    prev-tok (string= prev-tok "t")
		    (null (find #\. token)))
	       ;; if colon separated time, only hour will be parsed
	       ;; cannot set time-parsed to T unless len > 2
	       (setf (dt-hr res) (parse-integer (subseq token 0 2)))
	       (when (>= len 4)
		 (setf (dt-min res) (parse-integer (subseq token 2 4))
		       time-parsed t))
	       (when (= len 6)
		 (setf (dt-sec res) (parse-integer (subseq token 4)))))
	      ;; YYMMDD or HHMMSS
	      ((or (= len 6)
		   (and (> len 7) (setf tmp (position #\. token)) (= tmp 6)))
	       (if (and (= 0 num-date-comps) (= len 6)
			(or (null prev-tok) (string/= prev-tok "t")))
		   ;; date if not yet parsed and not explicitly marked as time
		   (let* ((yy (truncate (/ num 10000)))
			  (mm (truncate (- (/ num 100) (* yy 100))))
			  (dd (truncate (- num (+ (* yy 10000) (* mm 100))))))
		     (setf date-comps (list dd mm yy)) ; reverse order of specification
		     (incf num-date-comps 3))
		   ;; time
		   (setf (dt-hr res) (parse-integer (subseq token 0 2))
			 (dt-min res) (parse-integer (subseq token 2 4))
			 (dt-sec res) (parse-number (subseq token 4))
			 time-parsed t)))
	      ;; YYYYMMDD - only format for 8 digit date (ddmmyyyy must be e.g. dd/mm/yyyy)
	      ((= len 8)
	       (when (find #\. token)
		 ;; nnnn.nnn
		 (return-from string->date nil))
	       (let* ((yy (truncate (/ num 10000)))
		      (mm (truncate (- (/ num 100) (* yy 100))))
		      (dd (truncate (- num (+ (* yy 10000) (* mm 100))))))
		 (setf (dt-yr res) yy (dt-mth res) mm (dt-day res) dd)
		 (incf num-date-comps 3)))
	      ;; YYYYMMDDHHMM[SS[.ss]]
	      ((or (= len 12) (= len 14)
		   (and (> len 15) (setf tmp (position #\. token)) (= tmp 14)))
	       (setf (dt-yr res) (parse-integer (subseq token 0 4))
		     (dt-mth res) (parse-integer (subseq token 4 6))
		     (dt-day res) (parse-integer (subseq token 6 8))
		     (dt-hr res) (parse-integer (subseq token 8 10))
		     (dt-min res) (parse-integer (subseq token 10 12)))
	       (incf num-date-comps 3)
	       (when (> len 12)
		 (setf (dt-sec res) (parse-number (subseq token 12))))
	       (setf time-parsed t))
	      ;; HH:MM:SS
	      ((or (and next-tok (string= next-tok ":"))
		   (and (not (null (dt-hr res))) prev-tok (string= prev-tok ":")))
	       ;; distinguish case when coming here from yyyymmddThh:mm:ss
	       (unless (and prev-tok (not time-parsed) (string= prev-tok ":"))
		 (setf (dt-hr res) num)
		 (incf i 2))		; advance pointer to minute
	       (setf num (parse-number (elt tokens i)))
	       (when (null num)
		 (return-from string->date nil))
	       ;; also handle weirdness like hh:mm.ss
	       (multiple-value-bind (int frac) (truncate num)
		 (setf (dt-min res) int)
		 (when (> frac 0)
		   (setf (dt-sec res) (* 60 frac))
		   (when (and (< (1+ i) num-tok) (string= (elt tokens (1+ i)) ":"))
		     ;; can't handle hh:mm.ss:wtf
		     (return-from string->date nil))))
	       (when (and (< (1+ i) num-tok) (string= (elt tokens (1+ i)) ":"))
		 (incf i 2)		; position on second
		 ;; both hh:mm:<eos> and hh:mm:ss are acceptable
		 (when (< i num-tok)
		   (setf num (parse-number (elt tokens i)))
		   (when (null num)
		     (return-from string->date nil))
		   (setf (dt-sec res) num))))
	      ;; dd/mm/yy and variants
	      ((and next-tok (member next-tok +date-separators+ :test #'string=))
	       (let ((sep next-tok)
		     (is-ymd nil))
		 (if (> num 31)
		     ;; year - default to yymmdd format
		     (setf (dt-yr res) num
			   is-ymd t)
		     (push num date-comps))
		 (incf num-date-comps)
		 (incf i 2)		; position on second component
		 (when (>= i num-tok)
		   ;; reject strings ending in trailing slash
		   (return-from string->date nil))
		 (setf num (parse-number (elt tokens i)))
		 (if (numberp num)
		     (cond ((and is-ymd (<= num 12)) (setf (dt-mth res) num))
			   (is-ymd (setf (dt-day res) num))
			   (t (push num date-comps)))
		     (progn
		       (setf tmp (str-to-month (elt tokens i)))
		       (if tmp
			   (setf (dt-mth res) tmp)
			   ;; not number or month name
			   (return-from string->date nil))))
		 (incf num-date-comps)
		 (when (and (< (1+ i) num-tok) (string= sep (elt tokens (1+ i))))
		   (incf i 2)		; position on last component
		   (when (>= i num-tok)
		     (return-from string->date nil)) ; trailing slash
		   (setf num (parse-number (elt tokens i)))
		   (when (null num)
		     ;; month name in last position is not accepted
		     (return-from string->date nil))
		   (cond ((and is-ymd (not (null (dt-day res)))) (setf (dt-mth res) num))
			 (is-ymd (setf (dt-day res) num))
			 (t (push num date-comps)))
		   (incf num-date-comps))))
	      ;; 2nd, 3rd, 21st etc. - parse as day
	      ((and next-tok (str-is-day-suffix next-tok))
	       (incf i)			; skip suffix
	       (setf (dt-day res) num)
	       (incf num-date-comps))
	      ;; 13h45m, 13h 45, etc - parse as time
	      ((and next-tok (setf tmp (str-to-hms next-tok)))
	       (loop
		;; consume following tokens as far as possible
		(multiple-value-bind (int frac) (truncate num)
		  (cond ((eq tmp :hour)
			 (setf (dt-hr res) int)
			 (when (> frac 0)
			   (setf (dt-min res) (* 60 frac))))
			((eq tmp :minute)
			 (setf (dt-min res) int)
			 (when (> frac 0)
			   (setf (dt-sec res) (* 60 frac))))
			((eq tmp :second)
			 (setf (dt-sec res) num
			       time-parsed t))))
		(incf i)		; pointer is on suffix
		(when (or (>= (1+ i) num-tok) (eq tmp :second))
		  (return))		; done
		(setf num (parse-number (elt tokens (1+ i))))
		(when (null num)
		  (return))
		(incf i)		; pointer on number after time suffix
		(if (or (>= (1+ i) num-tok)
			(null (str-to-hms (elt tokens (1+ i)))))
		    ;; no time suffix - set to min/sec based on what prev token was
		    (progn
		      (if (eq tmp :hour)
			  (multiple-value-bind (int frac) (truncate num)
			    (setf (dt-min res) int)
			    (when (> frac 0)
			      (setf (dt-sec res) (* 60 frac))))
			  (setf (dt-sec res) num))
		      (setf time-parsed t)
		      (return))
		    ;; set up var for next loop iteration
		    (setf tmp (str-to-hms (elt tokens (1+ i)))))))
	      ;; time with am/pm indicator e.g. 10 am
	      ((and next-tok (setf tmp (str-to-ampm next-tok)))
	       (when (> num 12)
		 (return-from string->date nil))
	       (multiple-value-bind (int frac) (truncate num)
		 (if (and (< int 12) (eq tmp :pm))
		     (incf int 12)
		     (when (and (= int 12) (eq tmp :am))
		       (setf int 0)))
		 (setf (dt-hr res) int)
		 (when (> frac 0)
		   ;; 10.37 pm - frac is minutes not fraction of hour
		   (setf (dt-min res) frac
			 time-parsed t))))
	      ;; all other numbers - assume it is a date component
	      (t (if (and prev-tok (string= prev-tok "of") (not (null (dt-mth res))))
		     ;; june of 1976
		     (setf (dt-yr res) num)
		     (push num date-comps))
		 (incf num-date-comps)))
	    ;; avoid giant if-then-else
	    (go end-of-do-loop))
	  ;; token is not a number
	  (cond
	    ;; weekday
	    ((setf tmp (str-to-weekday token)) (setf (dt-dow res) tmp))
	    ;; relative day of week
	    ((setf tmp (str-to-relative-dow token)) (setf relative-dow tmp))
	    ;; today / tomorrow etc
	    ((setf tmp (str-to-relative-date token))
	     (when (/= 0 num-date-comps)
	       (return-from string->date nil))
	     (multiple-value-bind (yy mm dd h m s) (date->ymd (+ reference-date tmp) :want-time t)
	       (setf (dt-yr res) yy
		     (dt-mth res) mm
		     (dt-day res) dd
		     num-date-comps 3)
	       (when-null-set res dt-hr h)
	       (when-null-set res dt-min m)
	       (when-null-set res dt-sec s)))
	    ;; am/pm
	    ((setf tmp (str-to-ampm token))
	     (let ((hr (dt-hr res)))
	       (when (null hr)
		 ;; am/pm not accepted before time string
		 (return-from string->date nil))
	       (if (and (= hr 12) (eq tmp :am))
		   (setf hr 0)
		   (when (and (< hr 12) (eq tmp :pm))
		     (incf hr 12)))
	       (setf (dt-hr res) hr)))
	    ;; time zone name
	    ((or (and (> (length token) 2) (setf tmp (str-to-tz-offset token)))
		 ;; military time zone spec valid only at end of string
		 (and (<= (length token) 2) (null next-tok) (setf tmp (str-to-tz-offset token))))
	     (when (null (dt-hr res))
	       (return-from string->date nil))
	     ;; if a time zone is specified as e.g. GMT+9 or JST-1,
	     ;; the next cond-clause for numeric offset will process it.
	     ;; offset is stored as fractions of a day since we add it to the julian date
	     (setf (dt-tz res) (/ tmp 24)))
	    ;; Numeric time zone offset: e.g., +0530 or +05:30 or -5
	    ;; also handles GMT+9 etc in conjunction with previous clause
	    ((and (or (string= token "+") (string= token "-"))
		  next-tok (not (null (parse-number next-tok))))
	     (when (null (dt-hr res))
	       (return-from string->date nil))
	     (let ((sign (if (string= token "+") 1 -1))
		   offset-hrs)
	       (incf i)			; position on number
	       (cond
		 ;; e.g. 5:30
		 ((and (< (1+ i) num-tok) (string= ":" (elt tokens (1+ i))))
		  (setf offset-hrs (/ (parse-integer next-tok) 24))
		  (incf i 2) 		; position on minutes
		  (if (or (>= i num-tok) (null (setf tmp (parse-number (elt tokens i)))))
		      (return-from string->date nil)
		      (incf offset-hrs (/ tmp 1440))))
		 ;; e.g., 3.5 or 11
		 ((or (not (null (position #\. next-tok))) (< (length next-tok) 3))
		  (setf offset-hrs (/ (parse-number next-tok) 24)))
		 ;; e.g., 0530
		 ((= 4 (length next-tok))
		  (setf offset-hrs (+ (/ (parse-integer (subseq next-tok 0 2)) 24)
				      (/ (parse-integer (subseq next-tok 2 4)) 1440))))
		 ;; e.g., 530
		 ((= 3 (length next-tok))
		  (setf offset-hrs (+ (/ (parse-integer (subseq next-tok 0 1)) 24)
				      (/ (parse-integer (subseq next-tok 1 3)) 1440))))
		 ;; all others are invalid
		 (t (return-from string->date nil)))
	       (setf offset-hrs (* sign offset-hrs))
	       (if (null (dt-tz res))
		   (setf (dt-tz res) offset-hrs)
		   (incf (dt-tz res) offset-hrs))))
	    ;; Alphabetic month name
	    ((setf tmp (str-to-month token))
	     (when (dt-mth res)
	       (return-from string->date nil)) ; duplicate spec
	     (setf (dt-mth res) tmp)
	     (incf num-date-comps)
	     (when (and next-tok (member next-tok +date-separators+ :test #'string=))
	       ;; Jul/23/1956
	       (let ((sep next-tok))
		 (incf i 2)		; point to second component
		 (when (< i num-tok)
		   (setf num (parse-number (elt tokens i)))
		   (when (null num)
		     (return-from string->date nil))
		   (incf num-date-comps)
		   (if (> num 31)
		       (setf (dt-yr res) num) ; clearly a year
		       (push num date-comps))
		   (when (and (< (1+ i) num-tok) (string= sep (elt tokens (1+ i))))
		     (incf i 2) 	; point to third component
		     (setf num (parse-number (elt tokens i)))
		     (when (null num)
		       (return-from string->date nil))
		     (incf num-date-comps)
		     (if (> num 31)
			 (if (dt-yr res)
			     ;; Jul/99/1985
			     (return-from string->date nil)
			     ;; unambiguous date
			     (setf (dt-yr res) num
				   (dt-day res) (pop date-comps)))
			 (if (dt-yr res)
			     (setf (dt-day res) num)
			     (push num date-comps))))))))
	    ;; default - skip token
	    (t (push token skipped-tokens))))
       end-of-do-loop)
      (when (> num-date-comps 3)
	(return-from string->date nil))
      (when (and (= num-date-comps 0) (dt-dow res)
		 (null relative-dow))
	;; day of week without date is relative to reference date
	(setf relative-dow :closest))
      (when date-comps
	(setf date-comps (nreverse date-comps)) ; order of appearance
	(let ((len (length date-comps)))
	  (cond ((or (= 3 num-date-comps len)
		     (= 2 num-date-comps len)) ; all uncertain
		 (multiple-value-bind (yy mm dd) (assign-yy-mm-dd date-comps precedence)
		   (setf (dt-yr res) yy (dt-mth res) mm (dt-day res) dd)))
		((and (= 3 num-date-comps) (= 2 len)) ; 2 of 3 are not certain
		 (cond ((dt-day res) (multiple-value-bind (yy mm) (assign-yy-mm date-comps precedence)
				       (setf (dt-yr res) yy (dt-mth res) mm)))
		       ((dt-mth res) (multiple-value-bind (yy dd) (assign-yy-dd date-comps precedence)
				       (setf (dt-yr res) yy (dt-day res) dd)))
		       ((dt-yr res) (multiple-value-bind (mm dd) (assign-mm-dd date-comps precedence)
				      (setf (dt-mth res) mm (dt-day res) dd)))))
		((and (= 3 num-date-comps) (= 1 len)) ; assign to null memeber in res
		 (when-null-set res dt-yr (car date-comps))
		 (when-null-set res dt-mth (car date-comps))
		 (when-null-set res dt-day (car date-comps)))
		((and (= 2 num-date-comps) (= 1 len))
		 (cond ((dt-yr res) ;; dd or mm
			(cond ((> (car date-comps) 12) (setf (dt-day res) (car date-comps)))
			      ((eq precedence :dmy) (setf (dt-day res) (car date-comps)))
			      (t (setf (dt-mth res) (car date-comps)))))
		       ((dt-mth res) ;; dd or yy
			(if (> (car date-comps) 31)
			    (setf (dt-yr res) (car date-comps))
			    (setf (dt-day res) (car date-comps))))
		       ((dt-day res) ;; mm or yy
			(if (> (car date-comps) 12)
			    (setf (dt-yr res) (car date-comps))
			    (setf (dt-mth res) (car date-comps))))))
		(t ;; i.e., num-date-comps = 1 so one of dd or mm or yy
		 (cond ((> (car date-comps) 31) (setf (dt-yr res) (car date-comps)))
		       ((<= (car date-comps) 12) (setf (dt-mth res) (car date-comps)))
		       (t ;; between 12 and 31
			(setf (dt-day res) (car date-comps))))))))
      ;; check that we could assign all date components
      (when (/= num-date-comps (+ (if (dt-yr res) 1 0) (if (dt-mth res) 1 0) (if (dt-day res) 1 0)))
	;; duplicate day / mth / year
	(return-from string->date nil))
      ;; handle two-digit years
      (when (dt-yr res)
	(setf (dt-yr res) (make-four-digit-year (dt-yr res))))
      ;; copy all missing values from the reference date
      (multiple-value-bind (yy mm dd h m s) (date->ymd reference-date :want-time t)
	(when-null-set res dt-yr yy)
	(when-null-set res dt-mth mm)
	(when-null-set res dt-day dd)
	(when-null-set res dt-hr h)
	(when-null-set res dt-min m)
	(when-null-set res dt-sec s))
      (unless (and (valid-date-p (dt-yr res) (dt-mth res) (dt-day res))
		   (valid-time-p (dt-hr res) (dt-min res) (dt-sec res)))
	(return-from string->date nil))
      (let ((dt (ymd->date (dt-yr res) (dt-mth res) (dt-day res)
			    (dt-hr res) (dt-min res) (dt-sec res))))
	(if (and relative-dow (dt-dow res))
	    (setf dt (advance-date dt (dt-dow res) relative-dow))
	    (when (dt-dow res)
	      (when (and (= 3 num-date-comps) (not (eq (dt-dow res) (day-of-week dt))))
		;; date was misparsed or day of week is not consistent with date
		(return-from string->date nil))))
	(when (dt-tz res)
	  ;; +ve offset = subtract, -ve offset = add
	  (decf dt (dt-tz res)))
	(values dt skipped-tokens)))))
