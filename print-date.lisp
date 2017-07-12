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

(defun date->string (date &key (format :human) zone)
  "Return a string representation of a datetime as per the desired format.
Format must be specified as one of (:human :iso-8601 :asctime :rfc-822 :rfc-850).

The time is omitted if it is exactly midnight or noon

If timezone is not null, the datetime is first translated to the given timezone
from UTC and then converted to a string. Timezone can be specified as an alphabetic
code or a numeric offset in fractions of hours.

The :human format is the same as :iso-8601 except that the separators between date,
time and timezone are spaces to make it more readable to the human eye."
  (let ((offset (zone-to-offset zone)))
    (incf date (/ offset 24))
    (multiple-value-bind (yy mm dd h m s) (date->ymd date :want-time t)
      (let ((year (format nil "~d" yy))
	    (month (case format
		     ((:asctime :rfc-822 :rfc-850) (three-letter-month mm))
		     (otherwise (format nil "~2,'0d" mm))))
	    (day (format nil "~2,'0d" dd))
	    (dow (case format
		   ((:asctime :rfc-822 :rfc-850) (three-letter-dow (day-of-week date)))
		   (otherwise ""))))
	(if (or (integerp date)
		(= 1/2 (mod date 1)))
	    (date-only-string year month day dow format)
	    (date-time-string year month day dow h m s offset format))))))

(defun date->long-string (date &key zone (date-only nil))
  "Converts a date to a long string with day of week and month fully spelled out
e.g., \"Thursday, 6 July 2017, 09:38:43.567 +0900\".

If :date-only is true, the time and timezone are omitted."
  (let ((offset (zone-to-offset zone)))
    (incf date (/ offset 24))
    (multiple-value-bind (yy mm dd h m s) (date->ymd date :want-time t)
      (if date-only
	  (format nil "~a, ~d ~a ~d" (dow->string (day-of-week date))
		  dd (month->string mm) yy)
	  (let* ((tz-hh (truncate (abs offset)))
		 (tz-mm (truncate (* 60 (- (abs offset) tz-hh))))
		 (tz-str (cond ((= 0 offset) "UTC")
			       ((< offset 0) (format nil "-~2,'0d~2,'0d" tz-hh tz-mm))
			       (t (format nil "+~2,'0d~2,'0d" tz-hh tz-mm))))
		 (time-str (if (< (- s (truncate s)) 0.001)
			       (format nil "~2,'0d:~2,'0d:~2,'0d" h m (truncate s))
			       (format nil "~2,'0d:~2,'0d:~6,3,,,'0f" h m s))))
	    (format nil "~a, ~d ~a ~d, ~a ~a" (dow->string (day-of-week date))
		    dd (month->string mm) yy time-str tz-str))))))

(defun date-only-string (yy mm dd dow fmt)
  (case fmt
    (:rfc-822 (format nil "~a, ~a ~a ~a" dow dd mm yy))
    (:asctime (format nil "~a ~a ~a ~a" dow mm dd yy))
    (:rfc-850 (format nil "~a, ~a-~a-~a" dow dd mm yy))
    (t (format nil "~a-~a-~a" yy mm dd))))

(defun date-time-string (yy mm dd dow h m s offset fmt)
  (let* ((tz-hh (truncate (abs offset)))
	 (tz-mm (truncate (* 60 (- (abs offset) tz-hh))))
	 (tz-str (case fmt
		   ((:rfc-822 :rfc-850 :asctime)
		    (cond ((= 0 offset) "GMT")
			  ((< offset 0) (format nil "-~2,'0d~2,'0d" tz-hh tz-mm))
			  (t (format nil "+~2,'0d~2,'0d" tz-hh tz-mm))))
		   (t (cond ((= 0 offset) (if (eq fmt :iso-8601) "Z" "UTC"))
			    ((< offset 0) (format nil "-~2,'0d:~2,'0d" tz-hh tz-mm))
			    (t (format nil "+~2,'0d:~2,'0d" tz-hh tz-mm))))))
	 (time-str (if (< (- s (truncate s)) 0.001)
		       (format nil "~2,'0d:~2,'0d:~2,'0d" h m (truncate s))
		       (format nil "~2,'0d:~2,'0d:~6,3,,,'0f" h m s))))
    (case fmt
      (:rfc-822 (format nil "~a, ~a ~a ~a ~a ~a" dow dd mm yy time-str tz-str))
      (:asctime (format nil "~a ~a ~a ~a ~a ~a" dow mm dd time-str tz-str yy))
      (:rfc-850 (format nil "~a, ~a-~a-~a ~a ~a" dow dd mm yy time-str tz-str))
      (:iso-8601 (format nil "~a-~a-~aT~a~a" yy mm dd time-str tz-str))
      (t (format nil "~a-~a-~a ~a ~a" yy mm dd time-str tz-str)))))


      
