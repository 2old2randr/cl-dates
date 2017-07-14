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

(in-package :cl-dates-test)

;; Rounding errors in fractional seconds when changing timezones require
;; testing for approximate equality in some test cases
(defparameter +max-error+ 1/1000000)
(defun a= (a b)
  (< (abs (- a b)) +max-error+))

(deftest parse-dates ()
  (let ((dt (ymd->date 2003 9 25 1 36 28)))
    (check
      ;; Fully specified date with time and timezone
      (= (string->date "Thu Sep 25 10:36:28 JST 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (null (string->date "Wed Sep 25 10:36:28 JST 2003" :reference-date dt)) ; day of week doesn't match date
      (= (string->date "2003 10:36:28 JST 25 Sep Thu" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Thu, 25 Sep 2003 10:36:28 JST" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Thu, 25 Sep 2003 10:36:28 +0900" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003-09-25T10:49:41-03:00" :reference-date dt) (ymd->date 2003 9 25 13 49 41))
      (a= (string->date "2003-09-25T10:49:41.5-03:00" :reference-date dt) (ymd->date 2003 9 25 13 49 41.5))
      (= (string->date "20030925T10:49:41-03:00" :reference-date dt) (ymd->date 2003 9 25 13 49 41))
      (a= (string->date "20030925T10:49:41.5-03:00" :reference-date dt) (ymd->date 2003 9 25 13 49 41.5))

      ;; Partially specified dates (alphanumeric)
      (= (string->date "Thu Sep 25 10:36:28 2003" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "Thu Sep 25 10:36:28" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "Thu Sep 10:36:28" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "Thu 10:36:28" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "Sep 10:36:28" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "10:36:28" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "10:36" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "Thu Sep 25 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Sep 25 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Sep 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Sep" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003-09-25T10:49:41" :reference-date dt) (ymd->date 2003 9 25 10 49 41))
      (= (string->date "2003-09-25T10:49" :reference-date dt) (ymd->date 2003 9 25 10 49 28))
      (= (string->date "2003-09-25T10" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "2003-09-25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "20030925T104941" :reference-date dt) (ymd->date 2003 9 25 10 49 41))
      (= (string->date "20030925T1049" :reference-date dt) (ymd->date 2003 9 25 10 49 28))
      (= (string->date "20030925T10" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "20030925T10:49:41" :reference-date dt) (ymd->date 2003 9 25 10 49 41))
      (= (string->date "20030925T10:49" :reference-date dt) (ymd->date 2003 9 25 10 49 28))
      (= (string->date "20030925T10" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "20030925" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
     
      ;; Partially specified (numeric with possible alphabetic month name
      (= (string->date "19970902090807" :reference-date dt) (ymd->date 1997 9 2 9 8 7))
      (= (string->date "199709020908" :reference-date dt) (ymd->date 1997 9 2 9 8 28))
      (= (string->date "2003-Sep-25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "25-Sep-2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Sep-25-2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "09-25-2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28)) ; no ambiguity because 25 is a day
      (= (string->date "25-09-2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28)) ; no ambiguity because 25 is a day
      (= (string->date "10-09-2003" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 10 1 36 28)) ; explicitly dd/mm/yy
      (= (string->date "10-09-2003" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (= (string->date "10-09-03" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (= (string->date "10-09-03" :precedence :ymd :reference-date dt)
	 (ymd->date 2010 9 3 1 36 28)) ; explicitly yy/mm/dd
      (= (string->date "2003.09.25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003.Sep.25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "25.Sep.2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "09.25.2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "25.09.2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003/09/25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003/Sep/25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "25/Sep/2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "09/25/2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "25/09/2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "10.09.2003" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 10 1 36 28)) ; explicitly dd/mm/yy
      (= (string->date "10.09.2003" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (= (string->date "10.09.03" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 10 1 36 28)) ; explicitly dd/mm/yy
      (= (string->date "10.09.03" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (= (string->date "10/09/2003" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 10 1 36 28)) ; explicitly dd/mm/yy
      (= (string->date "10/09/2003" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (= (string->date "10/09/03" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 10 1 36 28)) ; explicitly dd/mm/yy
      (= (string->date "10/09/03" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (null (string->date "2003/25/Sep" :reference-date dt)) ; invalid - yy/dd/MMM
      (= (string->date "2003 09 25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003 Sep 25" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "2003 25 Sep" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "25 Sep 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Sep 25 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "09 25 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "25 09 2003" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "10 09 2003" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 10 1 36 28)) ; explicitly dd/mm/yy
      (= (string->date "10 09 2003" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (= (string->date "10 09 03" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 10 1 36 28)) ; explicitly dd/mm/yy
      (= (string->date "10 09 03" :precedence :mdy :reference-date dt)
	 (ymd->date 2003 10 9 1 36 28)) ; explicitly mm/dd/yy
      (= (string->date "03 25 Sep" :reference-date dt)
	 (ymd->date 2003 9 25 1 36 28)) ; default is yy/mm/dd
      (= (string->date "25 03 Sep" :precedence :dmy :reference-date dt)
	 (ymd->date 2003 9 25 1 36 28)) ; explicitly day before year
     
      ;; Assorted time formats
      (a= (string->date "10h16m38.5s" :reference-date dt) (ymd->date 2003 9 25 10 16 38.5))
      (= (string->date "10h16m38s" :reference-date dt) (ymd->date 2003 9 25 10 16 38))
      (a= (string->date "10h16m" :reference-date dt) (ymd->date 2003 9 25 10 16 28))
      (= (string->date "10h" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (a= (string->date "10 h 16" :reference-date dt) (ymd->date 2003 9 25 10 16 28))
      (= (string->date "10h am" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "10h pm" :reference-date dt) (ymd->date 2003 9 25 22 36 28))
      (= (string->date "10 am" :reference-date dt) (ymd->date 2003 9 25 10 36 28))
      (= (string->date "10 pm" :reference-date dt) (ymd->date 2003 9 25 22 36 28))
      (= (string->date "10:00 am" :reference-date dt) (ymd->date 2003 9 25 10 0 28))
      (= (string->date "10:00 pm" :reference-date dt) (ymd->date 2003 9 25 22 0 28))
      (= (string->date "10:00am" :reference-date dt) (ymd->date 2003 9 25 10 0 28))
      (= (string->date "10:00pm" :reference-date dt) (ymd->date 2003 9 25 22 0 28))
      (= (string->date "10:00a.m." :reference-date dt) (ymd->date 2003 9 25 10 0 28))
      (= (string->date "10:00p.m." :reference-date dt) (ymd->date 2003 9 25 22 0 28))
      (= (string->date "10:00a.m" :reference-date dt) (ymd->date 2003 9 25 10 0 28))
      (= (string->date "10:00p.m" :reference-date dt) (ymd->date 2003 9 25 22 0 28))

      ;; relative dates etc
      (= (string->date "Sep 03" :reference-date dt) (ymd->date 2003 9 3 1 36 28))
      (= (string->date "Sep of 03" :reference-date dt) (ymd->date 2003 9 25 1 36 28))
      (= (string->date "Wed" :reference-date dt) (ymd->date 2003 9 24 1 36 28))
      (= (string->date "Wednesday" :reference-date dt) (ymd->date 2003 9 24 1 36 28))
      (= (string->date "last Wednesday" :reference-date dt) (ymd->date 2003 9 24 1 36 28))
      (= (string->date "next Wednesday" :reference-date dt) (ymd->date 2003 10 1 1 36 28))
      (= (string->date "October" :reference-date dt) (ymd->date 2003 10 25 1 36 28))
      (= (string->date "31-Dec-00" :precedence :dmy :reference-date dt)
	 (ymd->date 2000 12 31 1 36 28))

      ;; Random verbiage / unusual formats
      (= (string->date "The date is the 25th of September of 2003, exactly at 10:49:41 with time zone -03:00"
		       :reference-date dt) (ymd->date 2003 9 25 13 49 41))
      (null (string->date "Today is the 25th of September of 2003, exactly at 10:49:41 with time zone -03:00"
			  :reference-date dt)) ; 'today' is assumed to be a date spec so a duplicate date is detected
      (= (string->date "   July    4 , 1976   12:01:02    am   " :reference-date dt)
	 (ymd->date 1976 7 4 0 1 2))
      (= (string->date "today 12:35" :reference-date dt) (ymd->date 2003 9 25 12 35 28))
      (= (string->date "today 12:35 JST" :reference-date dt) (ymd->date 2003 9 25 3 35 28))
      (= (string->date "tomorrow 12:35" :reference-date dt) (ymd->date 2003 9 26 12 35 28))
      (= (string->date "yesterday 12:35 am JST" :reference-date dt) (ymd->date 2003 9 23 15 35 28)) ; date is 2 days prior in UTC
      (= (string->date "Wed Jul 10, '96" :reference-date dt) (ymd->date 1996 7 10 1 36 28))
      (a= (string->date "1996.07.10 AD at 15:08:56 PDT" :reference-date dt) (ymd->date 1996 7 10 22 8 56))
      (= (string->date "1996.07.10 AD at 12:08 PM" :reference-date dt) (ymd->date 1996 7 10 12 8 28))
      (= (string->date "Saturday, April 12, 1952 AD 3:30:42pm PST" :reference-date dt) (ymd->date 1952 4 12 23 30 42))
      (= (string->date "November 5, 1994, 8:15:30 am EST" :reference-date dt) (ymd->date 1994 11 5 13 15 30))
      (= (string->date "1994-11-05T08:15:30-05:00" :reference-date dt) (ymd->date 1994 11 5 13 15 30))
      (= (string->date "1994-11-05T08:15:30Z" :reference-date dt) (ymd->date 1994 11 5 8 15 30))
      (= (string->date "0:01:02" :reference-date dt) (ymd->date 2003 9 25 0 1 2))
      (= (string->date "12h 01m02s am" :reference-date dt) (ymd->date 2003 9 25 0 1 2))
      (= (string->date "0:01:02 on July 4, 1976" :reference-date dt) (ymd->date 1976 7 4 0 1 2))
      (null (string->date "July 4, 1976 pm 12:01:02" :reference-date dt)) ; am/pm must come after time spec
      (= (string->date "July 4, 4pm 4:01:02" :reference-date dt)
	 (ymd->date 2003 7 4 4 1 2)) ; 4pm overridden by 4:01:02
      (= (string->date "July 4, 4pm" :reference-date dt) (ymd->date 2003 7 4 16 36 28)) ; this is ok
      (a= (string->date "04.04.95 00:22" :reference-date dt) (ymd->date 1995 4 4 0 22 28))
      (= (string->date "950404 122212" :reference-date dt) (ymd->date 1995 4 4 12 22 12))
      (= (string->date "0:00 PM, PST" :reference-date dt) (ymd->date 2003 9 25 20 0 28))
      (= (string->date "12:08 PM" :reference-date dt) (ymd->date 2003 9 25 12 8 28))
      (= (string->date "5:50 A.M on June 13, 1990" :reference-date dt) (ymd->date 1990 6 13 5 50 28))
      (= (string->date "01h02m03" :reference-date dt) (ymd->date 2003 9 25 1 2 3))
      (= (string->date "01h02" :reference-date dt) (ymd->date 2003 9 25 1 2 28))
      (= (string->date "01h02s" :reference-date dt) (ymd->date 2003 9 25 1 36 2))
      (= (string->date "01m02" :reference-date dt) (ymd->date 2003 9 25 1 1 2))
      (a= (string->date "01m02h" :reference-date dt) (ymd->date 2003 9 25 2 1 28))
      (= (string->date "2004 10 April 11h30m" :reference-date dt) (ymd->date 2004 4 10 11 30 28))
      ;; The next one works coincidentally because Japanese uses a y/m/d format by default.
      ;; However, it demonstrates that as long as date components are present, the presence
      ;; of extraneous characters does not matter
      (= (string->date "2004年8月9日") (ymd->date 2004 8 9)))))
