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

(deftest julian ()
  (format t "Julian date conversions ...~%")
  (check
   (= (ymd->date 2017 2 5) 2457789.5)
   (= (ymd->date 1959 12 29) 2436931.5)
   (= (ymd->date 1959 12 29 18 12 26.3) 2436932.2586376667d0)
   (equal '(2017 2 5 0 0 0.0d0)
	  (multiple-value-call #'list (date->ymd 2457789.5)))
   (= (ymd->date 1600 2 29) 2305506.5)
   (= (ymd->date 1600 2 29 9 31 23.5) 2305506.896799773d0)
   (equal '(1600 2 29 9 31 23.500385284423828d0)
	  (multiple-value-call #'list (date->ymd 2305506.896799773d0)))
   (= (date->javascript-time (ymd->date 2017 6 18)) 1497744000000)
   (= (jday-number (ymd->date 2017 6 18)) 2457923)))

(deftest misc-fns ()
  (format t "Miscellaneous date functions ...~%")
  (check
   (valid-date-p 2000 2 29)
   (null (valid-date-p 1900 2 29))
   (null (valid-date-p 1234 65 789))
   
   (eq :monday (day-of-week (ymd->date 2017 2 6)))
   (eq :wednesday (day-of-week (ymd->date 1959 9 23)))

   (= (nth-day-of-week (ymd->date 2017 2 5) :tuesday 3) (ymd->date 2017 2 21))
   (= (nth-day-of-week (ymd->date 2017 2 5) :thursday 3) (ymd->date 2017 2 16))
   ;; overflow month - returns last tuesday
   (= (nth-day-of-week (ymd->date 2017 2 5) :tuesday 7) (ymd->date 2017 2 28))
   ;; any number <= 1 should return 1st tuesday
   (= (nth-day-of-week (ymd->date 2017 2 5) :tuesday -7) (ymd->date 2017 2 7))))

(deftest print-fns ()
  (format t "Conversions to strings ...~%")
  (let ((dt1 (ymd->date 2017 2 16))
	(dt2 (ymd->date 2017 2 16 17 30 25 +9)))
    (check
     (string= (dow->string :monday) "Monday")
     (string= (dow->string :tuesday) "Tuesday")
     (string= (dow->string :wednesday) "Wednesday")
     (string= (dow->string :thursday) "Thursday")
     (string= (dow->string :friday) "Friday")
     (string= (dow->string :saturday) "Saturday")
     (string= (dow->string :sunday) "Sunday")

     (string= (month->string 1) "January")
     (string= (month->string 2) "February")
     (string= (month->string 3) "March")
     (string= (month->string 4) "April")
     (string= (month->string 5) "May")
     (string= (month->string 6) "June")
     (string= (month->string 7) "July")
     (string= (month->string 8) "August")
     (string= (month->string 9) "September")
     (string= (month->string 10) "October")
     (string= (month->string 11) "November")
     (string= (month->string 12) "December")

     (string= (date->string dt1) "2017-02-16")
     (string= (date->string dt1 :format :iso-8601) "2017-02-16")
     (string= (date->string dt1 :format :asctime) "Thu Feb 16 2017")
     (string= (date->string dt1 :format :rfc-822) "Thu, 16 Feb 2017")
     (string= (date->string dt1 :format :rfc-850) "Thu, 16-Feb-2017")
     
     (string= (date->string dt2) "2017-02-16 08:30:25 UTC")
     (string= (date->string dt2 :format :iso-8601) "2017-02-16T08:30:25Z")
     (string= (date->string dt2 :format :asctime) "Thu Feb 16 08:30:25 GMT 2017")
     (string= (date->string dt2 :format :rfc-822) "Thu, 16 Feb 2017 08:30:25 GMT")
     (string= (date->string dt2 :format :rfc-850) "Thu, 16-Feb-2017 08:30:25 GMT")

     (string= (date->string dt2 :zone "JST") "2017-02-16 17:30:25 +09:00")
     (string= (date->string dt2 :zone "JST" :format :iso-8601) "2017-02-16T17:30:25+09:00")
     (string= (date->string dt2 :zone "JST" :format :asctime) "Thu Feb 16 17:30:25 +0900 2017")
     (string= (date->string dt2 :zone "JST" :format :rfc-822) "Thu, 16 Feb 2017 17:30:25 +0900")
     (string= (date->string dt2 :zone "JST" :format :rfc-850) "Thu, 16-Feb-2017 17:30:25 +0900"))))

(deftest special-dates ()
  (format t "Computation of special dates ...~%")
  (check
   (= (easter-day 2001) (ymd->date 2001 4 15))
   (= (easter-day 2002) (ymd->date 2002 3 31))
   (= (easter-day 2005) (ymd->date 2005 3 27))
   (= (easter-day 2011) (ymd->date 2011 4 24))

   (= (vernal-equinox 2017)
      (ymd->date 2017 3 20 10 28 32.05221712589264d0))
   (= (summer-solstice 2017)
      (ymd->date 2017 6 21 4 23 43.49940687417984d0))
   (= (autumnal-equinox 2017)
      (ymd->date 2017 9 22 20 1 8.430179357528687D0))
   (= (winter-solstice 2017)
      (ymd->date 2017 12 21 16 27 51.39586955308914d0))
   (= (vernal-equinox 1959)
      (ymd->date 1959 3 21 8 55 7.991203665733337d0))
   (= (summer-solstice 1959)
      (ymd->date 1959 6 22 3 49 50.55352360010147d0))
   (= (autumnal-equinox 1959)
      (ymd->date 1959 9 23 19 8 29.363870322704315D0))
   (= (winter-solstice 1959)
      (ymd->date 1959 12 22 14 34 33.68946969509125d0))))
