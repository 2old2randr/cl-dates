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

(deftest holiday-tests ()
  (combine-results
   (loop for test-case in +holiday-tests+
	 collect (destructuring-bind (centre year-range dates) test-case
		   (setf dates (mapcar #'jday-number (mapcar #'string->date dates)))
		   (let* ((from-year (car year-range))
			  (start-range (ymd->date from-year 1 1))
			  (end-range (ymd->date (cadr year-range) 12 31))
			  (cal (make-calendar centre :base-year from-year))
			  (hols (sort (loop for dt being the
					    hash-keys in (cl-dates::holidays cal)
					    when (date<= start-range dt end-range)
					      collect dt)
				      #'date<)))
		     (check
		       (equal hols dates)))))))

(defparameter +holiday-tests+
  '(;; US settlement holidays
    (:usd (2004 2005) ("1 January 2004" "19 January 2004" "16 February 2004" "31 May 2004"
		       "5 July 2004" "6 September 2004" "11 October 2004" "11 November 2004"
		       "25 November 2004" "24 December 2004" "31 December 2004"
		       "17 January 2005" "21 February 2005" "30 May 2005" "4 July 2005"
		       "5 September 2005" "10 October 2005" "11 November 2005"
		       "24 November 2005" "26 December 2005"))
    ;; US settlement - before Uniform Monday Holiday Act
    (:usd (1961 1961) ("2 January 1961" "22 February 1961" "30 May 1961" "4 July 1961"
		       "4 September 1961" "10 November 1961" "23 November 1961"
		       "25 December 1961"))
    ;; US Bond market
    (:ust (2004 2004) ("1 January 2004" "19 January 2004" "16 February 2004" "9 April 2004"
		       "31 May 2004" "5 July 2004" "6 September 2004" "11 October 2004"
		       "11 November 2004" "25 November 2004" "24 December 2004"))
    ;; NY Stock exchange
    (:nyse (2004 2006) ("1 January 2004" "19 January 2004" "16 February 2004" "9 April 2004"
			"31 May 2004" "11 June 2004" "5 July 2004" "6 September 2004"
			"25 November 2004" "24 December 2004"
			"17 January 2005" "21 February 2005" "25 March 2005" "30 May 2005"
			"4 July 2005" "5 September 2005" "24 November 2005" "26 December 2005"
			"2 January 2006" "16 January 2006" "20 February 2006" "14 April 2006"
			"29 May 2006" "4 July 2006" "4 September 2006" "23 November 2006"
			"25 December 2006"))
    ;; TARGET
    (:target (1999 2002) ("1 January 1999" "31 December 1999"
			  "21 April 2000" "24 April 2000" "1 May 2000" "25 December 2000"
			  "26 December 2000"
			  "1 January 2001" "13 April 2001" "16 April 2001" "1 May 2001"
			  "25 December 2001" "26 December 2001" "31 December 2001"
			  "1 January 2002" "29 March 2002" "1 April 2002" "1 May 2002"
			  "25 December 2002" "26 December 2002"))
    (:target (2003 2006) ("1 January 2003" "18 April 2003" "21 April 2003" "1 May 2003"
			  "25 December 2003" "26 December 2003"
			  "1 January 2004" "9 April 2004" "12 April 2004"
			  "25 March 2005" "28 March 2005" "26 December 2005"
			  "14 April 2006" "17 April 2006" "1 May 2006" "25 December 2006"
			  "26 December 2006"))
    ;; EUREX
    (:eurex (2003 2004) ("1 January 2003" "18 April 2003" "21 April 2003" "1 May 2003"
			 "24 December 2003" "25 December 2003" "26 December 2003" "31 December 2003"
			 "1 January 2004" "9 April 2004" "12 April 2004" "24 December 2004"
			 "31 December 2004"))
    ;; UK
    (:gbp (2004 2007) ("1 January 2004" "9 April 2004" "12 April 2004" "3 May 2004" "31 May 2004"
		       "30 August 2004" "27 December 2004" "28 December 2004"
		       "3 January 2005" "25 March 2005" "28 March 2005" "2 May 2005" "30 May 2005"
		       "29 August 2005" "26 December 2005" "27 December 2005"
		       "2 January 2006" "14 April 2006" "17 April 2006" "1 May 2006" "29 May 2006"
		       "28 August 2006" "25 December 2006" "26 December 2006"
		       "1 January 2007" "6 April 2007" "9 April 2007" "7 May 2007" "28 May 2007"
		       "27 August 2007" "25 December 2007" "26 December 2007"))))
