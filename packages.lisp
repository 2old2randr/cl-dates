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

(in-package :cl-user)

(defpackage :cl-dates
  (:nicknames :dt)
  (:use :common-lisp)
  (:export
   ;; Make a new date
   :ymd->date				; date from y/m/d/h/m/s components
   :string->date			; date from string
   :todays-date				; system date
   :todays-datetime			; system date and time
   ;; Date converters
   :date->ymd				; to components
   :date->string			; to string
   :date->long-string			; to string (verbose)
   :date->javascript-time		; to JS datetime
   :date->local-time			; to local time zone
   :month->string			; full name of month
   :dow->string				; day of week as string
   :day-count->string			; day-count convention as string
   :eom-rule->string			; end of month rule as string
   ;; Special dates for given year
   :easter-day				; easter day
   :vernal-equinox			; spring equinox date-time
   :summer-solstice			; summer solstice date-time
   :autumnal-equinox			; autumn equinox date-time
   :winter-solstice			; winter solstice date-time
   ;; Miscellaneous functions
   :valid-date-p
   :valid-time-p
   :jday-number				; Julian day number
   :day-of-week				; Day of week for date
   :leap-year-p
   ;; Comparisons
   :date= :date/=
   :date< :date<=
   :date> :date>=
   ;; Calendar date arithmetic
   :nth-day-of-week	    ; 'n' weeks offset from start of month
   :first-of-next-month	    ; first day of next month
   :last-day-of-month	    ; last calendar day in month
   :last-day-of-prev-month  ; last calendar day in previous month
   :date+ :date-	    ; add/subtract days to date
   :add-months		    ; add/subtract months to date
   :add-years		    ; convenience function
   :diff-days		    ; absolute number of days between two dates
   :diff-years		    ; years between dates using day count conventions
   ))
