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
  (:use :cl :asdf))

(in-package :cl-dates)

(defsystem :cl-dates
  :version "0.7"
  :description "Date-time library for Common Lisp"
  :author "Sudhir Shenoy"
  :license "BSD"
  :serial t
  :components ((:file "packages")
	       (:file "util")
	       (:file "timezones")
               (:file "dates")
               (:file "parse-date")
	       (:file "print-date")
	       (:file "date-arith")))

(defsystem :cl-dates-test
  :description "Date-time library tests"
  :author "Sudhir Shenoy"
  :license "BSD"
  :depends-on (:cl-dates)
  :serial t
  :components ((:file "test-main")
	       (:file "test-dates")
	       (:file "test-parse-date")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-dates))))
  (operate 'load-op :cl-dates-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :cl-dates-test))))
