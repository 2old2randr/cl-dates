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

(defpackage :cl-dates-test
  (:use :common-lisp :cl-dates)
  (:export :run-all-tests))

(in-package :cl-dates-test)

;;;
;;; Peter Siebel's test framework from Practical Common Lisp with
;;; slight modifications
;;;
(defvar *test-name* nil)
(defvar *verbose-results* nil)
(defvar *total-tests* 0)
(defvar *failed-tests* 0)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (incf *total-tests*)
  (when (not result)
    (incf *failed-tests*))
  (if *verbose-results*
      (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
      (when (not result)
	(format t "FAIL ... ~a: ~a~%" *test-name* form)))
  result)

(defun run-all-tests (&key (verbose nil))
  (let ((*total-tests* 0)
	(*failed-tests* 0)
	(*verbose-results* verbose)
	(status (combine-results
		 (julian)
		 (misc-fns)
		 (special-dates)
		 (print-fns)
		 (parse-dates))))
    (unless (zerop *total-tests*)
      (format t "~d tests executed - ~d passed (~,2f%)~%"
	      *total-tests* (- *total-tests* *failed-tests*)
	      (/ (* 100 (- *total-tests* *failed-tests*)) *total-tests*)))
    status))
