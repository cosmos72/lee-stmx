;; BSD License
;;
;; Copyright (c) 2007, The University of Manchester (UK)
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;     - Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     - Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;     - Neither the name of the University of Manchester nor the names
;;       of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written
;;       permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;;; * lee-stmx

#+lee-stmx (in-package :lee-stmx)
#-lee-stmx (in-package :lee-cg)


(declaim (inline %make-thread-barrier))

(defstruct (thread-barrier (:constructor %make-thread-barrier))
  (lock        (bt:make-lock "thread-barrier") :read-only t)
  (conditions  nil :type list)
  (count       1   :type fixnum))


(defun thread-barrier (thread-count)
  "Create and return a THREAD-BARRIER that THREAD-COUNT threads can use to wait on."
  (declare (type fixnum thread-count))
  (%make-thread-barrier
   :count thread-count))



(defun join-thread-barrier (thread-barrier)
  "Block until THREAD-COUNT threads invoke this function on the same THREAD-BARRIER."
  (declare (type thread-barrier thread-barrier))
  (let1 lock (thread-barrier-lock thread-barrier)
    (bt:acquire-lock lock)
    (unwind-protect
         (if (zerop (the fixnum (decf (thread-barrier-count thread-barrier))))
             (progn
               (loop for cond in (thread-barrier-conditions thread-barrier) do
                    (bt:condition-notify cond))
               (setf (thread-barrier-conditions thread-barrier) nil))
             (let1 cond (bt:make-condition-variable :name "thread-barrier condition")
               (push cond (thread-barrier-conditions thread-barrier))
               (sb-thread:condition-wait cond lock)))
      (bt:release-lock lock))))
