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

(in-package :lee-stmx)


(defstruct (lee-args (:constructor lee-args))
  (input-file  ""  :type string)
  (output-file ""  :type string)
  (threads     1   :type fixnum)
  (validate    nil :type boolean))


(defstruct (thread-args (:constructor thread-args))
  (lee       0 :type lee)
  (temp-grid 0 :type temp-grid)
  (commits   0 :type fixnum)
  (retries   0 :type fixnum)
  (fails     0 :type fixnum))


(declaim (type thread-barrier *start-barrier* *end-barrier*))
(defvar *start-barrier* (thread-barrier 1))
(defvar *end-barrier*   (thread-barrier 1))




(defun run-transactions (thread-args)
  (declare (type thread-args thread-args))

  (let ((lee (thread-args-lee thread-args))
        (temp-grid (thread-args-temp-grid thread-args))
        (commits  0)
        (retries  0)
        (fails    0))

    (loop for q = (atomic-pop (lee-work lee))
       while q do
         (multiple-value-bind (success transactions)
             (lee-connect lee q temp-grid)

           (log:trace "track ~4d ~a, transactions = ~d"
                      (work-netno q) (if success "committed" "   failed") transactions)

           (if success
               (incf commits)
               (incf fails))

           (unless (zerop transactions)
             (incf retries (1- transactions)))))

    (log:debug "thread commits = ~d, failed = ~d, retried = ~a" commits fails retries)

    (setf (thread-args-commits thread-args) commits
          (thread-args-fails   thread-args) fails
          (thread-args-retries thread-args) retries)
    nil))


(defun timed-run-transactions (thread-args)
  (declare (type thread-args thread-args))

  ;; wait for all threads to come to this point
  (join-thread-barrier *start-barrier*)

  (let1 start-time (get-internal-real-time)

    ;; perform work
    (run-transactions thread-args)

    ;; wait for all threads to finish
    (join-thread-barrier *end-barrier*)

    (let1 end-time (get-internal-real-time)
      (/ (- end-time start-time) (float internal-time-units-per-second)))))


(defun run-benchmark (lee-args)
  (declare (type lee-args lee-args))

  (let* ((n (lee-args-threads lee-args))
         (input-file (lee-args-input-file lee-args))
         (input-file? (not (zerop (length input-file))))
         (lee (if input-file? (lee) (lee :grid-size 10))) ;; default grid size is 600
         (grid (lee-grid lee))
         ;; initialize thread arguments
         (thread-args (loop for i from 1 to n
                         collect (thread-args :lee lee :temp-grid (temp-grid grid)))))

    (setf (thread-barrier-count *start-barrier*) n
          (thread-barrier-count *end-barrier*)   n)
          
    (if input-file?
        (parse-lee-file lee (lee-args-input-file lee-args))
        (fake-test-data lee))
    
    (add-weights lee)

    (sort-lee-work lee)

    ;; now start all threads
    (loop for thread-arg in (rest thread-args)
       for thread-id from 2
       do
         (let1 arg thread-arg
           (bt:make-thread (lambda () (timed-run-transactions arg))
                           :name (format nil "lee ~d" thread-id))))

    (let  ;; run one instance in this thread
        ((duration (timed-run-transactions (first thread-args)))
         (commits  0)
         (retries  0)
         (fails    0))

      ;; print statistics
      (log:info "Duration ~a seconds" duration)

      (dolist (arg thread-args)
        (incf commits (thread-args-commits arg))
        (incf retries (thread-args-retries arg))
        (incf fails   (thread-args-fails   arg)))

      (log:info "Total commits ~d" commits)
      (log:info "Total failed  ~d" fails)
      (log:info "Total retried ~d" retries)
      (unless (zerop duration)
        (log:info "Total transactions per second ~a" (/ (+ commits fails) duration))
        (log:info "Total retried      per second ~a" (/ retries duration))))

    (let1 output-file (lee-args-output-file lee-args)
      (unless (zerop (length output-file))
        (with-open-file (stream output-file :direction :output :if-exists :supersede)
          (show-grid (lee-grid lee) stream))))))




(defun parse-arguments (args-list)
  "Parse ARGS and return a corresponding LEE-ARGS struct."
  (declare (type list args-list))

  (let1 args (lee-args)
    (loop for cell = args-list then rest
       while cell
       for arg = (first cell)
       for rest = (rest cell)
       do
         (cond
           ((or (eq arg :input-file)
                (eq arg 'input-file)
                (equal arg "input-file"))
            (setf (lee-args-input-file args) (the string (pop rest))))

           ((or (eq arg :output-file)
                (eq arg 'output-file)
                (equal arg "output-file"))
            (setf (lee-args-output-file args) (the string (pop rest))))

           ((or (eq arg :threads)
                (eq arg 'threads)
                (equal arg "threads"))
            (let1 n (pop rest)
              (setf (lee-args-threads args)
                    (etypecase n
                      (fixnum n)
                      (string (the fixnum (parse-integer n :junk-allowed t)))))))
           
           ((or (eq arg :validate)
                (eq arg 'validate)
                (equal arg "validate"))
            (let1 flag (pop rest)
              (setf (lee-args-validate args)
                    (or (eq  t flag)
                        (eql 1 flag)
                        (equal "on" flag)
                        (equal "yes" flag)
                        (equal "true" flag)))))

           ((or (eq arg :help)
                (eq arg 'help)
                (equal arg "help"))

            (format t "usage: (lee-main '([:input-file \"input-file-name\"] [:output-file \"output-file-name\"] [:threads n] [:validate boolean]))")
            (return-from parse-arguments nil))

           (t
            (error "unknown argument ~s, use \"help\" for help" arg))))
    args))



(defun cmd-line-main (args-list)
  "Parse arguments, then run Lee-STMX benchmark"
  (declare (type list args-list))

  (let1 args (parse-arguments args-list)
    (log:info "Lee-STMX benchmark arguments:")
    (log:info "~s" args)
    (run-benchmark args)))


(defun main (&key (input-file "") (output-file "") (threads 1) (validate nil))
  "Parse arguments, then run Lee-STMX benchmark"
  (declare (type string input-file)
           (type fixnum threads)
           (type boolean validate))
  (cmd-line-main (list :input-file input-file :output-file output-file
                       :threads threads :validate validate)))