;;
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
;;

;;;; * lee-stmx

(in-package :lee-stmx)

(declaim (type fixnum +empty+ +occ+ +via+ +bvia+ +track+ +temp-empty+))
(defconstant +empty+  0)
(defconstant +occ+   -1)
(defconstant +via+   -2)
(defconstant +bvia+  -3)
(defconstant +track+ -5)
(defconstant +temp-empty+ most-positive-fixnum)

;;;; ** frontier

(declaim (inline %make-frontier))

(defstruct (frontier (:constructor %make-frontier))
  (x  0 :type fixnum)
  (y  0 :type fixnum)
  (z  0 :type fixnum)
  (dw 0 :type fixnum))


(defun frontier (x y z dw)
  (declare (type fixnum x y z dw))
  (%make-frontier :x x :y y :z z :dw dw))

;;;; ** free frontier pool

(declaim (type list *frontier-pool* *cons-pool*))
(defvar *frontier-pool* nil)
(defvar *cons-pool* nil)

(eval-when (:load-toplevel :execute)
  (dolist (sym '(*frontier-pool* *cons-pool*))
    (unless (assoc sym bt:*default-special-bindings* :test 'eq)
      (push (cons sym nil) bt:*default-special-bindings*))))

(declaim (inline cons-from-pool
                 cons-to-pool
                 cons-and-frontier-from-pool
                 cons-and-frontier-to-pool
                 frontier-to-pool))

(defun cons-from-pool ()
  "Get a CONS from free cons pool if available, and return it."
  (when-bind cell *cons-pool*
    (setf *cons-pool* (rest cell)
          (rest cell) nil)
    cell))

(defun new-cons (&optional a b)
  "Get a CONS from free cons pool, otherwise allocate it. Return the CONS."
  (if-bind cell (cons-from-pool)
    (progn
      (setf (first cell) a
            (rest  cell) b)
      cell)
    (cons a b)))

(defun cons-to-pool (cell)
  "Add a CONS cell to free cons pool.
Note: (first cell) should be NIL."
  (declare (type cons cell))
  (setf (rest cell) *cons-pool*
        *cons-pool* cell)
  nil)


(defun cons-and-frontier-from-pool ()
  "Get a CONS and a FRONTIER from pool if available, and return them."
  (block nil
    (when-bind cell *frontier-pool*
      (setf *frontier-pool* (rest cell)
            (rest cell) nil)
      (return cell))
    (awhen (cons-from-pool)
      (return it))))


(defun cons-and-frontier-to-pool (cell)
  "Add (first cell) - which must be a FRONTIER - to the free frontier pool."
  (declare (type cons cell))
  (setf (rest cell) *frontier-pool*
        *frontier-pool* cell)
  nil)


(defun frontier-fifo-to-pool (head tail)
  "Add a list of FRONTIERs to the free frontier pool.
TAIL must be the last CONS cell of the list starting at HEAD."
  (declare (type cons head tail))
  (setf (rest tail) *frontier-pool*
        *frontier-pool* head)
  nil)


(defun frontier-to-pool (f)
  "Add frontier F to the free frontier pool."
  (declare (type frontier f))
  (cons-and-frontier-to-pool (new-cons f)))



;;;; ** frontier fifo

(declaim (inline %make-frontier-fifo))

(defstruct (frontier-fifo (:constructor %make-frontier-fifo))
  (head nil :type cons)
  (tail nil :type cons))

(defun frontier-fifo ()
  (let1 cell (new-cons)
    (%make-frontier-fifo :head cell :tail cell)))


(declaim (inline frontier-fifo-empty?))
(defun frontier-fifo-empty? (fifo)
  (declare (type frontier-fifo fifo))
  (eq (frontier-fifo-head fifo)
      (frontier-fifo-tail fifo)))


(defun push-new-frontier (fifo x y z dw)
  "Create and append a new FRONTIER to FIFO."
  (declare (type frontier-fifo fifo)
           (type fixnum x y z dw))
  (let* ((cell (cons-and-frontier-from-pool))
         (f    (if cell (first cell) nil)))

    (if cell
        (setf (first cell) nil)
        (setf cell (cons nil nil)))

    (if f
        (setf (frontier-x  f) x
              (frontier-y  f) y
              (frontier-z  f) z
              (frontier-dw f) dw)
        (setf f (frontier x y z dw)))


    (let1 tail (frontier-fifo-tail fifo)
      (setf (first tail) f
            (rest  tail) cell
            (frontier-fifo-tail fifo) cell))
    (the frontier f)))


(defun pop-frontier (fifo)
  "Remove first FRONTIER from FIFO and return it.
Return NIL if FIFO is empty."
  (declare (type frontier-fifo fifo))
  (let ((cell (frontier-fifo-head fifo))
        (tail (frontier-fifo-tail fifo)))
    (if (eq cell tail)
        nil
        (let1 f (first cell)
          (setf (frontier-fifo-head fifo) (rest cell)
                (first cell) nil)
          (cons-to-pool cell)
          (the frontier f)))))

(defun clear-frontier-fifo (fifo)
  "Remove all FRONTIERs from FIFO and add them to the free frontier pool."
  (let ((head (frontier-fifo-head fifo))
        (tail (frontier-fifo-tail fifo)))
    (unless (eq head tail)
      (rotatef (first head) (first tail))
      (let1 next (rest head)
        (frontier-fifo-to-pool next tail))
      (setf (rest head) nil
            (frontier-fifo-tail fifo) head))
    fifo))
        
        
           



;;;; ** grid

(declaim (inline %make-grid))

(defstruct (grid (:constructor %make-grid))
  (width  0 :type fixnum)
  (height 0 :type fixnum)
  (depth  0 :type fixnum)
  (cells  (simple-tvector 0) :type simple-tvector))

(defun grid (width height depth)
  (%make-grid :width width :height height :depth depth
              :cells (simple-tvector (* width height depth)
                                     :element-type 'fixnum
                                     :initial-element +empty+)))

(defun new-temp-grid (grid)
  (make-array (* (grid-width grid) (grid-height grid) (grid-depth grid))))

(declaim (inline point-index))

(defun point-index (grid x y z)
  (declare (type grid grid)
           (type fixnum x y z))
  (the fixnum
    (+ x (the fixnum
           (* (grid-width grid)
              (the fixnum
                (+ y (the fixnum
                       (* (grid-height grid)
                          z)))))))))
  

(defun point (grid x y z)
  (declare (type grid grid)
           (type fixnum x y z))
  (let ((index (point-index grid x y z))
        (cells (grid-cells grid)))
    (the fixnum (tsvref cells index))))


(defun (setf point) (value grid x y z)
  (declare (type grid grid)
           (type fixnum value x y z))
  (let ((index (point-index grid x y z))
        (cells (grid-cells grid)))
    (the fixnum (setf (tsvref cells index) value))))
    

(defun temp-point (grid temp-grid x y z)
  (declare (type grid grid)
           (type simple-vector temp-grid)
           (type fixnum x y z))
  (let1 index (point-index grid x y z)
    (the fixnum (svref temp-grid index))))


(defun (setf temp-point) (value grid temp-grid x y z)
  (declare (type grid grid)
           (type simple-vector temp-grid)
           (type fixnum value x y z))
  (let1 index (point-index grid x y z)
    (the fixnum (setf (svref temp-grid index) value))))


(declaim (inline point-ok?))
(defun point-ok? (grid x y)
  "Check that point is actually within the bounds of grid array."
  (declare (type grid grid)
           (type fixnum x y))
  (and
   (< 0 x (1- (grid-width  grid)))
   (< 0 y (1- (grid-height grid)))))


(defun occupy-grid (grid x1 y1 &optional (x2 x1) (y2 y1) (value +occ+))
  (loop for z from 0 to (1- (grid-depth grid)) do
       (loop for y from y1 to y2 do
            (loop for x from x1 to x2 do
                 (setf (point grid x y z) value))))
  nil)


(defun show-grid (grid &optional (stream t))
  (declare (type grid grid))
  (let ((width  (grid-width  grid))
        (height (grid-height grid))
        (depth  (grid-depth  grid)))
    ;; (format stream "#3A(~%")
    (dotimes (z depth)
      (format stream (if (zerop z) "(" "~&~%("))
      (dotimes (y height)
        (format stream (if (zerop y) "(" "~& ("))
        (dotimes (x width)
          (format t "~3d" (point grid x y z)))
        (write-string ")" stream))
      (write-string ")" stream))
    ;; (format stream ")~%")
    (format stream "~%")))


(defun show-temp-grid (grid temp-grid &optional (stream t))
  (declare (type grid grid)
           (type simple-vector temp-grid))
  (let ((width  (grid-width  grid))
        (height (grid-height grid))
        (depth  (grid-depth  grid)))
    ;; (format stream "#3A(~%")
    (dotimes (z depth)
      (format stream (if (zerop z) "(" "~&~%("))
      (dotimes (y height)
        (format stream (if (zerop y) "(" "~& ("))
        (dotimes (x width)
          (format t "~3d" (temp-point grid temp-grid x y z)))
        (write-string ")" stream))
      (write-string ")" stream))
    ;; (format stream ")~%")
    (format stream "~%")))
    

      
        
        
           


;;;; ** work

(declaim (inline %make-work))

(defstruct (work (:constructor %make-work))
  (x1     0 :type fixnum)
  (y1     0 :type fixnum)
  (x2     0 :type fixnum)
  (y2     0 :type fixnum)
  (netno  0 :type fixnum) ;; index of the connection
  (length^2  0 :type fixnum) ;; length squared
  (priority  0 :type fixnum))


(declaim (inline square distance^2))

(defun square (x)
  "Return (* x x)"
  (declare (type fixnum x))
  (the fixnum (* x x)))

(defun distance^2 (x1 y1 x2 y2)
  "Return square of euclidean distance from (x1 y1) to (x2 y2),
i.e. (+ (square (- x1 x2)) (square (- y1 y2)))."
  (declare (type fixnum x1 y1 x2 y2))
  (the fixnum
    (+ (square (- x1 x2))
       (square (- y1 y2)))))


(defun distance (x1 y1 x2 y2)
  "Return truncated euclidean distance from (x1 y1) to (x2 y2),
i.e. (truncate (sqrt (+ (square (- x1 x2)) (square (- y1 y2)))))."
  (declare (type fixnum x1 y1 x2 y2))
  (the fixnum
    (truncate (sqrt (distance^2 x1 y1 x2 y2)))))
       

(defun work (x1 y1 x2 y2 &key (netno 0))
  (let1 len^2 (distance^2 x1 y1 x2 y2)
    (%make-work :x1 x1 :y1 y1 :x2 x2 :y2 y2
                :netno netno
                :length^2 len^2
                :priority (truncate (sqrt len^2)))))




;;;; ** lee

(declaim (inline %make-lee))

(defstruct (lee (:constructor %make-lee))
  (grid-size         0      :type fixnum :read-only t)
  (max-track-length  0      :type fixnum :read-only t)
  (grid              nil    :type grid   :read-only t)
  (work              nil    :type list)
  ;; number of connections
  (netno             0      :type fixnum)
  (failures          0      :type fixnum))
  


(defun lee (&key (grid-size 600))
  "Create and return a new LEE benchmark struct."
  (%make-lee :grid-size grid-size :max-track-length (* grid-size 5)
             :grid (grid grid-size grid-size 2)))
            

(defun clear-lee (lee)
  "Clear LEE benchmark struct."
  (setf (lee-work     lee) nil
        (lee-netno    lee) 0
        (lee-failures lee) 0)
  (let* ((grid   (lee-grid lee))
         (width  (grid-width grid))
         (height (grid-height grid)))
    (occupy-grid grid 0 0 (1- width) (1- height) 0)))
        
        
(defun enqueue-work (lee x1 y1 x2 y2
                     &key (netno (incf (lee-netno lee))))
  (declare (type lee lee)
           (type fixnum x1 y1 x2 y2 netno))
  (push (work x1 y1 x2 y2 :netno netno)
        (lee-work lee)))


(defun occupy-enqueue-work (lee grid x1 y1 x2 y2)
  (declare (type lee lee)
           (type grid grid)
           (type fixnum x1 y1 x2 y2))
  (occupy-grid grid x1 y1)
  (occupy-grid grid x2 y2)
  (enqueue-work lee x1 y1 x2 y2))


(defun sort-work (work)
  (declare (type list work))
  (sort work #'fixnum< :key #'work-length^2))

(defun sort-lee-work (lee)
  (declare (type lee lee))
  (setf (lee-work lee) (sort-work (lee-work lee))))
           

(defun parse-lee-file (lee filename)
  (declare (type lee lee)
           (type string filename))

  (let1 grid (lee-grid lee)
    (with-open-file (f filename)
      (loop for line = (read-line f nil nil)
         while line
         when (not (zerop (length line))) do

           (let ((words nil)
                 (line (string-left-trim " " line))
                 (start 1))
             (push (char line 0) words)
             (loop do
                  (multiple-value-bind (num next-pos)
                      (parse-integer line :start start :junk-allowed t)
                    (unless num
                      (setf words (nreverse words))
                      (return))
                    (push num words)
                    (setf start next-pos)))

             (let1 ch (pop words)
               (case ch
                 (#\E (return)) ;; end of file
                 
                 (#\C           ;; chip bounding box
                  (occupy-grid grid
                               (pop words) (pop words)
                               (pop words) (pop words)))
                 
                 (#\P           ;; pad
                  (let* ((x (pop words))
                         (y (pop words)))
                    (occupy-grid grid x y x y)))
                 
                 (#\J           ;; join connection points
                  (enqueue-work lee
                                (pop words) (pop words)
                                (pop words) (pop words)))

                 (otherwise
                  (log:warn "unknown command ~s reading file ~s" line filename)))))))))
                

(defun fake-test-data (lee)
  (declare (type lee lee))
  (let1 grid (lee-grid lee)
    (occupy-enqueue-work lee grid 7 3 7 7)
    (occupy-enqueue-work lee grid 3 6 8 6)
    (occupy-enqueue-work lee grid 5 3 8 5)
    (occupy-enqueue-work lee grid 8 3 2 6)
    (occupy-enqueue-work lee grid 4 3 6 7)
    (occupy-enqueue-work lee grid 3 8 8 3)))


(defun expand-from-to (lee temp-grid x y xgoal ygoal
                       &optional (max-track-length (lee-max-track-length lee)))
  "Use Lee's expansion algorithm from coordinate (x,y) to (xgoal, ygoal)
for num iterations.
Return true if the goal is found and false if it is not reached within
the number of iterations allowed."

  (declare (type lee lee)
           (type fixnum x y xgoal ygoal max-track-length)
           (type simple-vector temp-grid))

  (dotimes (i (length temp-grid))
    (setf (svref temp-grid i) +temp-empty+))

  (let ((front     (frontier-fifo))
        (alt-front (frontier-fifo))
        (grid      (lee-grid lee))
        (extra-iterations 50)
        (reached0  nil)
        (reached1  nil))

    (declare (type boolean reached0 reached1)
             (type fixnum extra-iterations))

    (setf (temp-point grid temp-grid x y 0) 1) ;; set grid (x,y) as 1
    (setf (temp-point grid temp-grid x y 1) 1) ;; set grid (x,y) as 1

    (push-new-frontier front x y 0 0)
    (push-new-frontier front x y 1 0) ;; we can start from either

    (loop named outer until (frontier-fifo-empty? front) do
         (loop until (frontier-fifo-empty? front)
            for f   = (pop-frontier front)
            for fx  = (frontier-x  f)
            for fy  = (frontier-y  f)
            for fz  = (frontier-z  f)
            for fdw = (frontier-dw f) do

              (frontier-to-pool f)

              (if (plusp fdw)
                  ;; Used? fdw actually seems to always be zero...
                  (push-new-frontier alt-front fx fy fz (1- fdw))

                  (let1 curr-val (temp-point grid temp-grid fx fy fz)

                    (log:debug "processing (~d ~d ~d), weight ~d"
                               fx fy fz curr-val)

                    (when (> curr-val max-track-length)
                      (log:debug "weight ~d exceeds max track length ~d, quitting loop"
                                 curr-val max-track-length)
                      (return-from outer))


                    (loop for (dx . dy) in '((0 . 1) (1 . 0) (0 . -1) (-1 . 0))
                       for fdx = (the fixnum (+ fx dx))
                       for fdy = (the fixnum (+ fy dy))
                       when (point-ok? grid fdx fdy) do

                         (let* ((weight   (point grid fdx fdy fz))
                                ;; +occ+ is negative
                                (empty?   (>= weight +empty+))
                                (reached? (and (= fdx xgoal) (= fdy ygoal))))

                           (when (or empty? reached?)
                             (let ((val (if reached?
                                            (the fixnum (+ curr-val 1))
                                            (the fixnum (+ curr-val 1 weight))))
                                   (next-val (temp-point grid temp-grid fdx fdy fz)))

                               (when (> next-val val)
                                 (setf (temp-point grid temp-grid fdx fdy fz) val)
                                 (log:debug "set (~d ~d ~d) to weight ~d (was ~d)"
                                            fdx fdy fz val next-val)
                                 (unless reached?
                                   (push-new-frontier alt-front fdx fdy fz 0)))))))


                    (let* ((fdz      (the fixnum (- 1 fz)))
                           (weight   (point grid fx fy fdz)))
                      (when (>= weight +empty+) ;; +occ+ is negative
                        
                        (let ((next-val (temp-point grid temp-grid fx fy fdz))
                              ;; setting val = (+ curr-val weight 1) as above
                              ;; probably makes more sense, but the original lee-tm benchmark
                              ;; sets val = curr-val when changing layer,
                              ;; and we want to reproduce exactly the original algorithm
                              (val      curr-val))

                          (when (> next-val val)
                            (setf (temp-point grid temp-grid fx fy fdz) val)
                            (push-new-frontier alt-front fx fy fdz 0)))))

                    ;; must check if found goal, if so return true
                    (unless reached0
                      (setf reached0 (/= +temp-empty+ (temp-point grid temp-grid xgoal ygoal 0))))

                    (unless reached1
                      (setf reached1 (/= +temp-empty+ (temp-point grid temp-grid xgoal ygoal 1))))

                    ;; upon reaching goal, allow 100 more iterations
                    ;; to explore a bit further
                    (unless (eq reached0 reached1)
                      (setf extra-iterations 100))

                    (when (or reached0 reached1)
                      (log:sexp-debug reached0 reached1)
                      (if (zerop extra-iterations)
                          ;; (xgoal ygoal) found in time
                          (return-from outer)
                          (decf extra-iterations))))))

         (rotatef front alt-front))

    (clear-frontier-fifo front)
    (clear-frontier-fifo alt-front)

    ;; on small grids, we may expand to the whole grid
    ;; *before* running out of extra-iterations  
    (or reached0 reached1)))


(defvar *dx* #2A((-1  1  0  0) ( 0  0 -1  1)))
(defvar *dy* #2A(( 0  0 -1  1) (-1  1  0  0)))


(defun path-from-other-side (grid temp-grid x y z)
  (declare (type grid grid)
           (type simple-vector temp-grid)
           (fixnum x y z))
  
  (let* ((zo (the fixnum (- 1 z)))
         (point-o (temp-point grid temp-grid x y zo)))

    (unless (or (= +via+ point-o) (= +bvia+ point-o))
      (when (<= point-o (temp-point grid temp-grid x y z))
        (or (<= (temp-point grid temp-grid (1- x) y zo) point-o)
            (<= (temp-point grid temp-grid (1+ x) y zo) point-o)
            (<= (temp-point grid temp-grid x (1- y) zo) point-o)
            (<= (temp-point grid temp-grid x (1+ y) zo) point-o))))))



(defun backtrack-from-to (lee temp-grid x y xgoal ygoal netno)
  "backtrack from the goal position (XGOAL YGOAL) back to the starting position
\(X Y) filling GRID with the specified track number NETNO.

*** algorithm ***
CurrentPos = Goal
Loop
Find dir to start back from current position
Loop
Keep going in current dir and Fill in track (update currentPos)
Until box number increases in this current dir
Until back at starting point
*** end algorithm ***"
  (declare (type lee lee)
           (type fixnum x y xgoal ygoal netno)
           (type simple-vector temp-grid))
  (log:debug "Track ~d backtrack length ~d" netno (distance x y xgoal ygoal))

  (let ((grid (lee-grid lee))
        (zgoal (if (> (abs (- xgoal x)) (abs (- ygoal y)))
                   0
                   1)))
    (when (= +temp-empty+ (temp-point grid temp-grid xgoal ygoal zgoal))
      (log:debug "preferred layer ~d not reached" zgoal)
      (setf zgoal (- 1 zgoal)))

    (let ((xt xgoal)
          (yt ygoal)
          (zt zgoal)
          (num-vias 0)
          (forced-vias 0)
          (dist-so-far 0)
          (track (- +track+ netno))) ;; +track+ is negative

      (declare (type fixnum zgoal xt yt zt
                     num-vias forced-vias dist-so-far track))

      (loop until (and (= xt x) (= yt y)) ;; until back at (x y)
         for advanced = nil
         for min-d = 0
         for dir = 0
         for point   = (point grid xt yt zt)
         for point-t = (temp-point grid temp-grid xt yt zt)
         for min-weight = point-t
         do
           (dotimes (d 4) ;; find dir to start back from
             (let* ((dx (the fixnum (aref *dx* zt d)))
                    (dy (the fixnum (aref *dy* zt d)))
                    (xd (the fixnum (+ xt dx)))
                    (yd (the fixnum (+ yt dy)))
                    (point-d (temp-point grid temp-grid xd yd zt)))
                   
               (when (and (>= point-d +empty+) ;; +occ+ is negative
                          (< point-d min-weight))
                 (setf min-weight point-d
                       min-d d
                       dir (the fixnum (+ (ash dx 1) dy)) ;; hashed dir
                       advanced t))))
           
           (when advanced
             (incf dist-so-far))

           (log:debug "Backtracking track ~d from (~d ~d ~d), weight ~d, advanced ~a, min-d ~d"
                      netno xt yt zt point-t advanced min-d)
           
           (if (and (or (and (> min-d 1)
                             ;; not preferred dir for this layer
                             (> dist-so-far 15)
                             (> (distance^2 xt yt x y) 225))
                        (and (not advanced)
                             (/= +via+  point)
                             (/= +bvia+ point)))
                    (path-from-other-side grid temp-grid xt yt zt))

               (let1 via-t (if advanced +via+ +bvia+) ;; BVIA is nowhere else to go

                 (when (< point +empty+) ;; +occ+ is negative
                   (log:debug "Track ~d failed, point (~d ~d ~d) contains ~d, is not empty (case 1)"
                              netno xt yt zt point)
                   (return-from backtrack-from-to nil))
                 ;; mark via
                 (setf (temp-point grid temp-grid xt yt zt) via-t
                       (point grid xt yt zt) track
                       zt (the fixnum (- 1 zt))) ;; 0 if 1, 1 if 0
                 (let1 point (point grid xt yt zt)
                   (when (< point +empty+) ;; +occ+ is negative
                     (log:debug "Track ~d failed, point (~d ~d ~d) contains ~d, is not empty (case 2)"
                                netno xt yt zt point)
                     (return-from backtrack-from-to nil)))
                 ;; and the other side
                 (setf (temp-point grid temp-grid xt yt zt) via-t
                       (point grid xt yt zt) track)
                 (incf num-vias)
                 (if advanced
                     (log:debug "Via. dist-so-far ~d, dist-left ~d"
                                dist-so-far (distance xt yt x y))
                     (incf forced-vias))
                 (setf dist-so-far 0))
               
               ;; else
               (progn
                 (cond ;; +occ+ is negative
                   ((>= point +empty+)
                    ;; fill in track unless connection point
                    (setf (point grid xt yt zt) track))
                   ((= point +occ+)
                    (when (log:debug)
                      (unless (or (and (= xt xgoal) (= yt ygoal))
                                  (and (= xt x) (= yt y)))
                        (log:debug "Not backtracking track ~d, point (~d ~d ~d) contains ~d, is not empty (case 3)"
                                   netno xt yt zt point))))
                   ((and (< point +occ+) (/= point track))
                    (log:debug "Track ~d failed, point (~d ~d ~d) contains ~d, is not empty (case 4)"
                               netno xt yt zt point)
                    (return-from backtrack-from-to nil)))
                 
                 ;; update current position
                 (incf xt (aref *dx* zt min-d))
                 (incf yt (aref *dy* zt min-d)))))))

  (log:debug "Track ~d completed" netno)
  t)


               
                   

(defun lee-connect (lee q temp-grid)
  "Call expandFrom and backtrackFrom to create connection described by work Q.
Return 0 if work q completes successfully,
return 1 if it fails during expansion,
return 2 if it fails during backtrack."

  (declare (type lee lee)
           (type work q)
           (type simple-vector temp-grid))

  (let ((x     (work-x1 q))
        (y     (work-y1 q))
        (xgoal (work-x2 q))
        (ygoal (work-y2 q))
        (netno (work-netno q)))

    (log:debug "Connecting (~d ~d) to (~d ~d) netno ~d" x y xgoal ygoal netno)

    ;; Call expandFrom and backtrackFrom to create connection                
    ;; This is the only real change needed to make the program
    ;; transactional.
    ;; Instead of using the grid 'in place' to do the expansion, we take a
    ;; copy
    ;; but the backtrack writes to the original grid.
    ;; This is not a correctness issue. The transactions would still
    ;; complete eventually without it.
    ;; However the expansion writes are only temporary and do not logically
    ;; conflict.
    ;; There is a question as to whether a copy is really necessary as a
    ;; transaction will anyway create
    ;; its own copy. if we were then to distinguish between writes not to be
    ;; committed (expansion) and
    ;; those to be committed (backtrack), we would not need an explicit
    ;; copy.
    ;; Taking the copy is not really a computational(time) overhead because
    ;; it avoids the grid 'reset' phase
    ;; needed if we do the expansion in place.

    (if (expand-from-to lee temp-grid x y xgoal ygoal)
        (progn
          (log:debug "track ~d found route (~d ~d) to (~d ~d)" netno x y xgoal ygoal)
          (if (atomic (backtrack-from-to lee temp-grid x y xgoal ygoal netno))
              0
              1))
        (progn
          (log:debug "track ~d cannot find route from (~d ~d) to (~d ~d)" netno x y xgoal ygoal)
          2))))
