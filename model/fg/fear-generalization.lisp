;;; Fear generalization in PTSD

;;; This is from the original Smith & Stocco 2019 code (modified to use the goal  buffer instead of the imaginal) 
(defmethod modified-spreading-activation (chunk
                                          &optional (buffer 'goal))
  (let ((source (no-output (buffer-chunk-fct (list buffer)))))
    (when (> (length source) 0)
      (setf source (first source))
      (when (and source
                 (not (equalp chunk source)))
        (let ((kind1 (chunk-slot-value-fct source 'KIND))
              (kind2 (chunk-slot-value-fct chunk 'KIND)))
          (when (and (equalp kind1 'episode)
                     (equalp kind2 'episode))
            (let ((sim (chunk-similarity source chunk))
                  (w (get-parameter-value :ga)))
              (when (null w)
                (setf w 0.))
              (* sim w))))))))



(defun chunk-similarity (chunk1 chunk2)
  "Calculates the simulariy between two chunks"
  (when (and chunk1 chunk2)
    (let ((v1 (vectorize-memory chunk1))
          (v2 (vectorize-memory chunk2)))
      (when (= (length v1)
               (length v2))
        (/ (reduce #'+ (mapcar #'(lambda (x y) (if (and (equalp x y)
							 (not (null x))) 1 0))
                               v1 v2))
           (length v1))))))


(defmethod vectorize-memory (chunk)
  "Transforms a chunk into a vector of attributes"
  (mapcar #'(lambda (x) (chunk-slot-value-fct chunk x))
          '(first second slot)))

(defparameter *I* 1)

(defun chunk-emotional-intensity (chunk)
  (let ((shock (no-output (chunk-slot-value-fct chunk 'shock))))
    (if (equalp shock 'plus)
	*I*
	0)))

(defun reload-fg ()
  (delete-model)
  (load "fear-generalization.lisp"))

;;; -------------------------------------------------------------- ;;;
;;; THE MODEL
;;; -------------------------------------------------------------- ;;;

(define-model basic-model

  (sgp :ga 1
       :er t
       :esc t
       :bll 0.5
       :blc 0
       :ans 0.5
       :ol nil
       :esc t
       :rt 0.0
       :mas 10.0
       ;:sji-hook chunk-similarity ; 'modified-spreading-activation chunk
       :spreading-hook modified-spreading-activation
       :activation-offsets chunk-emotional-intensity
       )

;  (set-parameter-value :spreading-hook 'modified-spreading-activation)
  
(chunk-type stimulus first second shock kind id)

(add-dm (stim isa chunk)
	(episode isa chunk)
	(shock isa chunk)
	(a1 isa chunk kind stim)
	(b1 isa chunk kind stim)
	(a2 isa chunk kind stim)
	(b2 isa chunk kind stim)
	(a3 isa chunk kind stim)
	(b3 isa chunk kind stim)
	(a4 isa chunk kind stim)
	(b4 isa chunk kind stim)
	(a5 isa chunk kind stim)
	(b5 isa chunk kind stim)
	(a6 isa chunk kind stim)
	(b6 isa chunk kind stim)
	(a7 isa chunk kind stim)
	(b7 isa chunk kind stim)
	(a8 isa chunk kind stim)
	(b8 isa chunk kind stim)

	(x isa chunk kind stim)
	(plus isa chunk kind shock)
	(minus isa chunk kind shock)
	(ax1 isa stimulus first a1 second x shock plus kind episode id 1)
	(ax2 isa stimulus first a2 second x shock plus kind episode id 2)
	(ax3 isa stimulus first a3 second x shock plus kind episode id 3)
	(ax4 isa stimulus first a4 second x shock plus kind episode id 4)
	(ax5 isa stimulus first a5 second x shock plus kind episode id 5)
	(ax6 isa stimulus first a6 second x shock plus kind episode id 6)
	(ax7 isa stimulus first a7 second x shock plus kind episode id 7)
	(ax8 isa stimulus first a8 second x shock plus kind episode id 8)

	(bx1 isa stimulus first b1 second x shock minus kind episode id 1)
	(bx2 isa stimulus first b2 second x shock minus kind episode id 2)
	(bx3 isa stimulus first b3 second x shock minus kind episode id 3)
	(bx4 isa stimulus first b4 second x shock minus kind episode id 4)
	(bx5 isa stimulus first b5 second x shock minus kind episode id 5)
	(bx6 isa stimulus first b6 second x shock minus kind episode id 6)
	(bx7 isa stimulus first b7 second x shock minus kind episode id 7)
	(bx8 isa stimulus first b8 second x shock minus kind episode id 8)
	
	(a1x isa stimulus first a1 second x shock nil kind episode)
	(b1x isa stimulus first b1 second x shock nil kind episode)
	;(x-- isa stimulus first x second nil shock nil kind episode)
	(a1b1 isa stimulus first a1 second b1 shock nil kind episode))
		      
(p experience-stimulus
   "Experiences a stimulus"
   ?goal>
     state free
     buffer full

   ?retrieval>
     state free
     buffer empty
==>
   +retrieval>
     kind episode
   )

;;(goal-focus a--)
)


(defun simulate (n &key (stimulus 'a1x) (intensity 1) (constant 1) (rt 0) (noise 0.2))
  (let ((res nil))
    (dotimes (i n)
      (reload-fg)
      (sgp-fct `(:blc ,constant :rt ,rt :v nil :ans ,noise))  
      (setf *I* intensity)
      (goal-focus-fct stimulus)
      (run 1)
      (let* ((retrieved (no-output (first (buffer-chunk-fct '(retrieval)))))
	     (shock (if (null retrieved)
			nil
			(no-output
			  (chunk-slot-value-fct retrieved
						'shock)))))
	(if (equal shock 'plus)
	    (push 1 res)
	    (push 0 res))))
    res))

(defun mean (nums)
  (float (/ (reduce #'+ nums)
	    (length nums))))

(defun uber-simulate (&key (logfile "out.csv") (header t))
  (with-open-file (logfile logfile
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (when header
      (format logfile "~{~A~^,~}~%" '("Noise" "Intensity" "Threshold" "BLC" "Stimulus" "Percentage")))
              
    (dolist (noise '(0.25 0.3 0.35 0.4 0.45 0.5))
      (dolist (intensity '(0 0.1))
	(dolist (const '(0.25 0.5))
	  (dolist (rt '(1.0 2 2.5 3 3.25 3.5 3.75 4))
	    (dolist (chunk '(a1x b1x a1b1))
	      (let* ((res (simulate 200 :stimulus chunk :intensity intensity :rt rt :noise noise))
		     (row (list (no-output (first (sgp-fct '(:ans))))
				*I*
				(no-output (first (sgp-fct '(:rt))))
				(no-output (first (sgp-fct '(:blc))))
				chunk
				(mean res))))
		(format logfile "~{~,4f~^,~}~%" row))))))
      (format t "Done (noise ~a)~%" noise))))
