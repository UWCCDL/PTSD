;;; Lisp model for PTSD


;;; Simulated agent life

#|
(defun life ()
  (loop-until-end
	 (generate-new-context)
	 (when (time = target)
	   (insert traumatic Q0))
	 (set-new context)
	 (set-goal)
	 (set-new-update))
|#

;;; Memory-based behavioral control


(clear-all)

(define-model ptsd1

(sgp :esc t
     :er t)

(chunk-type task processed)

;;; DECLARATIVE KNOWLEDGE

;(add-dm (memory1 f1 a f2 b f3 c f4 c)
;		(memory2 f1 a f2 b f3 c f4 c)
;		(task processed no))

(add-dm (yes) (no))

(p face-situation
   "Realizes a new situation is present, and sets a goal to process it"
   ?goal>
     state free
     buffer empty
   ?imaginal>
     state free
     buffer full
==>
   +goal>
     isa task
     processed no
)

;;; PROC KNOWLEDGE
(p retrieve
   "Retrieves an appropriate chunk to respond to the current context"
   =goal>
     processed no
   
   ?retrieval>
     buffer empty
	 state free
==>
   +retrieval>
)

(p elaborate
   "Use the retrieved memory to respond approrpriately to the current context"
   =goal>
     processed no
   
   ?retrieval>
     buffer full
	 state free
==>
   =goal>
      processed yes
   -retrieval>
   -imaginal>
)

(p cant-retrieve
   "Catches retrieval errors"
   =goal>
     processed no
   
   ?retrieval>
     buffer empty
	 state error
==>
   *goal>
     processed yes
   -retrieval>
   -imaginal>
   !stop!
)

(p solved
   "Pops the goal"
   =goal>
     processed yes
   
   ?goal>
     state free  
==>
   -goal>
   !stop!
)

;(goal-focus task)

)
