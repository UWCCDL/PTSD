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

(sgp :esc t)
	
;;; DECLARATIVE KNOWLEDGE

(add-dm (memory1 f1 a f2 b f3 c f4 c)
		(memory2 f1 a f2 b f3 c f4 c)
		(task processed no))
	
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
   -visual>
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
   =goal>
     processed yes
   -retrieval>
   -visual>
   -imaginal>
)

(goal-focus task)

)
