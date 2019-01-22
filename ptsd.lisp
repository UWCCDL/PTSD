;;; Lisp model for PTSD

(defparameter *traumatic-event-time* (* 60 60 24 365 20)
  "A traumatic memory is generated at age 20 exatcly") 

(defun activation-hook (chunk mod)
  "Hook for the activation function---adds emotional value V to activation"
  nil)

;;; Simulated agent life

(defun life ()
  (loop-until-end
	 (generate-new-context)
	 (when (time = target)
	   (insert traumatic Q0))
	 (set-new context)
	 (set-goal)
	 (set-new-update))

;;; Memory-based behavioral control

(p retrieve
   "Retrieves an appropriate chunk to respond to the current context"
   =goal>
     processed no
   ?retrieval>
     Buffer empty
	 State free
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
