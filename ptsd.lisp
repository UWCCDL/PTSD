;;; Lisp model for PTSD


;;; Simulated agent life


(clear-all)

(define-model ptsd

(sgp :esc t
     :er t
     :bll 0.5
     :blc 1.0
     
     ;; Enable WM effects through spreading activation
     :imaginal-activation 5.0

     ;; Adds V term for base-level activation 
     :activation-offsets "v_offset"

     ;; Similarity-based metric
     :sji-hook "sji_calculation"

     ;; Monotor responses
     :retrieved-chunk-hook "monitor_retrievals"
     )

(chunk-type task processed)

(chunk-type memory kind slot1 slot2 slot3
            slot4 slot5 slot6 slot7 slot8
            slot9 slot10 V)

(chunk-type situation kind value) 


;;; DECLARATIVE KNOWLEDGE

(add-dm (a) (b) (c) (d) (e) (f) (g) (h) (i) (j) (k)
        (yes) (no) (memory))

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
     kind memory
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

)
