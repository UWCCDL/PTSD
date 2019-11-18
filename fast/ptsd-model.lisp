;;; -------------------------------------------------------------- ;;;
;;; AN ACT-R MODEL OF INTRUSIVE MEMORIES IN PTSD
;;; -------------------------------------------------------------- ;;;
;;; (c) 2019, Briana Smith and Andrea Stocco
;;; University of Washington, Seattle, WA 98195
;;; -------------------------------------------------------------- ;;;


(clear-all)

(define-model ptsd

(sgp :esc t
     :er t
     :bll 0.5
     :blc 1.0
     :ans 0.2
     :ol t

     ;; Enable WM effects through spreading activation
     :mas 100.0
     :imaginal-activation 5.0
     
     ;; Adds V term for base-level activation
     ;:activation-offsets "v_offset"
     ;:chunk-add-hook "keep_table"
     
     ;; Similarity-based metric
     ;;:sji-hook "sji_calculation"
     ;:spreading-hook "spreading"
     
     ;; Monitor responses
     ;;:retrieved-chunk-hook "monitor_retrievals"
     )

;;; ---- CHUNK TYPES ----------------------------------------------- #

;;; Internal goal
;;;
(chunk-type task processed rumination)

;;; Memory attributes (chunk slot values for memories)
;;;
(chunk-type memory kind q1 q2 q3
            q4 q5 q6 q7 q8 q9 q10
            q11 q12 q13 q14 q15
            traumatic)

;;; ** Currently unused **
(chunk-type situation kind value)


;;; ---- DECLARATIVE KNOWLEDGE ------------------------------------- #

;;; Chunk attributes (i.e., q values)
;;;
;;(add-dm (a) (b) (c) (d) (e) (f) (g) (h)
(define-chunks (a) (b) (c) (d) (e) (f) (g) (h)
               (i) (j) (k) (l) (m) (n) (o) (p)
               (q) (r) (s) (t) (u) (v) (w) (x)
               (y) (z) (yes) (no) (memory))


;;; ---- PROCEDURAL KNOWLEDGE AND CONTROL -------------------------- #
;;;
;;; Basically, the entire agent is a knowledgeless
;;; perceive-retrieve-respond loop.
;;; ---------------------------------------------------------------- #

;;; STEP 1: PERCEIVE

(p perceive
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
      processed no)

;;; STEP 2: RETRIEVE

(p retrieve
    "Retrieves an appropriate chunk to respond to the current context"
    =goal>
      processed no

    ?retrieval>
      buffer empty
      state free
==>
    +retrieval>
      kind memory)

;;; STEP 3: RESPOND

(p respond
    "Use the retrieved memory to respond appropriately to the current context"
    =goal>
      processed no

    ?retrieval>
      buffer full
      state free
==>
    =goal>
       processed yes
      -retrieval>
      -imaginal>)


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
)

(p solved
    "Pops the goal"
    =goal>
      processed yes

    ?goal>
      state free
  ==>
    -goal>
    )

) ;;; End of model
