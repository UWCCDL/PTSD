;;; -------------------------------------------------------------- ;;;
;;; PTSD SIMULATION CODE FOR FAST EXECUTION
;;; -------------------------------------------------------------- ;;;
;;; (c) 2019, Briana Smith and Andrea Stocco
;;; University of Washington, Seattle, WA 98195
;;; -------------------------------------------------------------- ;;;

;;;(ql:quickload "cl-mathstats")

(defconstant +letters+ '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
  "Letters are used as symbolic stimulus attributes")

(defconstant +minutes-per-day+ (* 60 24)
  "Handy constant to aoid recalculating the num of minutes in a day")


;;; -------------------------------------------------------------- ;;;
;;; UTILITIES
;;; -------------------------------------------------------------- ;;;

(defun seq (start end &optional (step 1))
  "Creates a ranges"
  (let ((results nil)
	(partial start))
    (cond ((and (< start end)
		(plusp step))
	   (loop while (< partial end) do
	     (push partial results)
	     (incf partial step)))
	  ((and (> start end)
		(minusp step))
	   (loop while (> partial end) do
	     (push partial results)
	     (incf partial step)))
	  (t
	   nil))
    (reverse results)))
	  

(defun pick (lst)
  "Picks up an element from a list"
  (when  (listp lst)
    (elt lst (random (length lst)))))


(defun scramble (lst &optional (sofar nil))
  "Scrambles a list of different elements"
  (if (null lst)
      sofar
    (let ((picked (pick lst)))
      (scramble (remove picked lst) (cons picked sofar)))))


(defun scramble* (lst)
  "Scrambles any list of objects"
  (let ((l (length lst))
        (pos nil))
    (dotimes (i l)
      (push i pos))
    (mapcar #'(lambda (x) (elt lst x)) (scramble pos))))


(defun gamma-stirling (x)
  "Stirling approximation to Gamma function"
  (* (sqrt (* 2 (/ PI x)))
     (expt (/ x (exp 1)) x)))


(defun dgamma (x shape scale)
  "Gamma distribution with given shape and scale"
  (let ((rate (/ 1 scale)))
    (/ (* (expt rate shape)
          (expt x (- shape 1))
          (exp (* -1 rate x)))
       (gamma-stirling shape))))
       ;;(exp (cl-mathstats:gamma-ln shape)))))
    

;;; -------------------------------------------------------------- ;;;
;;; SIMULATION OBJECT AND METHODS
;;; -------------------------------------------------------------- ;;;

(defclass simulation ()
  ((model :accessor model
          :initform "ptsd-model.lisp")
   (n :accessor n
      :initform 100)
   (ptev :accessor ptev
         :initform '(1 5 10))
   (ptes :accessor ptes
         :initform '(0 0.5 1))
   ;;(ptet :accessor ptet          ;; Deprecated
   ;;      :initform (* 60 500))
   ;;(max-time :accessor max-time  ;; Deprecated
   ;;          :initform 200000)
   (event-step :accessor event-step
               :initform (* 60 20))
   (counter :accessor counter
            :initform 0)
   (num-slots :accessor num-slots
              :initform 10)
   (num-attributes :accessor num-attributes
                   :initform 4)
   (current-v :accessor current-v
              :initform 1)
   (current-s :accessor current-s
              :initform 0)
   (v-table :accessor v-table
            :initform (make-hash-table))
   (num-days-before :accessor num-days-before
                    :initform 100)
   (num-days-after :accessor num-days-after
                   :initform 60)
   (slot-names :accessor slot-names  ;; Deprecated
               :initform '(q1 q2 q3 q4 q5 q6)) 
   (model-trace :accessor model-trace
                :initform nil)
   (model-params :accessor model-params
                 :initform (make-hash-table))
   (event-frequency :accessor event-frequency
                    :initform 20)
   (rumination-frequency :accessor rumination-frequency
                         :initform 0)
   (logfile :accessor logfile
            :initform nil)
   
   ;; These are values that will be stored during initialization
   ;; so that can be read out without the need to recaclulate them
   
   (slot-values :accessor slot-values
                :initform (subseq +letters+ 0 4))
   (traumatic-slot-values :accessor traumatic-slot-values
                          :initform   (subseq (reverse +letters+) 0 4))))

(defmethod init ((s simulation))
  "Prepares all the internal values for a simulation"
  (setf (slot-names s)
        (mapcar #'make-slot-name (seq 1 (1+ (num-slots s)))))
  (setf (slot-values s)
        (subseq +letters+ 0 (num-attributes s)))
  (setf (traumatic-slot-values s)
        (subseq (reverse +letters+) 0 (num-attributes s))))

(defmethod ptet ((s simulation))
  (* 60 +minutes-per-day+ (num-days-before s)))


;;(defmethod traumatic-slot-values ((s simulation))
;;  "Returns the list of attributes used for traumatic memories"
;;  (subseq (reverse +letters+) 0 (num-attributes s))) 


;;(defmethod slot-values ((s simulation))
;;  "Returns the list of attributes used for non-traumatic memories"
;;  (subseq +letters+ 0 (num-attributes s))) 


;;(defmethod slot-names ((s simulation))  
;;  (mapcar #'make-slot-name (seq 1 (1+ (num-slots s)))))

  
(defmethod set-model-parameter ((s simulation) param value)
  "Sets the value of ACT-R parameters for the current simulation" 
  (when (keywordp param)
    (setf (gethash param (model-params s))
          value)))


(defmethod get-model-parameters ((s simulation))
  "Returns the names of ACT-R parameters that are modified in the current simulation" 
  (sort (hash-table-keys (model-params s)) #'string-lessp))


(defmethod get-model-parameter-values ((s simulation))
  "Returns the values of the ACT-R parameters set in the current simulation"
  (let ((hash (model-params s)))
    (mapcar #'(lambda (x)
                (gethash x hash))
            (get-model-parameters s))))

(defun make-slot-name (num)
  "Generates a symbol 'Q<N>', with N being an integer number"
  (intern (string-upcase (format nil "Q~A" num))))


(defmethod generate-random-event ((s simulation) &optional (traumatic nil))
  "Generates a random memory"
  (let ((template (make-list (num-slots s) :initial-element nil))
        (tslot '(traumatic no))
        (slots '(isa memory kind memory)))
    (when traumatic
      (let ((num-changes (round (* (num-slots s)
                                   (- 1 (current-s s))))))
        (dotimes (j num-changes)
          (setf (nth j template) t)))
      (setf tslot '(traumatic yes))
      (setf template (scramble* template)))
      
    (dotimes (j (length template))
      (let ((val nil))
        (if (nth j template)
            (setf val (pick (traumatic-slot-values s)))
            (setf val (pick (slot-values s))))
        (setf slots (append slots (list (make-slot-name (1+ j))
                                        val)))))
    (setf slots (append slots tslot))))

      
(defmethod present-new-event ((s simulation) &optional (buffer 'imaginal))
  "Presents a new situation to the model"
  (when (query-buffer buffer '(state free
                               error nil
                               buffer empty))
    (let* ((newdef (generate-random-event s (= (mp-time)
                                               (ptet s))))
           (newchunk (first (define-chunks-fct (list newdef)))))
      (set-buffer-chunk buffer newchunk))))
    

(defmethod chunk-v-term ((s simulation) chunk)
  "Returns the log(V) term associated with a given chunk (and used for activation)"
  (log (gethash chunk (v-table s) 1.0)))


(defmethod add-chunk ((s simulation) chunk)
  "Adds a new chunk to the internal list of memories"
  (if (equalp (chunk-slot-value-fct chunk 'traumatic) 'no)
      (setf (gethash chunk (v-table s)) (random 2.0))
      (setf (gethash chunk (v-table s)) (current-v s))))


(defmethod vectorize-memory ((s simulation) chunk)
  "Transforms a chunk into a vector of attributes"
  (mapcar #'(lambda (x) (chunk-slot-value-fct chunk x))
          (slot-names s)))


(defmethod chunk-similarity ((s simulation) chunk1 chunk2)
  "Calculates the simulariy between two chunks"
  (let ((v1 (vectorize-memory s chunk1))
        (v2 (vectorize-memory s chunk2)))
    (when (= (length v1)
             (length v2))
      (/ (reduce #'+ (mapcar #'(lambda (x y) (if (equalp x y) 1 0))
                             v1 v2))
         (length v1)))))
  

(defmethod modified-spreading-activation ((s simulation)
                                          chunk
                                          &optional (buffer 'imaginal))
  (let ((source (no-output (buffer-chunk-fct (list buffer)))))
    (when (> (length source) 0)
      (setf source (first source))
      (when (not (equalp chunk source))
        (let ((kind1 (chunk-slot-value-fct source 'KIND))
              (kind2 (chunk-slot-value-fct chunk 'KIND)))
          (when (and (equalp kind1 'memory)
                     (equalp kind2 'memory))
            (let ((sim (chunk-similarity s source chunk))
                  (w (get-parameter-value :imaginal-activation)))
              (when (null w)
                (setf w 0.))
              (* sim w))))))))


(defmethod monitor-retrievals ((s simulation) chunk)
  "Keeps track of what is being retrieved"
  (push (create-trace-entry s chunk)
        (model-trace s)))

;; # Run,V_Traumatic,Time,V,Traumatic,Similarity
(defmethod create-trace-entry ((s simulation) chunk)
  (let* ((entry (list (counter s)
                      (current-v s)
                      (current-s s)
                      (num-slots s)
                      (num-attributes s)
                      (mp-time)
                      (if chunk
                          (chunk-v-term s chunk)
                          0)))
         
         (traumatic-value (if chunk
                              (chunk-slot-value-fct chunk 'traumatic)
                              nil))
           
         (traumatic (if (equalp traumatic-value 'yes) 1 0))

         (source (first (no-output (buffer-chunk-fct '(imaginal)))))

         (similarity (if chunk
                         (chunk-similarity s source chunk)
                         0)))
    
    (append entry (list traumatic similarity) (get-model-parameter-values s)))) 


(defparameter *colnames*
  '("Run" "PTEV" "PTES" "NumSlots"
    "NumAttributes" "Time" "ChunkV" "ChunkSimilarity")
  "Names of the fundametal values to log")


(defmethod save-trace ((s simulation))
  (with-open-file (fle (logfile s)
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (format fle "~{~A~^, ~}~%" (append *colnames* (get-model-parameters s)))
    (dolist (row (model-trace s))
      (format fle "~{~5,f~^, ~}~%" row))))

         
(defmethod simulate ((s simulation))
  (init s)
  (load "ptsd-model.lisp")
  ;;; Set hooks
  (set-parameter-value :activation-offsets #'(lambda (chunk)
                                               (chunk-v-term s chunk)))
  (set-parameter-value :chunk-add-hook #'(lambda (chunk)
                                           (add-chunk s chunk)))

  (set-parameter-value :spreading-hook #'(lambda (chunk)
                                           (modified-spreading-activation s chunk)))

  (set-parameter-value :retrieved-chunk-hook #'(lambda (chunk)
                                                 (monitor-retrievals s chunk)))

  (set-parameter-value :v nil)

  ;; Set the simulation-specific parameters

  (let ((params (model-params s))) 
    (dolist (key (hash-table-keys params))
      (set-parameter-value key (gethash key params)))) 

  ;; Resets simulations
  
  (setf (v-table s) (make-hash-table))
  ;;(setf (model-trace s) nil)

  ;;** OLD CODE **
  ;;(let ((time 0))
  ;;  (while (< time (max-time s))
  ;;    (schedule-event time #'present-new-event :params (list s))
  ;;    (incf time (event-step s))))
  ;;(run (max-time s))

  (let ((pte (list (* (num-days-before s)
                      +minutes-per-day+)))
        (before (generate-timeline (event-frequency s)
                                   0
                                   (num-days-before s)))
        (after (generate-timeline (event-frequency s)
                                  (num-days-before s)
                                  (num-days-after s))))
    
    (dolist (j (append before pte after))
       (schedule-event (* 60 j) #'present-new-event :params (list s))))
  (run (* 60 +minutes-per-day+ (+ (num-days-before s)
                                  (num-days-after s)))))


(defun generate-timeline (density start-day end-day
                          &key (gamma-shape 2.0) (gamma-scale 175))
  "Generate a time-line of events given a probability density function"
  (let ((queue nil))
    (dotimes (day end-day  (reverse queue))
      (dotimes (minute +minutes-per-day+)
        (when (> (* (dgamma minute gamma-shape gamma-scale)
                    density)
                 (random 1.0))
          (push (+ (* (+ start-day day) +minutes-per-day+)
                   minute)
                queue))))))


(defmethod run-simulations ((s simulation))
  (setf (model-trace s) nil)
  (dolist (v-val (ptev s))
    (dolist (s-val (ptes s))
      (setf (current-v s) v-val)
      (setf (current-s s) s-val)
      (dotimes (ii (n s))
        (setf (counter s) ii)
        (simulate s))))
  (unless (null (logfile s))
    (save-trace s)))

;;; -------------------------------------------------------------- ;;;
;;; DM EXPLORATION
;;; -------------------------------------------------------------- ;;;

(defun activation-distribution ()
  (let ((ltm (no-output (sdm kind memory))))
    (sort (get-base-level-fct ltm)
          #'>)))


