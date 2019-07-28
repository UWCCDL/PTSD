;;; PTSD SIMULATION CODE FOR FAST EXECUTION

(defconstant *letters* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
  "Letters are used as stimulus attributes")

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
   (ptet :accessor ptet
         :initform (* 60 300))
   (max-time :accessor max-time
             :initform 100000)
   (event-step :accessor event-step
               :initform 600)
   (counter :accessor counter
            :initform 0)
   (num-slots :accessor num-slots
              :initform 6)
   (num-attributes :accessor num-attributes
                   :initform 6)
   (current-v :accessor current-v
              :initform 1)
   (current-s :accessor current-s
              :initform 0)
   (v-table :accessor v-table
            :initform (make-hash-table))
   (slot-names :accessor slot-names
               :initform '(slot1 slot2 slot3 slot4 slot5 slot6)) 
   (model-trace :accessor model-trace
                :initform nil)
   (model-params :accessor model-params
                 :initform (make-hash-table))))

(defmethod traumatic-slot-values ((s simulation))
  "Returns the list of attributes used for traumatic memories"
  (subseq (reverse *letters*) 0 (num-slots s))) 


(defmethod slot-values ((s simulation))
  "Returns the list of attributes used for non-traumatic memories"
  (subseq *letters* 0 (num-slots s))) 


(defun make-slot-name (num)
  "Generates a symbol 'SLOT<N>', with N being an integer number"
  (intern (string-upcase (format nil "SLOT~A" num))))


(defmethod generate-random-memory ((s simulation) &optional (traumatic nil))
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

      
(defmethod present-new-situation ((s simulation) &optional (buffer 'imaginal))
  "Presents a new situation to the model"
  (let* ((newdef (generate-random-memory s (= (mp-time)
                                              (ptet s))))
         (newchunk (first (define-chunks-fct (list newdef)))))
    (when (= (mp-time) (ptet s))
      (print newchunk))
    (set-buffer-chunk buffer newchunk)))
    

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
  

(defmethod modified-spreading-activation ((s simulation) chunk
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
    (append entry (list traumatic similarity)))) 

(defmethod save-trace ((s simulation) filename)
  (with-open-file (fle "fast-simulations.txt"
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (dolist (row (model-trace s))
      (format fle "~{~5,f~^, ~}~%" row))))

         
(defmethod simulate ((s simulation))
  (load "~/Documents/Research/PTSD/fast/ptsd-model.lisp")
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
  (setf (model-trace s) nil)

  ;; Run
  (let ((time 0))
    (while (< time (max-time s))
      (schedule-event time #'present-new-situation :params (list s))
      (incf time (event-step s)))))
  
