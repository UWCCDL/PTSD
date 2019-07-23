;;; PTSD SIMULATION CODE FOR FAST EXECUTION

(defconstant *letters* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

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
   (model-trace :accessor model-trace
                :initform nil)
   (model-params :accessor model-params
                 :initform nil)))

(defmethod traumatic-slot-values ((s simulation))
  (subseq (reverse *letters*) 0 (num-slots s))) 

(defmethod slot-values ((s simulation))
  (subseq *letters* 0 (num-slots s))) 

(defun make-slot-name (num)
  (intern (string-upcase (format nil "SLOT~A" num))))

(defmethod generate-random-memory ((s simulation) &optional (traumatic nil))
  (let ((template (make-list (num-slots s) :initial-element nil))
        (tslot '(traumatic no))
        (slots '(isa memory kind memory)))
    (when traumatic
      (let ((num-changes (round (* (num-slots s)
                                   (- 1 (current-s s))))))
        (dotimes (j num-changes)
          (setf (nth j template) t)))
      
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
  (let* ((newdef (generate-random-memory s (equal (mp-time)
                                                  (ptet s))))
         (newchunk (first (define-chunks-fct (list newdef)))))
    (set-buffer-chunk buffer newchunk)))
    

(defmethod chunk-v-term ((s simulation) chunk)
  (log (gethash chunk (v-table s) 1.0))) 


(defmethod add-chunk ((s simulation) chunk)
  (if (equalp (chunk-slot-value 'traumatic chunk) 'no)
      (setf (gethash chunk 
