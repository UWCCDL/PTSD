;;; Lisp model for PTSD

(p retrieve
   =goal>
     processed no
   ?retrieval>
     Buffer empty
	 State free
==>
   +retrieval>
)

(p elaborate
   =goal>
     processed no
   ?retrieval>
     Buffer full
   State free
==>
   =goal>
      processed yes
   -retrieval>
   -visual>
   -imaginal>
)

(p cant-retrieve
   =goal>
     processed no
   ?retrieval>
     Buffer empty
   State error
==>
   =goal>
     processed yes
   -retrieval>
   -visual>
   -imaginal>
)
