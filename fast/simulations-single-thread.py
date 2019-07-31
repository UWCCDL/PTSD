#!/usr/bin/env python

# Meta-code to generate lisp simulations in single thread (no
# parallelism allowed)

import os

ACTRDIR="/projects/actr/actr7.x"
CURRENTDIR=os.path.dirname(os.path.realpath(__file__))
TEMPLATE="""
(setf sim (make-instance 'simulation))
(setf (ptev sim) 
      '(1 20 10 5 15))
(setf (ptes sim)
      '(0 1 0.5 0.25 0.75))
(let ((ht (make-hash-table)))
  (setf (gethash :imaginal-activation ht) %.1f)
  (setf (gethash :bll ht) %.2f)
  (setf (model-params sim) ht))

(setf (logfile sim) "simulations_w=%.1f_bll=%.2f.txt") 
(handler-case
    (run-simulations sim)
  (t (c)
    (format t "Got an exception: ~a~%" c)
    (save-trace sim)))
"""

fout=open("simulations-single-thread.lisp", "w")
fout.write('(load "%s/load-act-r.lisp")' % ACTRDIR)
fout.write('(load "%s/ptsd.lisp")' % CURRENTDIR)
fout.write('(defparameter sim nil)')
for w in [0.0, 10, 5, 2.5, 7.5]:
    for bll in [0.1, 0.9, 0.5, 0.3, 0.7]:
        fout.write(TEMPLATE % (w, bll, w, bll))
        fout.flush()
fout.write("(quit)")
fout.flush()
fout.close()
