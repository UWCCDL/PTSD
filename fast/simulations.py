#!/usr/bin/env python

# Meta-code to generate lisp simulations

import os

ACTRDIR="/projects/actr/actr7.x"
CURRENTDIR=os.path.dirname(os.path.realpath(__file__))
TEMPLATE="""
(load "%s/load-act-r.lisp")
(load "%s/ptsd.lisp")
(setf sim (make-instance 'simulation))
(setf (ptev sim) 
      '(1 20 10 5 15))
(setf (ptes sim)
      '(0.2 0.8 0.4 0.6))
(let ((ht (make-hash-table)))
  (setf (gethash :imaginal-activation ht) %.1f)
  (setf (gethash :bll ht) %.2f)
  (setf (gethash :rt ht) %.2f)
  (setf (model-params sim) ht))

(setf (logfile sim) "simulations_w=%.1f_bll=%.2f_rt=%.2f.txt") 
(run-simulations sim)
(quit)
"""

for w in [2, 10, 4, 6, 8]:
    for bll in [0.25, 1, 0.5, 0.75]:
        for rt in [0, 3, 1, 2]:
            fout=open("simulations_w_%.1f_bll_%.2f_rt_%.2f.lisp" % (w, bll, rt), "w")
            fout.write(TEMPLATE % (ACTRDIR, CURRENTDIR, w, bll, rt, w, bll, rt))
            fout.flush()
            fout.close()
        
