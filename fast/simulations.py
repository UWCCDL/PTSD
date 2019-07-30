#!/usr/bin/env python

# Meta-code to generate lisp simulations

ACTRDIR="/actr/actr7.x"

TEMPLATE="""
(load "%s/load-act-r.lisp")
(load "ptsd.lisp")
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
(run sim)
(quit)
"""

for w in [0.0, 10, 5, 2.5, 7.5]:
    for bll in [0.1, 0.9, 0.5, 0.3, 0.7]:
        fout=open("simulations_w_%.1f_bll_%.2f.lisp" % (w, bll), "w")
        fout.write(TEMPLATE % (ACTRDIR, w, bll, w, bll))
        fout.flush()
        fout.close()
        
