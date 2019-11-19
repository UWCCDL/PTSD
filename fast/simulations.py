#!/usr/bin/env python

# Meta-code to generate lisp simulations

import os

SLEEP_WORKAROUND = False

ACTRDIR="/projects/actr/actr7.x"

CURRENTDIR=os.path.dirname(os.path.realpath(__file__))

TEMPLATE="""
(load "%s/load-act-r.lisp")
(load "%s/ptsd.lisp")
(setf sim (make-instance 'simulation))
(let ((ht (make-hash-table)))
  (setf (gethash :imaginal-activation ht) %.1f)
  (setf (gethash :bll ht) %.2f)
  (setf (gethash :rt ht) %.2f)
  (setf (model-params sim) ht))

(setf (logfile sim) "simulations_w=%.1f_bll=%.2f_rt=%.2f.txt") 
(run-simulations sim)
(quit)
"""

ii = 0

for w in [2, 10, 4, 6, 8]:
    for bll in [0.25, 0.5, 0.75]:
        for rt in [0, 3, 1, 2]:
            if SLEEP_WORKAROUND:
                sleep_time =  1 + (30 * ii)
                fout=open("f%02d_simulations_w_%.1f_bll_%.2f_rt_%.2f.lisp" %
                          (ii, w, bll, rt), "w")
                ii += 1

            fout.write("(sleep %d)" % sleep_time)
            fout.write(TEMPLATE % (ACTRDIR, CURRENTDIR, w, bll, rt, w, bll, rt))
            fout.flush()
            fout.close()
        
