README

Introduction
============

These are instructions on how to run the second set of simulations of
the PTSD model.

Software and files needed
-------------------------

To run the simulations, you need the following software:

  1. ACT-R, version 7.14 (build 2969 was used)
  2. A Lisp interpreter for ACT-R (SBCL 1.5.8 was used)
  3. Python (Python 3.6 was used)

The following files:

  1. ptsd.lisp, the lisp-based simulation manager
  2. ptsd-model.lisp, the PTSD model
  3. generate_hyperparameter_code.py, the Python-based code that
     generates the Lisp code to simulate a single hyperpoint in
     parameter space.

In addition, the simulation parameters are contained in the following
files:

  1. attributes.values, number of attributes for chunks
  2. bll.values. values for decay d (BLL)
  3. ef.values, values for event frequency
  4. rf.values, values for rumination frequency
  5. ptes.values, values for PTE similiarity
  6. ptev.values, values of PTE intensity V
  7. rt.values, values for retrieval threshold
  8. slots.values, values for number of slots in chunks
  9. w.values, values for W (working memory)
  10. n.values, values for the number of simulations N

How simulations were run
------------------------

This is the how the simulations go

  parallel --jobs 10 \\
           --colsep ' ' \\
           -a attributes.values \\
           -a bll.values \\
           -a ef.values \\
           -a rf.values \\
           -a ptes.values \\
           -a ptev.values \\
           -a rt.values \\
           -a slots.values \\
           -a w.values \\
	   -a n.values \\
           ./generate_hyperparameter_code.py


then

 
  ls bll*.lisp | parallel --jobs 10  \\
                          -a -       \\
                          sbcl --load
