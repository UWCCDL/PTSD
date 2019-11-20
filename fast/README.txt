README

Simulations 2
=============

Introduction
------------
"Simulations2" is a folder containing the second set of large-scale
simulations for the PTSD model. The simulations were run with an
improved and more robuts Lisp code, and designed to take full
advantage of the GNU Parallel program.

Software Needed to Run the Simulations
--------------------------------------
The simulations were run using the following software:

  * sbcl, version 1.5.8
  * act-r, version 7.14 (build 2969)
  * GNU parallel

And using the following software files:

  * ptsd.lisp, the model Lisp interface and simulation manager
  * ptsd-model.lisp, the ACT-R model of PTSD
  * generate_hyperparameter_code.py, a Python command-line program
    that generates Lisp code to simulate specific points in parameter space. 

Re-Running the Simulations
--------------------------

Three commands were run. The first one generates the necessary lisp
files:

parallel --jobs 10            \\
         --colsep ' '         \\
         -a attributes.values \\
         -a bll.values        \\
         -a ef.values         \\
         -a rf.values         \\
         -a ptes.values       \\
         -a ptev.values       \\
         -a rt.values         \\
         -a slots.values      \\
         -a w.values          \\
         ./generate_hyperparameter_code.py

This will generate 2304 files, corresponding to as many points in
parameter space.

The second command generates a list of sbcl load parameters:

  

The third command runs SBCL on all the files:

ls bll*.lisp | parallel --jobs 10    \\
                        -a -         \\
                        sbcl --load
