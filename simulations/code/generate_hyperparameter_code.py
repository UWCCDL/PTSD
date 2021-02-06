#!/usr/bin/env python

import sys
import argparse
import os

ACTR_DIR = "/projects/actr/actr7.x"

CURRENT_DIR = os.path.dirname(os.path.realpath(__file__))

REPLACEMENTS = {" " : "_", ":" : "",
                "'" : "", "(" : "",
                ")" : "", '"' : ""}

PARAMETERS = {"n" : "n",
              "ptev" : "ptev",
              "ptes" : "ptes",
              "gamma" : "gamma",
              "ef" : "event-frequency",
              "rf" : "rumination-frequency",
              "before" : "num-days-before",
              "after" : "num-days-after",
              "attributes" : "num-attributes",
              "slots" : "num-slots",
              ":bll" : ":bll",
              ":w" : ":imaginal-activation",
              ":blc" : ":blc",
              ":rt" : ":rt",
              ":ans" : ":ans"
              }

def actr_param(name):
    return name.startswith(":")

def unique_file_name(dic):
    names = [x for x in dic.keys() if dic[x] is not None]

    fname = ""
    for item in sorted(names): #.sort():
        fname += "%s-%s_" % (item, dic[item])

    for old, new in REPLACEMENTS.items():
        fname = fname.replace(old, new)

    return fname[:-1]

def write_code(params):
    with open(unique_file_name(params) + ".lisp", 'w') as out:
        out.write("""(ql:quickload "split-sequence")\n""")
        out.write("""(push :actr-fast *features*)\n""")
        out.write("""(load "%s/load-act-r.lisp")\n""" % ACTR_DIR)
        out.write("""(load "%s/ptsd.lisp")\n""" % CURRENT_DIR)
        out.write("""(let ((sim (make-instance 'simulation)))\n""")

        for p in [x for x in params.keys() if not actr_param(x) and params[x] != None]:
            out.write("""  (setf (%s sim) %s)\n""" % (PARAMETERS[p],
                                                      params[p].replace('"', "")))

        for p in [x for x in params.keys() if actr_param(x) and params[x] != None]:
            out.write("""  (setf (gethash %s (model-params sim)) %s)\n""" % (PARAMETERS[p], params[p]))

        out.write("""  (setf (logfile sim) "%s.csv")\n""" % unique_file_name(params))
        out.write("""  (run-simulations sim))\n""")
                    
        out.write("(quit)\n")
                      

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    # Basic params
    parser.add_argument('--n', help='Number of simulations in each hyperpoint')
    parser.add_argument('--ptev', help='Value of V for PTE')
    parser.add_argument('--ptes', help='Similarity value for PTE')
    parser.add_argument('--ef', help='Expected frequency of events')
    parser.add_argument('--rf', help='Expected frequency of events')
    parser.add_argument('--before', help='Number of days before PTE')
    parser.add_argument('--after', help='Number of days after PTE')
    parser.add_argument('--attributes', help='Number of attributes for each episode')
    parser.add_argument('--slots', help='Number of slots for each episode')
    parser.add_argument('--gamma', help='Value of V decay')
    
    # ACT-R PARAMS
    parser.add_argument('--:w', help='Value of :imaginal-activation')
    parser.add_argument('--:bll', help='Decay value')
    parser.add_argument('--:blc', help='Base-level constant')
    parser.add_argument('--:rt', help='Retrieval threshold constant')
    parser.add_argument('--:ans', help='Noise')
    
    args = vars(parser.parse_args())
    write_code(args)

