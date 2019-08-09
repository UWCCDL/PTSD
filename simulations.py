#!/usr/bin/env python

import ptsd

#for ia in [0, 2, 4, 6, 8, 10]:
for ia in [0, 5, 10]:
#    for blc in [0.3, 0.4, 0.5, 0.6, 0.7]:
    for blc in [0.3,  0.5, 0.7]:
        s = ptsd.Simulation()
        s.PTEV = [1, 5, 10, 15, 20]
        s.PTES = [0, 0.5, 1]
        s.model_params = {":imaginal-activation" : ia,
                          ":blc" : blc}
        s.n = 100
        s.max_time = 100000
        s.run()
        s.save_trace("trace_ia=%d_blc=%.2f.txt" % (ia, blc))
