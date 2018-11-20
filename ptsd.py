#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

# memories can be just inserted into a list.
# memories are contentless.
# New memories are created at a fixed rate.
# Memories are created within a context made of individual
# cues q1... qN.
# Only a subset M < N of cues is available at any point in time.
# Cues change over time; at any point in time, cues are
# slowly changed.
# The number of cues that are changed is drawn from a gamma
# distribution.

# Learning
# --------
# When created, memories are simply put into the list.
# Every time a meory 



NAMES = {}

def gen_name(prefix):
    """Generated names"""
    if prefix in NAMES:

class MemoryTrace():
    """A memory trace in ACT-R"""
    def __init__(self, time):
        self.name = gen_name()
        self.history = [time]
        self.impact = []
        

    def base_activation(self, t, decay):
        traces = [(time - t)**decay for time in self.history]

    def spreading_activation(self, context):
        
