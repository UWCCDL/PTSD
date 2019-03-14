#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

import numpy as np
from random import choice
import actr

def random_memory_generator(num_chunks, num_slots):
    memories = []
    name = []
    attributes = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    a=len(attributes)
    for i in range(num_chunks):
        memory=[]
        slots=[]
        name=['memory' + str(i + 1)]
        for j in range(num_slots):
            random_attribute = choice(attributes)
            slots+=['slot' + str(j + 1), str(random_attribute)]
        memory+=[name+slots]
        memories+=memory

    return memories

def add_memories(num, num_slots):
    """Adds NUM memories to the model"""
    memories = random_memory_generator(num, num_slots)
    for m in memories:
        actr.add_dm(m)

#actr.add_dm(["memory3", "f1", "a", "f2", "b"])    


