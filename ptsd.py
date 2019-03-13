#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

import numpy as np

def random_memory_generator(num_chunks, num_slots):
    memories=[]
    name=[]
    attributes=['a', 'b', 'c', 'd', 'e', 'f', 'g']
    a=len(attributes)
    for i in range(num_chunks):
        memory=[]
        slots=[]
        name=['memory'+str(i)]
        for j in range(num_slots):
            random_num=int(round(np.random.uniform(0,a)))
            random_attribute=attributes[random_num]
            slots+=['slot'+str(j), str(random_attribute)]
        memory+=[name+slots]
        memories+=memory

    return memories

def add_memories(num, num_slots):
    """Adds NUM memories to the model"""
    memories = random_memory_generator(num, num_chunks)
    for m in memories:
        actr.add_dm(m)

actr.add_dm(["memory3", "f1", "a", "f2", "b"])    


