#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

import random
import actr

def random_memory_generator(mem_num, num_chunks, num_slots):
    memories = []
    name = []
    attributes = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    a=len(attributes)
    for i in range(num_chunks):
        memory=[]
        slots=[]
        name=['memory' + str(mem_num + 1) + "_" + str(i + 1)]
        for j in range(num_slots):
            random_attribute = choice(attributes)
            slots+=['slot' + str(j + 1), str(random_attribute)]
        memory+=[name+slots]
        memories+=memory

    return memories

def add_memories(mem_num, num, num_slots):
    """Adds NUM memories to the model"""
    memories = random_memory_generator(mem_num, num, num_slots)
    for m in memories:
        actr.add_dm(m)

#actr.add_dm(["memory3", "f1", "a", "f2", "b"])

#push random memory set into act-r over the "life time". 1=10 years
#traumatic event occurs at a random time over the range of life_time

life_time=10
event_time=randrange(0,life_time,1)

#add memories from that decade to act-r

for t in range(life_time):
  if t==event_time:
    #this memory is assigned a high V
    add_memories(t, 1, 5)
  else:
    add_memories(t, 1, 5)
