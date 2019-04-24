#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

import random as rnd
import actr

# Variables
life_time = 10 #number of time points in simulated life time
num = 2 #number of chunks per memory at a time point
slots = 3 #slots per chunk


def random_memory_generator(mem_num=None,
                            num_chunks=num,
                            num_slots=slots,
                            v_val=rnd.uniform(0,2)):
    """Generates random chunks (slot/attribute pairs)"""
    memories = []
    name = []
    attributes = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    a = len(attributes)
    for i in range(num_chunks):
        memory = []
        slots = []
        name = []
        if mem_num is not None: 
            name = ['memory' + str(mem_num + 1) + "_" + str(i + 1)]
            
        for j in range(num_slots):
            random_attribute = rnd.choice(attributes)
            slots+=['slot' + str(j + 1), str(random_attribute)]
            
        V = ['V', v_val]   # The emotional value
        memory += [name + slots + V]
        memories += memory
    return memories

def add_memories(mem_num, num, num_slots, v_val):
    """Adds NUM memories to the model"""
    memories = random_memory_generator(mem_num, num, num_slots, v_val)
    for m in memories:
        actr.add_dm(m)

#actr.add_dm(["memory3", "f1", "a", "f2", "b"])

#push random memories from the simulated life time to act-r


#traumatic event occurs at a random time point in simulated life time
event_time=rnd.randrange(0, life_time, 1)

def life(life_time = life_time):
    """Abstract life function"""
    for t in range(life_time):
        if t == event_time:
            #all memories at this time point are assigned a high V
            v_val=10
            add_memories(t, num, slots, v_val)
        else:
            #all other memories assigned a random low V
            v_val=rnd.uniform(0,2)
    add_memories(t, num, slots, v_val)

    
def present_new_situation():
    """Creates a new situation for the model"""
    newdef = random_memory_generator()[0]
    newchunk = actr.define_chunks(newdef)[0]
    actr.set_buffer_chunk("imaginal", newchunk)


def simulation(max_time=10):
    actr.load_act_r_model("ptsd.lisp")
    while actr.mp_time() < max_time:
        present_new_situation()
        actr.run(100)

    
    
