#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

import random as rnd
import numpy as np
import actr
import os

# Variables
life_time = 10 #number of time points in simulated life time
num = 2 #number of chunks per memory at a time point
slots = 3 #slots per chunk


def random_memory_generator(mem_num=None,
                            num_chunks=num,
                            num_slots=slots,
                            v_val=None):
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


        if v_val is None:
            v = rnd.uniform(0,2)
        else:
            v = v_val
            
        V = ['V', v]   # The emotional value
        memory += [name + ['kind', 'memory'] + slots + V]
        memories += memory
    return memories

def add_memories(mem_num, num, num_slots, v_val):
    """Adds NUM memories to the model"""
    memories = random_memory_generator(mem_num, num, num_slots, v_val)
    for m in memories:
        actr.add_dm(m)


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

def v_offset(chunk):
    """Calculates the V-term for the given chunk"""
    np.log(1 + actr.chunk_slot_value(chunk, "V"))

    
def sji_calculation(chunk1, chunk2):
    """
Calculates the association between two chunks
(association defined as similarity
    """
    return 0.0


def monitor_retrievals(chunk):
    """Keeps track of retrievals"""
    if chunk is not None:
        v = actr.chunk_slot_value(chunk, "V")
        if v is not None:
            print("---> %.3f" % v)
            return v
            
    
def simulation(model="ptsd.lisp", max_time=10):
    #actr.reset()

    # Add commands and hooks
    actr.add_command("v_offset", v_offset,
                     "Extra term in activation")
    actr.add_command("sji_calculation", sji_calculation,
                     "Overrides normal strength of association")
    actr.add_command("monitor_retrievals", monitor_retrievals,
                     "Monotors what is being retrieved")
    
    # Makes sure we are loading the current model from
    # the current directory
    curr_dir = os.path.dirname(os.path.realpath(__file__))
    actr.load_act_r_model(os.path.join(curr_dir, "ptsd.lisp"))
    
    # Run a life simulation

    while actr.mp_time() < max_time:
        present_new_situation()
        actr.run(100)

    actr.remove_command("v_offset")
    actr.remove_command("sji_calculation")
    actr.remove_command("monitor_retrievals")
