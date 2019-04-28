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


def present_new_situation(where="imaginal"):
    """Creates a new situation for the model and presents to the WHERE buffer"""
    newdef = random_memory_generator()[0]
    newchunk = actr.define_chunks(newdef)[0]
    actr.set_buffer_chunk(where, newchunk)
    actr.dm()

    
def v_offset(chunk):
    """Calculates the V-term for the given chunk"""
    np.log(1 + actr.chunk_slot_value(chunk, "V"))


SLOTS = tuple("SLOT" + "%d" % (x + 1,) for x in range(10))


def vectorize_memory(chunk):
    """Returns a vector representation of a chunk"""
    values = [actr.chunk_slot_value(chunk, slot) for slot in SLOTS]
    return tuple([x for x in values if x is not None])
    
    
def sji_calculation(chunk1, chunk2):
    """
Calculates the association between two chunks
(association defined as similarity between chunks)
    """
    if (chunk1 != chunk2):
        kind1 = actr.chunk_slot_value(chunk1, "KIND")
        kind2 = actr.chunk_slot_value(chunk2, "KIND")
        print(">>> From %s to %s" % (chunk1, chunk2))
        print(">>> Kinds (%s, %s) " % (kind1, kind2))
        
        if (kind1.upper() == "MEMORY" and kind2.upper() == "MEMORY"):
            v1 = vectorize_memory(chunk1)
            v2 = vectorize_memory(chunk2)
            if (len(v1) == len(v2)):
                N = len(v1)
                sim = np.sum([1 if (v1[j] == v2[j]) else 0 for j in range(N) ])/N
                
                print(">>> S(%s, %s) = %.3f " % (v1, v2, sim))
                return sim


def monitor_retrievals(chunk):
    """Keeps track of retrievals"""
    if chunk is not None:
        v = actr.chunk_slot_value(chunk, "V")
        if v is not None:
            print("---> %.3f" % v)
            return v

TABLE = {}
        
def keep_table(chunk):
    global TABLE
    TABLE[chunk] = rnd.uniform(0,2)


def simulation(model="ptsd.lisp", max_time=100, event_step=20):
    #actr.reset()

    # Add commands and hooks
    actr.add_command("v_offset", v_offset,
                     "Extra term in activation")
    actr.add_command("sji_calculation", sji_calculation,
                     "Overrides normal strength of association")
    actr.add_command("monitor_retrievals", monitor_retrievals,
                     "Monotors what is being retrieved")

    actr.add_command("next", present_new_situation,
                     "Presents a new situation")

    ## Experimental
    actr.add_command("keep_table", keep_table)
    
    # Makes sure we are loading the current model from
    # the current directory
    curr_dir = os.path.dirname(os.path.realpath(__file__))
    actr.load_act_r_model(os.path.join(curr_dir, model))
    
    # Run a life simulation

    event_time = 0.0
    
    while actr.mp_time() < max_time:
        actr.schedule_event(event_time, "next")
        event_time += event_step
        actr.run(event_step) # No need to run beyond the event step

    # Clean-up
    
    actr.remove_command("next")
    actr.remove_command("v_offset")
    actr.remove_command("sji_calculation")
    actr.remove_command("monitor_retrievals")
