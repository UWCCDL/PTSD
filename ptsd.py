#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

import random as rnd
import numpy as np
import actr
import os
import sys
import string

# Variables
life_time = 10 #number of time points in simulated life time
num = 2 #number of chunks per memory at a time point
slots = 5 #slots per chunk
#num_traumatic_attributes=5 #number of unique traumatic attributes within traumatic memory

PTE_TIME = 600*30
TRACE = []
TRAUMATIC_V = 10
COUNTER = 1

def random_memory_generator(mem_num=None,
                            num_chunks=num,
                            num_slots=slots,
                            traumatic=False):
    """Generates random chunks (slot/attribute pairs)"""
    memories = []
    name = []

    for i in range(num_chunks):
        memory = []
        slots = []
        name = []
        if mem_num is not None:
            name = ['memory' + str(mem_num + 1) + "_" + str(i + 1)]

        for j in range(num_slots):
            attributes = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
            random_attribute = rnd.choice(attributes)
            slots+=['slot' + str(j + 1), str(random_attribute)]

        T = ['traumatic', 'no']

        if traumatic:
            """ Unique memory is created with traumatic_attributes """
            for j in range(num_slots):
                traumatic_attributes = ['z', 'y', 'x', 'w', 'u', 't', 's']
                random_attribute = rnd.choice(traumatic_attributes)
                slots+=['slot' + str(j + 1), str(random_attribute)]

            #for j in range(num_slots-num_traumatic_attributes):
            #    attributes=['a', 'b', 'c', 'd', 'e', 'f', 'g']
            #    random_attribute=rnd.choice(attributes)

            #    slots+=['slot'+str(j+1+num_traumatic_attributes), str(random_attribute)]

            T = ['traumatic', 'yes']

        memory += [name + ['isa', 'memory', 'kind', 'memory'] + slots + T]# + V]
        memories += memory
    return memories


def add_memories(mem_num, num, num_slots, v_val):
    """Adds NUM memories to the model"""
    memories = random_memory_generator(mem_num, num, num_slots, v_val)
    for m in memories:
        actr.add_dm(m)


#traumatic event occurs at a random time point in simulated life time
#event_time=rnd.randrange(0, life_time, 1)


def present_new_situation(where="imaginal"):
    """Creates a new situation for the model and presents to the WHERE buffer"""
    global PTE_TIME
    newdef = random_memory_generator()[0]
    if actr.mp_time() == PTE_TIME:
        #print("**** PTE ****")
        newdef = random_memory_generator(traumatic=True)[0]

    newchunk = actr.define_chunks(newdef)[0]
    actr.set_buffer_chunk(where, newchunk)


def v_offset(chunk):
    """Calculates the V-term for the given chunk"""
    global TABLE
    if chunk in TABLE.keys():
        #print("Adding %.3f" % TABLE[chunk])
        return np.log(1 + TABLE[chunk])
    else:
        return 0.0


SLOTS = tuple("SLOT" + "%d" % (x + 1,) for x in range(10))


def vectorize_memory(chunk):
    """Returns a vector representation of a chunk"""
    values = [actr.chunk_slot_value(chunk, slot) for slot in SLOTS]
    return tuple([x for x in values if x is not None])

def chunk_similarity(chunk1, chunk2):
    v1 = vectorize_memory(chunk1)
    v2 = vectorize_memory(chunk2)
    sim = 0.0
    if (len(v1) == len(v2)):
        N = len(v1)
        return np.sum([1 if (v1[j] == v2[j]) else 0 for j in range(N) ])/N


def spreading_function(chunk):
    """Calculates spreading activation from imaginal"""
    source = actr.buffer_chunk("imaginal")
    if len(source) > 0:
        source = source[0]
    if (chunk != source):
        kind1 = actr.chunk_slot_value(source, "KIND")
        kind2 = actr.chunk_slot_value(chunk, "KIND")

        if (kind1.upper() == "MEMORY" and kind2.upper() == "MEMORY"):
            v1 = vectorize_memory(source)
            v2 = vectorize_memory(chunk)
            sim = chunk_similarity(source, chunk)
            w = actr.get_parameter_value(":imaginal-activation")

            if w is None:
                w = 0.0

            return sim * w


def monitor_retrievals(chunk):
    """Keeps track of retrievals"""
    global TABLE
    global TRACE
    global COUNTER
    global TRAUMATIC_V
    v = 0.0
    s = 0.0
    if chunk is not None and actr.chunk_slot_value(chunk, "kind") == "MEMORY":
        source = actr.buffer_chunk("imaginal")[0]
        v = TABLE[chunk]
        s = chunk_similarity(chunk, source)
    TRACE.append([COUNTER, TRAUMATIC_V, actr.mp_time(), v, s])


TABLE = {}

def keep_table(chunk):
    global TABLE
    global TRAUMATIC_V
    if (actr.chunk_slot_value(chunk, "kind") == "MEMORY"):
        if actr.chunk_slot_value(chunk, "traumatic") == "NO":
            TABLE[chunk] = rnd.uniform(0,2)
        elif actr.chunk_slot_value(chunk, "traumatic") == "YES":
            TABLE[chunk] = TRAUMATIC_V


def simulation(model="ptsd.lisp", max_time=50000, event_step=600):
    #actr.reset()

    #global TABLE
    TABLE = {} # Reset memory

    # Add commands and hooks
    actr.add_command("v_offset", v_offset,
                     "Extra term in activation")

    actr.add_command("spreading", spreading_function,
                     "Overrides normal spreading activation algorithm")

    actr.add_command("monitor_retrievals", monitor_retrievals,
                     "Monotors what is being retrieved")

    actr.add_command("next", present_new_situation,
                     "Presents a new situation")

    actr.add_command("keep_table", keep_table)

    # Makes sure we are loading the current model from
    # the current directory
    curr_dir = os.path.dirname(os.path.realpath(__file__))
    actr.load_act_r_model(os.path.join(curr_dir, model))

    actr.set_parameter_value(":V", False)
    actr.set_parameter_value(":cmdt", False)

    # Run a life simulation

    event_time = 0.0

    while actr.mp_time() < max_time:
        actr.schedule_event(event_time, "next")
        event_time += event_step
        actr.run(event_step) # No need to run beyond the event step

    # Clean-up

    actr.remove_command("next")
    actr.remove_command("v_offset")
    actr.remove_command("spreading")
    actr.remove_command("monitor_retrievals")
    actr.remove_command("keep_table")
    #actr.resume_output()

def meta(V=[2, 4, 6, 8, 10, 12, 14], n=20, fname="sims.txt"):
    """"Simulates a lot!!"""
    global COUNTER
    global TABLE
    global TRAUMATIC_V
    for v in V:
        print("V = %.2f: " % v)#, end="")
        for j in range(n):
            if (j % 5) == 0.0:
                print(".")#, end="")
            TABLE = {}
            TRAUMATIC_V = v
            simulation(max_time=40000)
            COUNTER += 1
            sys.stdout.flush()
        print("")


    np.savetxt(fname, TRACE,
               delimiter=",",
               header="Run,V_Traumatic,Time,V,Similarity")

## ---------------------------------------------------------------- ##
## Object-oriented version (much cleaner)
## ---------------------------------------------------------------- ##

class PTSD_Object:
    SLOT_VALUES = tuple(x for x in string.ascii_letters[-26:-16])
    TRAUMATIC_SLOT_VALUES = tuple(x for x in string.ascii_letters[-10:])
    SLOTS_NAMES = tuple("SLOT" + "%d" % (x + 1,) for x in range(10))


    def vectorize_memory(self, chunk):
        """Returns a vector representation of a chunk"""
        values = [actr.chunk_slot_value(chunk, slot) for slot in self.SLOTS]
        return tuple([x for x in values if x is not None])


    def chunk_similarity(self, chunk1, chunk2):
        """
Calculates the similarity between two chunks. Currently, similarity i
is defined as the number of all attributes that are identical (same
value, same position) normalized by the total number of slots.
        """
        v1 = vectorize_memory(chunk1)
        v2 = vectorize_memory(chunk2)
        if (len(v1) == len(v2)):
            N = len(v1)
            return np.sum([1 if (v1[j] == v2[j]) else 0 for j in range(N) ])/N


class Simulation(PTSD_Object):
    """An object that runs, manages, and stores simulations"""
    def __init__(self, model = "ptsd.lisp",
                 Vs = [1, 5, 10, 15, 20],
                 n = 100):
        self.n = n
        self.Vs = Vs
        self.model = model
        self.PTEV = 10        # Peri-Traumatic Event Value
        self.PTET = 600 * 30  # Peri-Traumatic Event Time
        self.max_time = 50000
        self.event_step = 600 # Interval between events to be experienced
        self.counter = 0
        self.V_TABLE = {}
        self.TRACE = []


    def present_new_situation(self, where="imaginal"):
        """Creates a new situation for the model and presents to the WHERE buffer"""
        if actr.mp_time() == self.PTET:
            newdef = random_memory_generator(traumatic=True)[0]
        else:
            newdef = random_memory_generator(traumatic=False)[0]

        newchunk = actr.define_chunks(newdef)[0]
        actr.set_buffer_chunk(where, newchunk)


    def chunk_v_term(self, chunk):
        """Calculates the V-term for the given chunk"""
        if chunk in self.V_TABLE.keys():
            return 0.001 + np.log(self.V_TABLE[chunk])
        else:
            return 0.0


    def add_chunk(self, chunk):
        """Adds a chunk to the V Table and generates a V value for it"""
        if (actr.chunk_slot_value(chunk, "kind") == "MEMORY"):
            if actr.chunk_slot_value(chunk, "traumatic") == "NO":
                self.V_TABLE[chunk] = rnd.uniform(0,2)
            elif actr.chunk_slot_value(chunk, "traumatic") == "YES":
                self.V_TABLE[chunk] = self.PTEV


    def spreading_activation(self, chunk):
        """Calculates spreading activation from imaginal"""
        source = actr.buffer_chunk("imaginal")
        if len(source) > 0:
            source = source[0]
            if (chunk != source):
                kind1 = actr.chunk_slot_value(source, "KIND")
                kind2 = actr.chunk_slot_value(chunk, "KIND")

                if (kind1.upper() == "MEMORY" and kind2.upper() == "MEMORY"):
                    v1 = vectorize_memory(source)
                    v2 = vectorize_memory(chunk)
                    sim = self.chunk_similarity(source, chunk)
                    w = actr.get_parameter_value(":imaginal-activation")

                    if w is None:
                        w = 0.0

                return sim * w


    def monitor_retrievals(self, chunk):
        """Keeps track of what is being retrieved and why"""
        v = 0.0
        s = 0.0

        if chunk is not None and \
           actr.chunk_slot_value(chunk, "kind") == "MEMORY":
            source = actr.buffer_chunk("imaginal")[0]
            v = self.V_TABLE[chunk]
            s = self.chunk_similarity(chunk, source)
            self.TRACE.append([self.counter, self.PTEV, actr.mp_time(), v, s])


    def simulation(self):
        #actr.reset()

        # Add commands and hooks
        actr.add_command("v_offset", self.chunk_v_term,
                         "Extra term in activation")

        actr.add_command("spreading", self.spreading_activation,
                         "Overrides normal spreading activation algorithm")

        actr.add_command("monitor_retrievals", self.monitor_retrievals,
                         "Monitors what is being retrieved")

        actr.add_command("next", self.present_new_situation,
                         "Presents a new situation")

        actr.add_command("keep_table", self.add_chunk)

        # Makes sure we are loading the current model from
        # the current directory
        curr_dir = os.path.dirname(os.path.realpath(__file__))
        actr.load_act_r_model(os.path.join(curr_dir, self.model))

        actr.set_parameter_value(":V", False)
        actr.set_parameter_value(":cmdt", False)

        # Run a life simulation

        event_time = 0.0

        while actr.mp_time() < self.max_time:
            actr.schedule_event(event_time, "next")
            event_time += self.event_step
            actr.run(self.event_step) # No need to run beyond the event step

        # Clean-up

        actr.remove_command("next")
        actr.remove_command("v_offset")
        actr.remove_command("spreading")
        actr.remove_command("keep_table")
        actr.remove_command("monitor_retrievals")

        # Update counter

        self.counter += 1
