#!/usr/bin/env python

# A theory of PTSD form a declarative memory standpoint

import random as rnd
import numpy as np
import actr
import os
import sys
import string


## ---------------------------------------------------------------- ##
## Object-oriented version (much cleaner)
## ---------------------------------------------------------------- ##

class PTSD_Object:
    
    def chunk_similarity(self, chunk1, chunk2):
        """
        Calculates the similarity between two chunks. Currently,
        similarity is defined as the number of all attributes that
        are identical (same value, same position) between two chunks,
        normalized by the total number of slots.
        """
        v1 = self.vectorize_memory(chunk1)
        v2 = self.vectorize_memory(chunk2)
        if len(v1) == len(v2):
            N = len(v1)
            return np.sum([1 if (v1[j] == v2[j]) else 0 \
                           for j in range(N) ]) / N


class Simulation(PTSD_Object):
    """An object that runs, manages, and stores the results of simulations"""
    def __init__(self, model = "ptsd.lisp",
                 Vs = [1, 5, 10, 15, 20],
                 n = 100):
        self.n = n
        self.Vs = Vs
        self.model = model
        self._currentV = 0
        self.PTEV = 10        # Potentially Traumatic Event Value
        self.PTET = 600 * 30  # Potentially Traumatic Event Time
        self.PTES = 0.0       # Similarity of PTE to other chunks ([0,1] range)
        self.max_time = 50000 # Max duration of a simulation
        self.event_step = 600 # Interval between events to be experienced
        self.counter = 0
        self.num_slots = 6
        self.num_attributes = 6
        self.V_TABLE = {}
        self.TRACE = []
        self.model_params = {}

        # Read-only params
        #self._currentV = 0.0
        #self._slot_values = tuple(x for x in string.ascii_letters[-26:-16])
        #self._traumatic_slot_values = tuple(x for x in string.ascii_letters[-10:])
        #self._slot_names = tuple("SLOT" + "%d" % (x + 1,) for x in range(10))

    # --- READ-ONLY PROPERTIES -------------------------------------- #
        
    @property
    def currentV(self):
        return self._currentV

    @property
    def currentS(self):
        return self._currentS
    
    @property
    def slot_values(self):
        return self._slot_values

    @property
    def traumatic_slot_values(self):
        return self._traumatic_slot_values

    @property
    def slot_names(self):
        return self._slot_names

    # --- READ/WRITE PROPERTIES ------------------------------------- #
    
    @property
    def PTEV(self):
        return self._PTEV

    @PTEV.setter
    def PTEV(self, val):
        if type(val) in [int, float]:
            self._PTEV = [val]
            self._currentV = val
        elif type(val) in [list, tuple]:
            self._PTEV = val
            self._currentV = val[0]
            

    @property
    def PTES(self):
        return self._PTES

    @PTES.setter
    def PTES(self, val):
        if type(val) in [int, float]:
            if val >= 0.0 and val <= 1.0:
                self._PTES = [val]
                self._currentS = val
        elif type(val) in [list, tuple]:
            fval = [x for x in val if x >= 0.0 and x <= 1.0]
            if len(fval) > 0:
                self._PTES = fval
                self._currentS = fval[0]

    @property
    def num_slots(self):
        """Returns the number of slots"""
        return self._num_slots

    @num_slots.setter
    def num_slots(self, val):
        """Sets the number of slots, and generates the internal slots names"""
        self._num_slots = val
        self._slot_names = tuple("SLOT" + "%d" % (x + 1,) for x in range(val))

    @property
    def num_attributes(self):
        """Returns the number of attributes"""
        return self._num_slots

    @num_attributes.setter
    def num_attributes(self, val):
        """
        Sets the number of attributes, and generates the internal set of traumatic
        and non-traumatic admissible values for the slots.
        """
        self._num_slots = val
        self._slot_values = tuple(x for x in string.ascii_lowercase[:val])
        self._traumatic_slot_values = tuple(x for x in string.ascii_lowercase[-val:])
        


    # --- METHODS -------------------------------------------------- #

    def vectorize_memory(self, chunk):
        """Returns a vector representation of a chunk"""
        values = [actr.chunk_slot_value(chunk, slot) for slot in self.slot_names]
        return tuple([x for x in values if x is not None])


    def generate_random_memory(self, traumatic=False):
        """Generates a new memkory with random attributes"""
        template = [False] * self.num_slots
        T = []
        if traumatic:
            n_changes = int(self.num_slots * (1 - self.currentS))
            for j in range(n_changes):
                template[j] = True
            T = ["traumatic", "yes"]
            
        else:
            T = ["traumatic", "no"]
            
        rnd.shuffle(template)

        slots = ['isa', 'memory', 'kind', 'memory']
        
        for j, slot in enumerate(template):
            if slot:
                random_attribute = rnd.choice(self.traumatic_slot_values)
            else:
                random_attribute = rnd.choice(self.slot_values)
            slots += ['slot' + str(j + 1), str(random_attribute)]

        return [slots + T]


    def reset(self):
        """Resets the results of a simulation and gets ready for another"""
        self.counter = 0
        self.V_TABLE = {}
        self.TRACE = []


    def present_new_situation(self, buffer="imaginal"):
        """Creates a new situation for the model and presents to the WHERE buffer"""
        if actr.mp_time() == self.PTET:
            newdef = self.generate_random_memory(traumatic=True)
        else:
            newdef = self.generate_random_memory(traumatic=False)
            
        newchunk = actr.define_chunks(newdef[0])
        actr.set_buffer_chunk(buffer, newchunk[0])


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
                self.V_TABLE[chunk] = self.currentV


    def spreading_activation(self, chunk):
        """Calculates spreading activation from imaginal"""
        source = actr.buffer_chunk("imaginal")
        if len(source) > 0:
            source = source[0]
            if (chunk != source):
                kind1 = actr.chunk_slot_value(source, "KIND")
                kind2 = actr.chunk_slot_value(chunk, "KIND")
                if (kind1.upper() == "MEMORY" and kind2.upper() == "MEMORY"):
                    v1 = self.vectorize_memory(source)
                    v2 = self.vectorize_memory(chunk)
                    sim = self.chunk_similarity(source, chunk)
                    w = actr.get_parameter_value(":imaginal-activation")
                    
                    if w is None:
                        w = 0.0

                return sim * w


    def monitor_retrievals(self, chunk):
        """Keeps track of what is being retrieved and why"""
        v = 0.0  # Emotional load of retrieved memory
        s = 0.0  # Similarity of memory to current situation
        t = 0.0  # Is the memory traumatic or not?

        if chunk is not None and \
           actr.chunk_slot_value(chunk, "kind") == "MEMORY":
            param_values = []
            for param in sorted(self.model_params.keys()):
                param_values.append(self.model_params[param])
                
            source = actr.buffer_chunk("imaginal")[0]
            v = self.V_TABLE[chunk]
            s = self.chunk_similarity(chunk, source)
            if actr.chunk_slot_value(chunk, "traumatic") == "YES":
                t = 1.0
            self.TRACE.append([self.counter, self.currentV, actr.mp_time(), v, t, s] + \
                              param_values)


    def simulate(self):
        """Runs a single simulation"""
 
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

        # Apply the set of provided parameters
        for param, value in self.model_params.items():
            actr.set_parameter_value(param, value)
        
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



    def run(self, verbose = True):
        """Runs the simulations for all cases described in this
        object parameters.

        Keyword arguments:
        verbose --- If True (default), prints progress updates
        """
        for v in self.PTEV:
            for s in self.PTES:
                self._currentV = v
                self._currentS = s
                
                if verbose:
                    print("V = %.2f, S = %.2f: " % (v, s), end="")
                    sys.stdout.flush()
                    
                for j in range(self.n):
                    if verbose and (j % 5) == 0.0:
                        print(".", end="")
                    self.simulate()
                    sys.stdout.flush()

                print("")


    def save_trace(self, fname="trace.txt"):
        """Saves the results of a series of a run of simulations

        Keyword arguments:
        fname --- Name of the file to save the data onto
                  (default is 'trace.txt')
        """
        header = "Run,PTEV,Time,V,Traumatic,Similarity"
        for param in sorted(self.model_params.keys()):
            header += (",%s" % param)
            
        np.savetxt(fname,
                   self.TRACE,
                   delimiter=",",
                   header=header)
