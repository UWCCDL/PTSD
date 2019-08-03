# A simple ACT-R model of intrusive memories in PTSD 

## The "Fast" version of the PTSD code

This folder contains a "fast" implementation of the Python PTSD code
found in this main repository. The fast implementation is needed
because the Python code can run incredbly slow. Most of the addition
time is spent in coordinated JSON RPC calls through TCP/IP protocol
between the ACT-R core and the Python client.

To obviate this problem, the Python code was translated into Lisp, so
that it can run faster and be integrated directly with ACT-R. The
general priciples are as such:

  1. The `pstd.lisp` code mirrors the classes, methods, and structures
  of the `ptsd.py` file, including a high-level `simulation` class
  that manages simulations.

  2. All the original Python commands that track the model execution
  have been translated into equivalent ACT-R "hooks" Lisp functions.  

  3. All the commands setting have been removed from the
  `ptsd-model.lisp` file, and are instead set directly through Lisp
  code at the beginnig of every simulation (_Note_: this is clearly
  where the Lisp code diverges from the Python code)

In general, only two minor adjustments are 

# Compatibility

The "fast" Lisp code is __entirely incompatible_ with the Python
code. You can use one of the either, but not both. In fact, the Lisp
code assumes that no JSON RPC system even exists, and replaces all of
the command calls with Lisp-level hook functions.

# Usage

## Object-oriented code

The recommended way to use the code is to use the new, object-oriented
version of the simulations. In this new version, every simulation is
encapsulated as a `Simulation` object. This make it possible to handle
several simulation parameters without affecting the internal variables
of the `ptsd` module.

To start a simulation, one must first load the `ptsd`
module:

```python
import ptsd
```

This will automatically load the `actr.py` module and establish a
connection, if it hasn't been done before.

The next step is to then create an instance of a Simulation object:

```python
mysim = ptsd.Simulation()
```

At this point, the simulation parameters can be controlled by setting
appropriate variables of the simulation object. For example, to create
a simulation over _V_ values of 2.0 and 5.0, with _N_ = 100 runs per
value, and a maximum duration of 100,000 seconds, one would type this:

```python
mysim.Vs = [2.0, 5.0]
mysim.n = 100
mysim.max_time = 100000
```

The entire simulation can be executed through the `run` method:

```python
mysim.run()
```

Once the simulation is done, the data is saved in the `TRACE` variable
of the `mysim` object. The trace can be saved to a file:

```python
mysim.save_trace("mysim.txt")
```

## Parameters

The following parameters can be set for each simulation:

* __PTEV__ The "emotional" value of a traumatic memory. It can be set
  to either a number or to a list of numbers; in the latter case,
  the simulations will loop over the given values.  

* __PTES__ The degree of similarity between the traumatic event and
  all the other events in memory. It can be set to either a number 0
  <= n <=1, or to a list of numbers 0<= n <= 1; in the latter case,
  the simulations will loop over the given values. Similarity is
  controlled by selecting the slot values for the PTE from a different
  set. If similarity is 1.0, then all the slots of the PTE will come
  from the same set of slot values as the other events. If _PTES_ =
  0.0, on the other hand, the slots will come from a different set. 

* __num_slots__ The number of slots in each chunk. These slots will be
  named `slot1`, `slot2` ... `slotN`.

* __n__: The number of runs for each combination of parameters. Must
  be a non-negative integer.

* __PTET__: The (ACT-R) time (in seconds, starting 0.0) at which the
  PTE is scheduled to occur.

* __event_step__: The distance between two consecutive situations
  being presented to the model.

* __model__: The ACT-R model to be loaded and used for the simulation.

* __model_params__: A dictionary of model parameters to be set in the
  model. These are supposed to be meaningful ACT-R parameters; no
  check is performed on their consistency.


## Example code

The `simulations.py` file provide an example of script that can be
used to run simulations. In essence, simulations can be easily managed
by looping through the desired parameters:

```python
for ia in [0, 2, 4, 6, 8, 10]:
    for blc in [0.3, 0.4, 0.5, 0.6, 0.7]:
        s = ptsd.Simulations()
        s.PTEV = [1, 5, 10, 15, 20]
        s.PTES = [0, 0.25, 0.5, 0.75, 1]
        s.model_params = {":imaginal-activation" : ia,
                          ":blc" : blc}
        s.n = 100
        s.max_time = 100000
        s.run()
        s.save_trace("trace_ia=%d_blc=%.2f.txt" % (ia, blc))

```

Note that the code assumes that ACT-R is running in the background; a
fully-functional script would start an ACT-R process in the background
and run the simulatins afterwards.