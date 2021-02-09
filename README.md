# An ACT-R Model of Intrusive Memories in PTSD

This repository contains the code of an ACT-R model designed to
capture the dynamics of intrusive memory retrievals in PTSD. This in
an ongoing collaboration with Briana Smith of [UW Bioengineering](https://bioe.uw.edu/) and
[Lori Zoellner](https://psych.uw.edu/directory/2704) of [UW Psychology](https://psych.uw.edu/) and [UWCATS](https://faculty.washington.edu/zoellner/wp/).

## Theory and Description

This model is based on the Bayesian memory model developed by Anderson
(1990) and currently implemented in the ACT-R cognitive
architecture. Within this framework, In this model, the probability of
retrieving a memory _m_ is proportional to its activation _A_(_m_). A
memory’s activation is, in turn, the sum of three components: a
base-level activation _B_(_m_), a contextual or "spreading" activation
_S_(_m_), and the emotional intensity of the memory at the time of its
creation, _I_(_m_).


The base-level activation _B_(_m_) reflects the history of a
memory. For each memory _m_, multiple traces _i_ exist, each
corresponding to an episode in which m was retrieved or
re-encoded. The activation of each trace _i_ decays with time, and the
activation of _m_ is the sum of the decaying activations of all its
traces _i_:


_B_(_m_) = log ∑_i _t_(_i_)^-_d_

where _t_(_i_) is the time elapsed since the creation of _i_ and _d_ is an individual-specific rate of forgetting.  The contextual activation _S_(_m_) is related to the associated strength sq,m between each current context’s cue q (that is, any feature the the current state of the environment) and m: 

S(m) = ∑ _q_ _WS(q➝m)_ 	

Where _W_ represents the amount of attention given to the context. The
strength between cues and memories _S(q➝m)_ is calculated through a Bayesian
analysis, and reflects the probability of _m_ being retrieved in the
presence of _q_.

Finally, the emotional intensity of a memory is represented by the
term γ_I_(_m_), where _I_(_m_) is the intensity (i.e., a metric summarizing
arousal and value) of the memory at the moment of its creation and γ
is free parameter that captures an individual’s specific recollection
vividness. Thus, the final activation of a memory is

_A_(_m_) = _B_(_m_) + _S_(_m_) + γ _I_(_m_)


### Biological Interpretation of the Model 

All of these terms were derived from a Bayesian analysis of memory
demands (Smith et a., 2020). Furthermore, each of them can be put into
correspondence with a specific brain circuit that is known to play a
causal role in intrusive memories. Specifically, B(m) reflects the
contribution of the hippocampus and the medial temporal lobe in memory
encoding and maintenance; S(m) reflects the role of
prefrontal-thalamic projections in guiding memory retrieval in the
appropriate context; I(m) reflects the contribution of the amygdala to
emotion processing and its effects on the hippocampus, and γ reflects
the functional connectivity between the amygdala and the hippocampus
(Figure ZA).


## Implementation

The `model` folder contains the current, "fast", Lisp-based  implementation of the model. The
implementation is based on the original Python/Lisp model that can
still be found in he `old` folder. The fast implementation is needed
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

  3. The `ptsd-mdoel.lisp` file contains the main ACT-R code that
  manage the model's cognitive cycle.

  4. All the commands setting have been removed from the
  `ptsd-model.lisp` file, and are instead set directly through Lisp
  code at the beginnig of every simulation (__Note__: this is clearly
  where the Lisp code diverges from the Python code)

### Compatibility

The "fast" Lisp code is __entirely incompatible__ with the Python
code. You can use one or the other, but not both. In fact, the Lisp
code assumes that no JSON RPC system even exists, and replaces all of
the command calls with Lisp-level hook functions.

### Usage

The only way to use the Fast code is through a Lisp interpreter, after
ACT-R has been loaded. If you are using the Stand-Alone version of
ACT-R, then you (obviously) do not need to load ACT-R. Otherwise, you
need to load ACT-R by typing the following command on your REPL prompt:

```lisp
(load "/path/to/actr/load-act-r.lisp")
```

To start a simulation, one must first load the `ptsd` file:

```lisp
(load "ptsd.lisp")
```

This will automatically define all the necessary classes and methods
for the simulations.

The next step is to then create an instance of a Simulation object:

```lisp
(setf mysim (make-instance 'simulation))
```

At this point, the simulation parameters can be controlled by setting
appropriate variables of the simulation object. For example, to create
a simulation over _V_ values of 2.0 and 5.0, with _N_ = 100 runs per
value, and a maximum duration of 100,000 seconds, one would type this:

```lisp
(setf (ptev mysim) '(2 5))
(setf (n mysim) 100)
(setf (max-time mysim) 100000)
```

The entire simulation can be executed through the `run-simulations` method:

```lisp
(run-simulations mysim)
```

(__Note__: the Lisp code `run-simulations` replaces the `run` method in Python. It had to be renamed because the `run` function is already defined by ACT-R).

Once the simulation is done, the data is saved in the `TRACE` variable
of the `mysim` object. The trace can be saved to a file:

```python
(save_trace mysim "mysim.txt")
```

### Parameters

As in the Python code, the following parameters can be set for each
simulation. This being Lisp code, none of the names are actually
case-sensitive.

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


### Example code

The `simulations.py` file provide an example of script that can be
used to generate Lisp code that manages and runs simulations. It
contains a Lisp template that can be copied and personalized. In
essence, simulations can be easily managed by looping through the
desired parameters:


```lisp
(load "load-act-r.lisp")
(load "ptsd.lisp")
(setf sim (make-instance 'simulation))
(setf (ptev sim) 
      '(1 20 10 5 15))
(setf (ptes sim)
      '(0 1 0.5 0.25 0.75))
(let ((ht (make-hash-table)))
  (setf (gethash :imaginal-activation ht) 10)
  (setf (gethash :bll ht) 7/10)
  (setf (model-params sim) ht))

(setf (logfile sim) "simulations_w=10_bll=0.6.txt") 
(run-simulations sim)
(quit)
```

## Publications

The following publicatins are based on the model:

* Smith, B. M., Chiu, M., Yang, Y. C., Sibert, C., & Stocco, A. (2020). Modeling the Effects of Post-Traumatic Stress on Hippocampal Volume. _Proceedings of the 18th International Conference on Cognitive Modeling_.
