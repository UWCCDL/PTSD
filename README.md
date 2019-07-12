# A simple ACT-R models of intrusive memories in PTSD 

## Main idea

The main idead behind the model is that intrusive memories in PTSD are
memories who additional emotional boost leads to "hijacking" the
(otherwise rational) declarative memory system. When the emotional
boost increases the activation of a memory beyond a certain threshold,
the memory becomes "intrusive" because its strength overrides
declarative decay and cue-based filtering during memory
retrieval. This, in turns, leads to a positive feedback loop, leading
the memory's activation to self-perpetuation.


We can calculate the probability of retrieval of an average memory
(sliding window) since the moment is introduced, against the
probability of the traumatic memory.

    Odds = P(c)/P(-c) * P(Q|c)/P(Q|-c) 
    P(c)/P(-c) * P(Q|c)/P(Q)


A rational approach to memory is that we should retrieve chunks
according to ther probability of being used. This approach assumes
that all information is, potentially, equally important, and the only
important issue to allocate the availability of information so that
the more likely relevant information can be retrieved more easily.

However, in more general terms, one can calculate the expected _utility_
of information, E[c]. The expected utility conveys both the probability
of needing the piece of information, and the additional value of using
it. This is akin to the concept of expected utility in economic
theory, which is calculated as the product of the probability of the
event by its future rewards. In simple terms, we can write:

E[c] = P(c|Q) * V(c)
	
V(c) represents the value of a chunk, or the opposite of the costs
incurring in using the information. For simplicity, we can assume that
the value of a chunk c is independent of its context Q. We know how to
calculate P(c|Q); a detailed analysis was proposed by Anderson
1991. In essence, Anderson calculated that P(c|Q) could be expressed
in odds:

   P(c)/P(-c) * P(Q|c)/P(Q) 

Thus, the expected utility becomes

   P(c)/P(-c) * P(Q|c)/P(Q) * V(c)

Anderson, 1991 proposed that these terms could be decomposed into the
log sum:

   log(P(c)/P(-c)) + log P(Q|c)/P(Q) + log  V(c) 

The first two terms have been extensively discussed in the context of
ACT-R. In particular, the first can be captured by the equation log
Sum t (t0 –t)^-d. The second, represents contextual activation, and is
the result of an empirical co-occurrence between the contents of c and
the contents of Q.

The third term is the topic of this model. Specifically, for events
that have an very high emotional costs, such as traumatic events, the
additional term can result in catastrophic runoff. This is due to the
peculiar interaction between memory and environment: When items are
retrieved, they become more likely to be retrieved in the future. This
positive feedback loop is typically kept at bay by the contextual
factor, which favor elements based on co-occurrence. When the third
factor V overtakes both, it generates two contextual effects: First,
it leads to dominate the base-level probabilities. Second, it tends to
generate even stronger associations with other cues, thus
contaminating them and eventually taking over the contextual factor as
well. Both of these mechanisms are known, under different names, in
the field. Furthermore, this model provides an explanation of why
certain treatments work, and why other fail.


# Implementation

The model is a simple perceive-retrieve-respond loop based on
ACT-R. The model perceives a situation (represented as a chunk in
ACT-R's `imaginal` buffer), retrieves a relevant memory, and uses the
retrieved memory as a response. Thus, response to a new situation is a
simple is a simple cue-based retrieval process.

Specifically, when a new situation is encountered, the model sets a
goal to process the new situation. The goal leads to initiating a
retrieval process; when a relevant memory is retrieved, the goal is
accompolished and marked as such.

At fixed intervals of time, new situations are encountered.

All new situations eventually become episodic memories, and end up in
the long term memory store.

## Intrusive memories

Chunks becomes intrusive memories because they “take over” the
retrieval process.

How do we assign an emotional value to chunks? We can imagine V values
being normally distributed with a small variation---most things are
neither good nor bad. Traumatic events are incredibly bad, so they
have high emotional value. The sign, in many ways, does not matter.

The environment simulates the life of an agent. This is a simple
function in pseudocode:

```python
def life ()
  while actr.mp_time() < end:
	 if time = PTET:
        present_new_situation(traumatic = True)
     else:
        present_new_situation(traumatic = False)
```

The model responds to any new situatiobn by retrieving the past
situation that best matches the current one.

## Emotional valence

In this implementation, the emotional valence _V_ of each chunk is stored
in a separate table, the __V__ table. Every time a chunk _c_ is
created, the pair <_c_, _V_(_c_)> is stored in the table. Its value
_V_(_c_) is calculated as follows. For "normal" chunks, _V_ = 1.0 +/-
noise. For a potentially traumatic event (PTE), it is set to a
predefined value _PTEV_ >> 1.0.   

An alternative implementation (not pursued here, but worth
considering) is that the emotional valence of each chunk is calculated
as a function of its cues. Each cue has a value of V(q) ~ N(0).

One cue, _Q_*, is defined as having an incredibly high value emotional
valence V, which is fixed. In this implementation, the V table
contains only values for cues, not for chunks. This makes the
management of __V__ tables easier. We can calculate exactly the
emotional value of each chunk as the sum of the emotional valences of
its cues, V(chunk) = Sum[q] V(q).

## Associations

In ACT-R, associations are stored in Sij values, and are supposed to
be learned through experience. In the current implementation,
association is purely defined by similarity: The more similar two
chunks are, the more they are associated. To keep the
implementation consistent with the ACT-R principle of Sji representing
the shared contents of two chunks, the similarity between two chunks
is calculated as the proportion of slots in which the two chunks share
the same value.

In the future, it would be helpful to learn associations. To do so, we
could just keep two tables: One is the table of co-occurrence of
memories, that is, the probability that c is needed when q is present
in the context. This is simple enough to calculate.

The other is the emotional association Vs. Sij has an additive term,
which is the emotional nature of the chunk being retrieved. W(Sij+V).


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

* __PTES__ The degree of similarity between the traumatic event and
  all the other events in memory. Similarity is controlled by
  selecting the slot values for the PTE from a different set. If
  similarity is 1.0, then all the slots of the PTE will come from the
  same set of slot values as the other events. If _PTES_ = 0.0, on the
  other hand, the slots will come from a different set.

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