# A simple ACT-R models of intrusive memories in PTSD 

## Main idea

The main idead behind the model is that intrusive memories in PTSD are memories who additional emotional boost leads to "hijacking" the (otherwise rational) declarative memory system. When the emotional boost increases the activation of a memory beyond a certain threshold, the memory becomes "intrusive" because its strength overrides declarative decay and cue-based filtering during memory retrieval. This, in turns, leads to a positive feedback loop, leading the memory's activation to self-perpetuation.

## Implementation

In ACT-R, everything would be a simple cue-based retrieval process.

To initial a retrieval, all we need to do is to occasionally change the chunks in visual, aural, and imaginal buffers, and reset the “processed” slot in the goal buffer.

Context chunks (visual, aural, and imaginal) will always become memories. Episodic memories, so… I guess we can model PTSD as events (visual, imaginal) that are intrinsically awful?

## Intrusive memories

Chunks becomes intrusive memories because they “take over” the retrieval process.

How do we assign an emotional value to chunks? We can imagine V values being normally distributed with a small variation---most things are neither good nor bad. Traumatic events are incredibly bad, so they have high emotional value. The sign, in many ways, does not matter.

The environment simulates the life of an agent. We simulate about 15 years of memories, with the idea that a retrieval happens about  X an hour, 16 hours a day, 365 days for 15 years, that is, X*87,600.
Over these 87,600 retrievals, memories are created.
This is a simple function:

```lisp
(defun life ()
  (loop-until-end
	 (generate-new-context)
	 (when (time = target)
	   (insert traumatic Q0))
	 (set-new context)
	 (set-goal)
	 (set-new-update))
```

## Implementation details

How many cues are present? And how big is each chunk?
We need to measure chunk size. We can start with very small chunk contexts, maybe 5-10 cues.

## Emotional valence

The emotional valence of each chunk is calculated as a function of its cues. Each cue has a value of V(q) ~ N(0).

One cue, Q0, is defined as having an incredibly high value emotional valence V, which is fixed. In fact, all cue values are stored in a table. However, we can calculate exactly the emotional value of each chunk as the sum of the emotional valences of its cues, V(chunk) = Sum[q] V(q).

## Associations

In ACT-R, associations are stored in Sij values. 

We can just keep two tables: One is the table of co-occurrence of memories, that is, the probability that c is needed when q is present in the context. This is simple enough to calculate.

Notice that Sjis go from slot to slot… So, the slots are also cues q. In fact, our association matrix can be a 100x100 (Nq by Nq) matrix. We can keep track of associations. If they are not completely random, cues are going to co-occur and be reflected in memory.

The other is the emotional association Vs. Sij has an additive term, which is the emotional nature of the chunk being retrieved. W(Sij+V).

V could be just the emotional valence of chunk c. This is equivalent to adding a fixed quantity NWV… 

The alternative from Fum & Stocco 2004 is to use V to as associative links. In practice, the result is the same (activation boost), but the form proposed by Fum & Stocco (2004) cannot be derived from rational analysis. Here, we will derive a simpler form using the same rational analysis approach that was originally used by Anderson as the basis of ACT-R's equations.

We can calculate the probability of retrieval of an average memory (sliding window) since the moment is introduced, against the probability of the traumatic memory.

Odds = P(c)/P(-c) * P(Q|c)/P(Q|-c) 
P(c)/P(-c) * P(Q|c)/P(Q)


A rational approach to memory is that we should retrieve chunks according to ther probability of being used. This approach assumes that all information is, potentially, equally important, and the only important issue to allocate the availability of information so that the more likely relevant information can be retrieved more easily.

However, in more general terms, one can calculate the expected utility of information E[c]. The expected utility conveys both the probability of needing the piece of information, and the additional value of using it. This is akin to the concept of expected utility in economic theory, which is calculated as the product of the probability of the event by its future rewards. In simple terms, we can write”
E[c] = P(c|Q) * V(c)
	
V(c) represents the value of a chunk, or the opposite of the costs incurring in using the information. For simplicity, we can assume that the value of a chunk c is independent of its context Q. We know how to calculate P(c|Q); a detailed analysis was proposed by Anderson 1991. In essence, Anderson calculated that P(c|Q) could be expressed in odds:

P(c)/P(-c) * P(Q|c)/P(Q) 

Thus, the expected utility becomes

P(c)/P(-c) * P(Q|c)/P(Q) * V(c)

Anderson 1991 proposed that these terms could be decomposed into the log sum:

log(P(c)/P(-c)) + log P(Q|c)/P(Q) + log  V(c) 

The first two terms have been extensively discussed in the context of ACT-R. In particular, the first can be captured by the equation log Sum t (t0 –t)^-d. The second, represents contextual activation, and is the result of an empirical co-occurrence between the contents of c and the contents of Q.

The third term is the topic of this paper. Specifically, we will show that  for events that have an very high emotional costs, such as traumatic events, the additional term can result in catastrophic runoff. This is due to the peculiar interaction between memory and environment: When items are retrieved, they become more likely to be retrieved in the future. This positive feedback loop is typically kept at bay by the contextual factor, which favor elements based on co-occurrence. When the third factor V overtakes both, it generates two contextual effects: First, it leads to dominate the base-level probabilities. Second, it tends to generate even stronger associations with other cues, thus contaminating them and eventually taking over the contextual factor as well. Both of these mechanisms are known, under different names, in the field. Furthermore, this model provides an explanation of why certain treatments work, and why other fail.

## Representation

Memory is represented as a database of individual memories. The
database is simply a list.

Memories are contentless.

New memories are created at a fixed rate, and inserted into the list.

Memories are created within a context made of individual cues
q1... qN.

Cues are also contentless. A limited number of N cues is available.

Only a subset M < N of cues is available at any point in time, forming
the context in which a memory is retrieved.

Cues change over time; at any point in time, cues are slowly changed.

The number of cues that are changed is drawn from a gamma
distribution.

It would likely be smart to initialize memories and cues-memory
associations randomly at the beginning.

Learning
--------
When created, memories are simply put into the list.

Every time a meory 

