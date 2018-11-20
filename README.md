# Representation

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

