Pairheap

A min-heap implementation of paring heaps for Erlang.  It exhibits the following
performance traits:

find-min: O(1)
insert: O(1)
merge: O(1)
empty: O(1)
delete: O(log n)

Pairheap has also been optimized for memory utilization, as big heaps can take
substantial amounts of memory.
