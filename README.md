Pairheap

A heap implementation of paring heaps for Erlang.  It exhibits the following
performance traits:

find-min: O(1)

insert: O(1)

merge: O(1)

delete: O(log n)

empty: O(1)

Pairheap has also been optimized for memory utilization, as big heaps can take
substantial amounts of memory.

Pairheap allows usage of a custom comparator function for comparing custom entries.

Example:

    % Using the normal comparator, from_list
    1> H1 = pairheap:from_list(lists:seq(20,1,-1)).
    {heap,{1,
           [{2,[{3,[{4,[{5,[{6,[{7,[{8,[{9,[{...}]}]}]}]}]}]}]}]}]},
          #Fun<pairheap.1.19011100>}

    % Find min
    2> pairheap:find_min(H1).
    {ok, 1}

    % Insert
    3> H2 = pairheap:insert(H1, 12).
    {heap,{1,
       [{12},{2,[{3,[{4,[{5,[{6,[{7,[{8,[{9,[...]}]}]}]}]}]}]}]}]},
      #Fun<pairheap.1.19011100>}

    % Delete
    4> H3 = pairheap:delete_min(H2).
    {ok,{heap,{2,
           [{12},{3,[{4,[{5,[{6,[{7,[{8,[{9,[{...}]}]}]}]}]}]}]}]},
          #Fun<pairheap.1.19011100>}}


Example of a Max heap using custom comparator
    
    1> H1 = pairheap:from_list(lists:seq(1,20), fun(A,B) -> A > B end).

    2> pairheap:find_min(H1).
    {ok,20}

    3> pairheap:delete_min(H1).
    {ok,{heap,{19,
           [{18,[{17,[{16,[{15,[{14,[{13,[{12,[{11,...}]}]}]}]}]}]}]}]},
          #Fun<erl_eval.12.17052888>}}

