-module(pairheap_test).
-include_lib("eunit/include/eunit.hrl").

make_random_list(N) ->
    [random:uniform(100000) || _ <- lists:seq(1, N)].

pop_all(Heap) ->
    pop_all(Heap, []).

pop_all(empty, Acc) ->
    lists:reverse(Acc);
pop_all(Heap, Acc) ->
    Min = pairheap:find_min(Heap),
    {ok, NewHeap} = pairheap:delete_min(Heap),
    pop_all(NewHeap, [Min|Acc]).

pairheap_sorted_test() ->
    List = make_random_list(100),
    Heap = pairheap:from_list(List),
    SortedList = pop_all(Heap),
    ?assert(SortedList =:= lists:sort(SortedList)).
