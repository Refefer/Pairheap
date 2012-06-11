-module(pairheap).
-export([new/0,
         find_min/1,
         insert/2,
         from_list/1,
         merge/2,
         delete_min/1]).

new() -> empty.

find_min(empty) ->
    {error, empty};

find_min({Elem,_SubHeap}) ->
    {ok, Elem}.
    
insert(Heap, Data) ->
    merge(Heap, {Data, empty}).

from_list(Items) ->
    lists:foldl(fun(Item, Heap) -> insert(Heap, Item) end, new(), Items).

merge(Heap1, empty) ->
    Heap1;
merge(empty, Heap2) ->
    Heap2;
merge({E1, SH1}, {E2, SH2}) ->
    if
        E1 < E2 ->
            {E1, [{E2, SH2} | to_list(SH1)]};
        true ->
            {E2, [{E1, SH1} | to_list(SH2)]}
    end.

to_list(empty) -> [];
to_list(L) -> L.

delete_min([]) ->
    {error, empty};
delete_min({_Elem, empty}) ->
    {ok, empty};
delete_min({_Elem, SubHeaps}) ->
    {ok, merge_pairs(SubHeaps)}.

merge_pairs([]) -> empty;

merge_pairs([SubHeap]) -> SubHeap;

merge_pairs([SH1, SH2 | Rest]) ->
   merge(merge(SH1, SH2), merge_pairs(Rest)).
    
