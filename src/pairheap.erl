-module(pairheap).
-export([new/0,
         find_min/1,
         insert/2,
         from_list/1,
         merge/2,
         delete_min/1,
         empty/1]).

new() -> empty.

find_min(empty) ->
    {error, empty};
find_min({Elem}) ->
    {ok, Elem};
find_min({Elem,_SubHeap}) ->
    {ok, Elem}.
    
insert(Heap, Data) ->
    merge(Heap, {Data}).

from_list(Items) ->
    lists:foldl(fun(Item, Heap) -> insert(Heap, Item) end, new(), Items).

merge(Heap1, empty) ->
    Heap1;
merge(empty, Heap2) ->
    Heap2;
merge(Heap1, Heap2) ->
    E1 = element(1, Heap1),
    E2 = element(1, Heap2),
    if
        E1 < E2 ->
            {E1, [Heap2 | second(Heap1)]};
        true ->
            {E2, [Heap1 | second(Heap2)]}
    end.

second({_E, SH}) -> SH;
second({_E}) -> [].

delete_min({_Elem}) ->
    {ok, empty};
delete_min({_Elem, SubHeaps}) ->
    {ok, merge_pairs(SubHeaps)}.

empty(empty) ->
    true;
empty(_Other) ->
    false.

merge_pairs([]) -> empty;
merge_pairs([SubHeap]) -> SubHeap;
merge_pairs([SH1, SH2 | Rest]) ->
   merge(merge(SH1, SH2), merge_pairs(Rest)).
    
