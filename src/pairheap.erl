-module(pairheap).
-export([new/0,
         new/1,
         find_min/1,
         insert/2,
         from_list/1,
         from_list/2,
         merge/2,
         delete_min/1,
         empty/1]).

-record(heap, {heap, comp}).

% Public
new() -> new(default_comp()).
new(Comparator) -> #heap{heap=empty, comp=Comparator}.

find_min(#heap{heap=empty}) ->
    {error, empty};
find_min(#heap{heap={Elem}}) ->
    {ok, Elem};
find_min(#heap{heap={Elem,_SubHeap}}) ->
    {ok, Elem}.
    
insert(#heap{heap=Heap, comp=Comp}=OldHeap, Data) ->
    OldHeap#heap{heap=merge_(Heap, {Data}, Comp)}.

from_list(Items) ->
    from_list(Items, default_comp()).

from_list(Items, Comparator) ->
    lists:foldl(fun(Item, Heap) -> 
                insert(Heap, Item) end, new(Comparator), Items).

merge(#heap{heap=Heap1, comp=Comp}=OldHeap, #heap{heap=Heap2}) ->
    OldHeap#heap{heap=merge_(Heap1, Heap2, Comp)}.

delete_min(#heap{heap={_Elem}}=OldHeap) ->
    {ok, OldHeap#heap{heap=empty}};
delete_min(#heap{heap={_Elem, SubHeaps}, comp=Comp}=OldHeap) ->
    {ok, OldHeap#heap{heap=merge_pairs(SubHeaps, Comp)}}.

empty(#heap{heap=empty}) ->
    true;
empty(#heap{heap=_Other}) ->
    false.

% Private

default_comp() -> fun(A,B) -> A < B end.

merge_(Heap1, empty, _Comp) ->
    Heap1;
merge_(empty, Heap2, _Comp) ->
    Heap2;
merge_(Heap1, Heap2, Comp) ->
    E1 = element(1, Heap1),
    E2 = element(1, Heap2),
    case Comp(E1, E2) of
        false ->
            {E2, [Heap1 | second(Heap2)]};
        true ->
            {E1, [Heap2 | second(Heap1)]}
    end.

second({_E, SH}) -> SH;
second({_E}) -> [].

merge_pairs([], _Comp) -> empty;
merge_pairs([SubHeap], _Comp) -> SubHeap;
merge_pairs([SH1, SH2 | Rest], Comp) ->
   merge_(merge_(SH1, SH2, Comp), merge_pairs(Rest, Comp), Comp).
    
