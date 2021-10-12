/*  
    Author:  Zi Ng <zyng@student.unimelb.edu.au>
    Id:      zyng
    Purpose: Solves fillin puzzles represented by a 2D grid and a list of words
             by fitting all the words onto the grid.

    E.g.    [[_,_,_,_],                    [[b,o,a,t],  
             [_,_,_,#],                     [a,r,t,#],
             [_,_,_,_]]                     [n,e,e,d]]
         Words = [[b,o,a,t],[a,r,t],[n,e,e,d],[b,a,n],[o,r,e],[a,t,e]] 

    The grid is first processed into a list of word slots. Then, we limit the 
    search space by counting the number of possible words that can fit into
    each slot. The program fills in the easiest slot with the smallest number 
    of possible words. The filled in slot and used word are removed from the 
    lists. The program then attempts to fill in the remaining slots with words 
    until all the slots are filled and all thewords exhausted. Finally the 
    solved fillin puzzle is printed to console.

    To run the program with a pre-defined test case numbered N (1-15), use 
    these terminal commands. 
    ?- consult('tests.pl').
    ?- test(N).
*/

:- ensure_loaded(library(clpfd)).

/*
    puzzle_solution(+Puzzle, +WordList)
    Solves a fillin puzzle for the given list of words.
*/
puzzle_solution(_, []).
puzzle_solution(Puzzle, WordList) :-
    list_slots(Puzzle, Slots),
    map_list_to_pairs(length, WordList, WordLens),
    map_list_to_pairs(length, Slots, SlotLens),
    fill_puzzle(SlotLens, WordLens).

/*
    fill_puzzle(+SlotLens, +WordLens)
    Recursively fills in a list of slot-length pairs with a list of word-length
    pairs starting from the least ambiguous slot.
*/
fill_puzzle([], []).
fill_puzzle(SlotLens, WordLens) :-
    possible_fits(SlotLens, WordLens, ProcessedSlots),
    % fill least ambiguous slot
    ProcessedSlots = [_NFits-Slot-Words | _Slots],
    member(L-Word, Words),
    Slot = Word,
    % remove filled slot and word
    delete(WordLens, L-Word, NewWordLens),
    exclude(slot_filled(Word), SlotLens, NewSlotLens),
    fill_puzzle(NewSlotLens, NewWordLens).

/*
    slot_filled(+Word, +Slot)
    Determines if a slot has been filled by a word.
*/
slot_filled(Word, _Len-Slot) :-
    maplist(atom, Slot),
    unifiable(Word, Slot, _).

/*
    possible_fits(+SlotLens, +WordLens, -ProcessedSlots)
    Given a list of slot-length pairs and a list of word-length pairs, produces
    `ProcessedSlots` which is a list of the number of solutions, slots and a 
    lists of word-length pairs that fit, sorted in ascending order of number of
    solutions, e.g. [1-[_,_,t]-[1-[c,a,t]], ...].
*/
possible_fits(SlotLens, WordLens, SortedSlots) :-
    maplist(count_slot_fits(WordLens), SlotLens, ProcessedSlots),
    sort(1, @=<, ProcessedSlots, SortedSlots).

/*
    count_slot_fits(+WordLens, +SlotLen, -ProcessedSlot)
    Takes a list of word-length pairs and a slot-length pair and produces a 
    `ProcessedSlot` consisting of the number of solutions, slot and a list of 
    word-length pairs that fit, e.g. 1-[_,_,t]-[1-[c,a,t]].
*/
count_slot_fits(WordLens, Len-Slot, Count-Slot-FilteredWords) :-
    include(word_fits_in_slot(Len-Slot), WordLens, FilteredWords),
    length(FilteredWords, Count).

/*
    word_fits_in_slot(+Slot, +Word)
    Determines if a word can fit into a slot.
*/
word_fits_in_slot(SlotLen-Slot, WordLen-Word) :-
    SlotLen = WordLen,
    unifiable(Slot, Word, _).

/*
    list_slots(+Puzzle, -Slots)
    Lists all horizontal and vertial slots in a fillin puzzle by transposing 
    the puzzle. 
*/
list_slots(Puzzle, Slots) :-
    transpose(Puzzle, PuzzleT),
    list_rows_slots(Puzzle, RowSlots),
    list_rows_slots(PuzzleT, ColumnSlots),
    append(RowSlots, ColumnSlots, Slots). 

/*
    list_rows_slots(+Puzzle, -Slots)
    Lists all horizontal slots in a puzzle.
*/
list_rows_slots(Puzzle, RowSlots) :-
    list_rows_slots_helper(Puzzle, RowSlots, []).

/*
    list_rows_slots_helper(+Rows, -Slots, +Acc)
    Converts the first row into a list of slots and adds them to the 
    accumulator. 
*/
list_rows_slots_helper([], AllSlots, AllSlots).
list_rows_slots_helper([FirstRow|Rows], AllSlots, Acc) :- 
    row_to_slots(FirstRow, RowSlots),
    append(Acc, RowSlots, NewAcc),
    list_rows_slots_helper(Rows, AllSlots, NewAcc).

/*
    row_to_slots(+Row, -Slots)
    Turns a row of squares into a list of slots separated by blocks ('#').
    E.g. [_,_#_,#,c,_,_,#,#_,_] -> [[_,_],[c,_,_],[_,_]]
*/
row_to_slots(Row, Slots) :-
    row_to_slots_helper(Row, Slots, [], []).

/*
    row_to_slots_helper(+Row, -Slots, +Acc, +CurrSlot)
    Scans the first character and adds it to the slot it is building if it is 
    not a block square. If the next square is a block or the end of the row, 
    the slot is complete and added to the accumulator. 
*/
row_to_slots_helper([], Slots, Slots, _). 
row_to_slots_helper([Char|Row], Slots, Acc, CurrSlot) :-
    % add Char to current slot
    ((var(Char); Char \= #) -> 
        append(CurrSlot, [Char], NewSlot);
        NewSlot = []
    ),
    length(NewSlot, SlotLen),
    % finish constructing slot 
    ((SlotLen > 1, (Row = []; (Row=[Next|_], atom(Next), Next='#'))) ->
        append(Acc, [NewSlot], NewAcc);
        NewAcc = Acc
    ),
    row_to_slots_helper(Row, Slots, NewAcc, NewSlot).
