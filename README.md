# Prolog-Word-Fillin-Puzzle-Solver

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
