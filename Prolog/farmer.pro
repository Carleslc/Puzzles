% A chicken farmer also has some cows for a total of 30 animals,
% and the animals have 74 legs in all.
% 
% How many chickens does the farmer have?
% 
% https://www.metalevel.at/prolog/optimization

:- use_module(library(clpfd)).

solve(TotalAnimals, TotalLegs, Chickens, Cows) :-
    Animals = [Chickens, Cows],
    Animals ins 0..sup,
    Chickens + Cows #= TotalAnimals,
    Chickens*2 + Cows*4 #= TotalLegs.

solve :-
    solve(30, 74, Chickens, Cows),
    format("~w Chickens~n~w Cows~n", [Chickens, Cows]),
    halt.

% :- initialization solve.

/** <examples>

?- solve(30, 74, Chickens, Cows).
?- solve(30, 100, Chickens, Cows).
*/
