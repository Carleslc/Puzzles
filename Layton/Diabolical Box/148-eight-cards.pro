% Professor Layton: Diabolical Box (Puzzle 148)
% https://layton.fandom.com/wiki/Puzzle:Eight_Cards

/**
These cards, numbered 1 to 8, can be used to make two multiplication equations.
One is given here (453 x 6 = 2718), but can you work out the second one?

What are the valid products including all digits from 1 to 8 in the form ABC x D = EFGH ?
*/

:- use_module(library(clpfd)).

cards(Cards) :-
    Cards = [A,B,C,D,E,F,G,H],
    Cards ins 1..8,
    all_distinct(Cards),
    (A*100 + B*10 + C) * D #= E*1000 + F*100 + G*10 + H.

solve(Cards) :-
    cards(Cards),
    labeling([ffc], Cards).

display_cards([A,B,C,D,E,F,G,H]) :- format("~w~w~w x ~w = ~w~w~w~w", [A,B,C,D,E,F,G,H]), nl.

solve :-
    findall([A,B,C,D,E,F,G,H], solve([A,B,C,D,E,F,G,H]), Solutions),
    maplist(display_cards, Solutions),
    halt.

/** <examples>

?- solve([A,B,C,D,E,F,G,H]).
?- solve([A,B,C,D,E,F,G,H]), display_cards([A,B,C,D,E,F,G,H]).
*/
