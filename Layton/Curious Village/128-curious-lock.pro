% Professor Layton: Curious Village (Puzzle 128)
% https://layton.fandom.com/wiki/Puzzle:Number_Lock

/**
The door in front of you has an odd lock mounted on the front.
The only way to unlock this strange contraption is to place small tiles labeled from one to nine in its slots.
The lock will open when the numbers on the lock equal the same number when multiplied vertically and horizontally.

There are nine tiles, but the lock only has seven slots, so you won't need two tiles.

Can you open the lock?

  A   E
  B D F
  C   G

A * B * C = E * F * G = B * D * F
*/

:- use_module(library(clpfd)).

lock(Lock) :-
    Lock = [A,B,C,D,E,F,G],
    Lock ins 1..9,
    all_distinct(Lock),
    A*B*C #= E*F*G,
    A*B*C #= B*D*F.

solve(Lock) :-
    lock(Lock),
    labeling([ffc], Lock).

display_lock([A,B,C,D,E,F,G], Suffix) :- format("~n~w   ~w~n~w ~w ~w~n~w   ~w~s", [A,E,B,D,F,C,G,Suffix]), nl, flush_output.
display_lock(Lock) :- display_lock(Lock, "").
display_lock_nl(Lock) :- display_lock(Lock, "\n").

solve :-
    findall([A,B,C,D,E,F,G], solve([A,B,C,D,E,F,G]), Solutions),
    length(Solutions, N),
    format("~w solutions", N), nl,
    maplist(display_lock, Solutions),
    halt.

/** <examples>

?- solve([A,B,C,D,E,F,G]).
?- solve([A,B,C,D,E,F,G]), display_lock_nl([A,B,C,D,E,F,G]).
*/
