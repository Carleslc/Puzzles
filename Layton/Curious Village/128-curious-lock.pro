% Professor Layton: Curious Village (Puzzle 128)

% Solve the following lock:
%
%   A   E
%   B D F
%   C   G
%
% Where A * B * C = E * F * G = B * D * F
% With different digits from 1 to 9

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
