% Professor Layton: Diabolical Box (Puzzle 145)
% https://layton.fandom.com/wiki/Puzzle:500_Pearls

/**
A wizard sets a traveller a strange task.

"There are 500 pearls in that cave.
I want you to go and bring a certain number of pearls to me.
This number allows you to divide the pearls into groups of 2, 3, 4, 5, 6 or 7 and always have one pearl left over.
Bring me this number of pearls and you can keep them all for yourself!"

The traveller tries his best, but the number of pearls he bring back doesn't leave one pearl over when divided into groups of 4.
How many pearls did he bring to the wizard?
*/

:- use_module(library(clpfd)).

test(Pearls, Remainder, []) :- Pearls #> Remainder.
test(Pearls, Remainder, [Divisor|Divisors]) :-
  Pearls mod Divisor #= Remainder,
  test(Pearls, Remainder, Divisors).

solve(TotalPearls, Remainder, Divisors, Pearls) :-
  Pearls in Remainder..TotalPearls,
  test(Pearls, Remainder, Divisors),
  indomain(Pearls).

solve :-
  solve(500, 1, [2,3,5,6,7], TravellerPearls),
  TravellerPearls mod 4 #\= 1,
  format("Traveller brings ~w pearls to the wizard", TravellerPearls), nl,
  solve(500, 1, [2,3,4,5,6,7], WizardPearls),
  format("But the wizard expected ~w pearls", WizardPearls), nl,
  halt.

/** <examples>

?- solve(500, 1, [2,3,5,6,7], Pearls).
?- solve(500, 1, [2,3,4,5,6,7], Pearls).
*/
