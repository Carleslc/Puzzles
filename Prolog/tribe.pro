% https://wiki.haskell.org/Logic_programming_example
% 
% There is a tribe where all the Male members speak true statements and Female members never speak two true statements in a row, nor two untrue statements in a row. (I apologize for the obvious misogyny).
% 
% A researcher comes across a mother, a father, and their child. The mother and father speak English but the child does not. However, the researcher asks the child "Are you a boy?". The child responds but the researcher doesn't understand the response and turns to the parents for a translation.
% 
% Parent 1: "The child said 'I am a boy.'"
% Parent 2: "The child is a girl. The child lied."
% 
% What is the sex of parent 1, parent 2, the child, and what sex did the child say they were?
% 
% There is a unique solution for heterosexual, gay, and lesbian couples. Find all three solutions.

% GLOBAL

bool(true).
bool(false).

% CLOSED WORLD

sex(male).
sex(female).

sex_male(male).
sex_male(true).
sex_female(female).
sex_female(false).

bool_sex(true, male).
bool_sex(false, female).

valid_statements(_, []).
% male members only speak true statements
valid_statements(male, [S|SS]) :- bool(S), S == true, valid_statements(male, SS).
% female members never speak two true statements in a row, nor two untrue statements in a row
valid_statements(female, [S]) :- bool(S).
valid_statements(female, [S1, S2]) :- bool(S1), bool(S2), dif(S1, S2).
valid_statements(female, [S1, S2, S3|SS]) :- valid_statements(female, [S1, S2]), bool(S3), dif(S2, S3), valid_statements(female, [S3|SS]).

person_says(P, []) :- sex(P).
person_says(P, S) :- sex(P), valid_statements(P, S).

% Child: Responds to "Are you a boy?"
child_responds_are_you_a_boy(Child, ChildSays) :- person_says(Child, [ChildSays]).

% Parent 1: "The child said 'I am a boy.'"
first_parent_says_child_said_is_a_boy(Parent1, ChildSaidIsABoy, ChildSays) :-
  person_says(Parent1, [ChildSaidIsABoy]),
  child_said_is_a_boy(ChildSays, ChildSaidIsABoy).

child_said_is_a_boy(ChildSays, true) :- sex_male(ChildSays).
child_said_is_a_boy(ChildSays, false) :- sex_female(ChildSays).

% Parent 2: "The child is a girl. The child lied."
second_parent_says_child_is_girl_and_lied(Parent2, ChildIsAGirl, ChildLied, Child, ChildSays) :-
  person_says(Parent2, [ChildIsAGirl, ChildLied]),
  child_is_a_girl(Child, ChildIsAGirl),
  child_lied(Child, ChildSays, ChildLied).

child_is_a_girl(Child, true) :- sex_female(Child).
child_is_a_girl(Child, false) :- sex_male(Child).

child_said_truth(Child, ChildSays) :- (sex_male(ChildSays), sex_male(Child)) ; (sex_female(ChildSays), sex_female(Child)).
child_lied(Child, ChildSays) :- \+ child_said_truth(Child, ChildSays).
child_lied(Child, ChildSays, true) :- child_lied(Child, ChildSays).
child_lied(Child, ChildSays, false) :- child_said_truth(Child, ChildSays).

solve(Parent1, Parent2, Child, ChildSays, ChildSaidIsABoy, ChildIsAGirl, ChildLied) :-
  child_responds_are_you_a_boy(Child, ChildSays),
  first_parent_says_child_said_is_a_boy(Parent1, ChildSaidIsABoy, ChildSays),
  second_parent_says_child_is_girl_and_lied(Parent2, ChildIsAGirl, ChildLied, Child, ChildSays).

display_solution([Parent1, Parent2, Child, ChildSays, ChildSaidIsABoy, ChildIsAGirl, ChildLied]) :-
  format("~nChild = ~w [Responds to 'Are you a boy?' with: ~w]~nParent1 = ~w [The child said 'I am a boy.': ~w]~nParent2 = ~w [The child is a girl: ~w, The child lied: ~w]",
    [Child, ChildSays, Parent1, ChildSaidIsABoy, Parent2, ChildIsAGirl, ChildLied]), nl.

solve :-
  findall([Parent1, Parent2, Child, ChildSays, ChildSaidIsABoy, ChildIsAGirl, ChildLied],
    solve(Parent1, Parent2, Child, ChildSays, ChildSaidIsABoy, ChildIsAGirl, ChildLied), Solutions),
  length(Solutions, N),
  format("~w solutions", N), nl,
  maplist(display_solution, Solutions),
  halt.

/** <examples>

?- solve(Parent1, Parent2, Child, ChildSays, ChildSaidIsABoy, ChildIsAGirl, ChildLied).
*/
