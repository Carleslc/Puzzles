# Prolog

Puzzles solved with [logic programming](https://en.wikipedia.org/wiki/Logic_programming) using [**SWI-Prolog**](https://www.swi-prolog.org/) language.

## Default goal

```sh
$ swipl -g solve -s farmer.pro
```

Replace `farmer` with the desired puzzle.

## Interactive console

```sh
$ swipl -s farmer.pro
```
```prolog
?- solve(30, 74, Chickens, Cows).

% Chickens = 23,
% Cows = 7.

% Exit with Ctrl^C + e or halt.
```

```sh
$ swipl -s "../Layton/Curious Village/128-curious-lock.pro"
```
```prolog
solve([A,B,C,D,E,F,G]), display_lock_nl([A,B,C,D,E,F,G]).

3   1
4 2 9
6   8

A = 3,
B = 4,
C = 6,
D = 2,
E = 1,
F = 9,
G = 8 ;

% press ; or space for more solutions

3   8
4 2 9
6   1

A = 3,
B = 4,
C = 6,
D = 2,
E = 8,
F = 9,
G = 1 .

% press . or enter to stop
```

See: [Executing a query](https://www.swi-prolog.org/pldoc/man?section=execquery)

At the end of each puzzle file there is a `<examples>` section with example queries.

## Develop

```prolog
$ swipl

?- ["farmer.pro"]. % load file
?- make. % reload file
?- solve.
```

## Debug

```prolog
?- trace.
[trace]  ?- solve(30, 74, Chickens, Cows).
%    Call: (10) solve(30, 74, _51476, _51478) ? creep
%    ...
%    Exit: (10) solve(30, 74, 23, 7) ? creep
[trace]  ?- notrace.
[debug]  ?- nodebug.
```
