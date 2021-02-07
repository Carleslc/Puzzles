#!/usr/bin/python3
# -*- coding: utf-8 -*-

# N queens problem
# https://en.wikipedia.org/wiki/Eight_queens_puzzle

# usage: queens-backtracking.py [-h] [--n N] [--verbose] [--count-only] [--first]
#                  [--solution SOLUTION]

# optional arguments:
#   -h, --help           show this help message and exit
#   --n N                number of queens
#   --verbose            explain process
#   --count-only         do not display solutions
#   --first              stop at the first solution. Recommended for n > 12
#   --solution SOLUTION  verify a solution with columns separated by commas,
#                        e.g. 1,3,5,7,9,11,13,2,4,6,8,10,12

# NOTE: N = 32 needs about 5 minutes of computation for the first solution and N = 16 about 40 minutes for all solutions (dual-core CPU 2.9GHz)
# This program is suitable for N <= 32 with --first option or N <= 15 for all solutions

import argparse
from timeit import default_timer as timer
from itertools import permutations
from functools import reduce

def set_args():
  parser = argparse.ArgumentParser()
  parser.add_argument("--n", help="number of queens", type=int, default=8)
  parser.add_argument("--verbose", action='store_true', help="explain process")
  parser.add_argument("--count-only", action='store_true', help="do not display solutions")
  parser.add_argument("--first", action='store_true', help="stop at the first solution. Recommended for n > 12")
  parser.add_argument("--solution", help="verify a solution with columns separated by commas, e.g. 1,3,5,7,9,11,13,2,4,6,8,10,12")
  args = parser.parse_args()
  if args.n < 1:
    print("N must be a natural number")
    exit(1)
  return args

# Some math to make it easier to validate a solution
# Two queens are in the same diagonal if i1 - j1 == i2 - j2 (left to right) OR i1 + j1 == i2 + j2 (right to left)
def diagonals(i1, j1, i2, j2):
  return i1 - j1 == i2 - j2 or i1 + j1 == i2 + j2

def is_solution(queens):
  for i1, q1 in enumerate(queens):
    next_row = i1 + 1
    for r, q2 in enumerate(queens[next_row:]):
      i2 = next_row + r
      if diagonals(i1, q1, i2, q2):
        return False
  return True

def is_solution_verbose(queens):
  sol = list(enumerate(queens))
  print(f"Check solution {list(map(lambda q: (q[0] + 1, q[1] + 1), sol))}")
  for i1, q1 in sol:
    print(f"Check ({i1 + 1}, {q1 + 1})")
    next_row = i1 + 1
    for r, q2 in enumerate(queens[next_row:]):
      i2 = next_row + r
      print_(f"  Against ({i2 + 1}, {q2 + 1})")
      if diagonals(i1, q1, i2, q2):
        print("  X\n")
        return False
      else:
        print("  OK")
  print("OK\n")
  return True

def queens_backtrack(n, i, queens, cols, diaglr, diagrl):
  if i == n:
    yield tuple(queens)
  else:
    for j in range(n):
      if cols[j] and diaglr[n - 1 + i - j] and diagrl[i + j]:
        queens[i] = j
        cols[j] = diaglr[n - 1 + i - j] = diagrl[i + j] = False
        yield from queens_backtrack(n, i + 1, queens, cols, diaglr, diagrl)
        cols[j] = diaglr[n - 1 + i - j] = diagrl[i + j] = True

def queens_backtrack_verbose(n, i, queens, cols, diaglr, diagrl):
  def queens_start_1():
    return '[' + ', '.join(map(lambda j: str(j + 1) if j is not None else '', queens)) + ']'
  if i == n:
    print(f"\nSOLUTION {queens_start_1()}\n")
    yield tuple(queens)
  else:
    def is_available(j):
      return cols[j] and diaglr[n - 1 + i - j] and diagrl[i + j]
    for j in range(n):
      print_(f"({i + 1}, {j + 1})  ")
      if is_available(j):
        queens[i] = j # move
        cols[j] = diaglr[n - 1 + i - j] = diagrl[i + j] = False # threatened
        print("MOVE")
        print(queens_start_1())
        yield from queens_backtrack_verbose(n, i + 1, queens, cols, diaglr, diagrl) # solutions with (i,j) set
        queens[i] = None # release for next move
        cols[j] = diaglr[n - 1 + i - j] = diagrl[i + j] = True
        print(f"({i + 1}, {j + 1})  RELEASE")
        print(queens_start_1())
      else:
        print("X")

def queens_solutions(n, verbose = False):
  solve = queens_backtrack_verbose if verbose else queens_backtrack
  return solve(n, 0, [None] * n, [True] * n, [True] * (2*n - 1), [True] * (2*n - 1))

def ilen(iterable):
  return reduce(lambda sum, element: sum + 1, iterable, 0)

def print_(s):
  print(s, end='')

def print_board(queens):
  n = len(queens)
  columns = list(range(1, n + 1))
  def identifier(j):
    return str(j) if j / 10 < 1 else '+'
  print("  " + ' '.join(map(identifier, columns)))
  for i, q in enumerate(queens):
    print_(f"{identifier(i + 1)} ") # row
    print_('. ' * q) # first empty squares
    print_('Q') # queen
    print(' .' * (n - 1 - q)) # latest empty squares and new line
  print() # separator

if __name__ == "__main__":
  args = set_args()

  if args.solution:
    try:
      sol = tuple(map(lambda q: int(q) - 1, args.solution.split(',')))
      rows = len(sol)
      cols = max(sol) + 1
      print(f"{rows} x {cols}\n")
      print_board(sol)
      verify_solution = is_solution_verbose if args.verbose else is_solution
      print(verify_solution(sol))
    except ValueError:
      print("--solution must be a list of numbers separated by commas")
    exit(0)

  start = timer()

  is_solution_filter = queens_solutions(args.n, args.verbose)

  if args.first:
    is_solution_filter = [next(is_solution_filter, ())]
    if is_solution_filter[0] == ():
      is_solution_filter = ()
  
  if args.count_only:
    total = ilen(is_solution_filter)
  else:
    solutions = list(is_solution_filter)
    total = len(solutions)

  if not args.count_only:
    if args.verbose and not args.first:
      print()
    for qs in solutions:
      print_board(qs)
  
  end = timer()
  elapsed = end - start
  
  print(f"{total} solutions{f' in {elapsed} s' if elapsed > 1 else ''}")
