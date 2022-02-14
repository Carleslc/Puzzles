#!/usr/bin/python3
# -*- coding: utf-8 -*-

# N queens problem
# https://en.wikipedia.org/wiki/Eight_queens_puzzle

# Brute-force permutations solver
# Time complexity O(N!)

# usage: queens-permutations.py [-h] [--n N] [--verbose] [--count-only] [--first]
#                  [--solution SOLUTION]

# optional arguments:
#   -h, --help           show this help message and exit
#   --n N                number of queens
#   --verbose            explain process
#   --count-only         do not display solutions
#   --first              stop at the first solution. Recommended for n > 9
#   --solution SOLUTION  verify a solution with columns separated by commas,
#                        e.g. 1,3,5,7,9,11,13,2,4,6,8,10,12

# NOTE: N = 12 needs 9 seconds of computation for the first solution or about 30 minutes for all solutions (dual-core CPU 2.9GHz)
# This program is not suitable for N >= 14 (20 minutes for first solution or more than 6 hours for all solutions)

import argparse
from timeit import default_timer as timer
from itertools import permutations
from functools import reduce

def set_args():
  parser = argparse.ArgumentParser()
  parser.add_argument("--n", help="number of queens", type=int, default=8)
  parser.add_argument("--verbose", action='store_true', help="explain process")
  parser.add_argument("--count-only", action='store_true', help="do not display solutions")
  parser.add_argument("--first", action='store_true', help="stop at the first solution. Recommended for n > 9")
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

def ilen(iterable):
  return reduce(lambda sum, _: sum + 1, iterable, 0)

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

  verify_solution = is_solution_verbose if args.verbose else is_solution

  if args.solution:
    try:
      queens = tuple(map(lambda q: int(q) - 1, args.solution.split(',')))
      rows = len(queens)
      cols = max(queens) + 1
      print(f"{rows} x {cols}\n")
      print_board(queens)
      print(verify_solution(queens))
    except ValueError:
      print("--solution must be a list of numbers separated by commas")
    exit(0)

  start = timer()

  # indices are rows, values are columns [0..7]
  # this ensures only one queen per row and column
  queens = range(args.n)

  candidates = permutations(queens)

  is_solution_filter = filter(verify_solution, candidates)

  if args.first:
    is_solution_filter = [next(is_solution_filter, ())]
    if is_solution_filter[0] == ():
      is_solution_filter = []
  
  if args.count_only:
    total = ilen(is_solution_filter)
  else:
    solutions = list(is_solution_filter)
    total = len(solutions)

  if not args.count_only:
    for qs in solutions:
      print_board(qs)
  
  end = timer()
  elapsed = end - start
  
  print(f"{total} solutions{f' in {elapsed} s' if elapsed > 1 else ''}")
