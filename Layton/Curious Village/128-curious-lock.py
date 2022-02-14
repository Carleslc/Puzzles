#!/usr/bin/python3
# -*- coding: utf-8 -*-

# Professor Layton: Curious Village (Puzzle 128)
# https://layton.fandom.com/wiki/Puzzle:Number_Lock

"""
The door in front of you has an odd lock mounted on the front.
The only way to unlock this strange contraption is to place small tiles labeled from one to nine in its slots.
The lock will open when the numbers on the lock equal the same number when multiplied vertically and horizontally.

There are nine tiles, but the lock only has seven slots, so you won't need two tiles.

Can you open the lock?

  A   E
  B D F
  C   G

A * B * C = E * F * G = B * D * F
"""

from itertools import permutations

# Declare positions
A = 0;        E = 4;
B = 1; D = 3; F = 5;
C = 2;        G = 6;

# Some math to make it easier to find a solution 
# D * F = A * C
# B * D = E * G
def is_solution(lock):
  return lock[D] * lock[F] == lock[A] * lock[C] \
    and lock[B] * lock[D] == lock[E] * lock[G]

def print_lock(lock):
  print(f"{lock[A]}   {lock[E]}\n{lock[B]} {lock[D]} {lock[F]}\n{lock[C]}   {lock[G]}")

if __name__ == "__main__":
  digits = range(1, 10)

  candidates = permutations(digits, 7)
  solutions = list(filter(is_solution, candidates))

  total = len(solutions)
  print(f"{total} solutions\n")

  for i, lock in enumerate(solutions):
    print_lock(lock)
    if i != total - 1:
      print()
