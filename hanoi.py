#!/usr/bin/python3
# -*- coding: utf-8 -*-

# HANOI'S TOWER
# Minimum number of moves: 2^n + 1

import argparse

def set_args():
  global args
  parser = argparse.ArgumentParser()
  parser.add_argument("n", type=int, help="number of discs")
  args = parser.parse_args()

def hanoi(n, src, dest, aux):
  if n == 1:
    print(f"{src} -> {dest}")
    return 1
  else:
    moves = hanoi(n - 1, src, aux, dest)
    moves += hanoi(1, src, dest, aux)
    moves += hanoi(n - 1, aux, dest, src)
    return moves

if __name__ == "__main__":
  set_args()

  print(f"Moves: {hanoi(args.n, 1, 3, 2)}")
