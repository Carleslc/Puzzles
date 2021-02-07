#!/usr/bin/python3
# -*- coding: utf-8 -*-

# PUZZLE 148 from Professor Layton and the Diabolical Box
# What are the valid products including all digits from 1 to 8 in the form ABC x D = EFGH ?

from itertools import permutations

def test(s):
  return (s[0] * 100 + s[1] * 10 + s[2]) * s[3] == (s[4] * 1000 + s[5] * 100 + s[6] * 10 + s[7])

def print_solution(s):
  print(f"{s[0]}{s[1]}{s[2]} x {s[3]} = {s[4]}{s[5]}{s[6]}{s[7]}")

if __name__ == "__main__":
  digits = range(1, 9) # 1 (included) to 9 (excluded)

  for s in filter(test, permutations(digits)):
    print_solution(s)
