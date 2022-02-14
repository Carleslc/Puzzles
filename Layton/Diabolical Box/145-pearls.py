#!/usr/bin/python3
# -*- coding: utf-8 -*-

# Professor Layton: Diabolical Box (Puzzle 145)
# https://layton.fandom.com/wiki/Puzzle:500_Pearls

"""
A wizard sets a traveller a strange task.

"There are 500 pearls in that cave.
I want you to go and bring a certain number of pearls to me.
This number allows you to divide the pearls into groups of 2, 3, 4, 5, 6 or 7 and always have one pearl left over.
Bring me this number of pearls and you can keep them all for yourself!"

The traveller tries his best, but the number of pearls he bring back doesn't leave one pearl over when divided into groups of 4.
How many pearls did he bring to the wizard?
"""

import argparse

def set_args():
    global args
    parser = argparse.ArgumentParser()
    parser.add_argument("--pearls", type=int, default=500, help="total of pearls")
    args = parser.parse_args()

def test(n, g):
  return n % g == 1

if __name__ == "__main__":
    set_args()

    print([i for i in range(1, args.pearls + 1) if test(i, 2) and test(i, 3) and not test(i, 4) and test(i, 5) and test(i, 6) and test(i, 7)])
