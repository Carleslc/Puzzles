#!/usr/bin/python3
# -*- coding: utf-8 -*-

# PUZZLE 145 from Professor Layton and the Diabolical Box

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
