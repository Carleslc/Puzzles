#!/usr/bin/python3
# -*- coding: utf-8 -*-

# https://ktane.timwi.de/HTML/Funny%20Numbers.html

from math import sqrt
from math_utils import get_divisors, is_prime
from utils import input_number, print_info, print_command

FUNNY = [3, 13, 21, 25, 69, 420, 522, 777, 789, 6969, 8008, 42069, 58008, 69420, 80085]
UNFUNNY = [1, 19, 24, 68, 70, 419, 421, 666, 2003]

def is_funny(n):
  if n in FUNNY:
    return True
  elif n == 0 or n in UNFUNNY:
    return False
  funni = 0
  # If the number is a multiple of any funny number, add 1 funni point per funny number that it is divisible by.
  funni += len(get_divisors(n, FUNNY))
  # If the number is a multiple of any unfunny number, subtract 1 funni point per unfunny number that it is divisible by.
  funni -= len(get_divisors(n, UNFUNNY))
  # If the number is even subtract one funni point, otherwise add 1 funni point.
  if n % 2 == 0:
    funni -= 1
  else:
    funni += 1
  # If the number is prime, add 2 funni points.
  if is_prime(n):
    funni += 2
  # If the number is a perfect square, subtract 2 funni points.
  root = sqrt(n)
  if root == int(root):
    funni -= 2
    # If the number is the square of a prime, subtract another 2 funni points.
    if is_prime(root):
      funni -= 2
  print_info(funni)
  return funni > 0

if __name__ == "__main__":
  try:
    print_info('Press Ctrl+C to cancel')
    for i in range(5):
      if is_funny(input_number('Number: ')):
        print_command('funny')
      else:
        print_command('unfunny')
  except KeyboardInterrupt:
    pass
