#!/usr/bin/python3
# -*- coding: utf-8 -*-

# https://ktane.timwi.de/HTML/Prime%20Checker.html

from math_utils import is_prime
from utils import input_number, print_info, print_command

if __name__ == "__main__":
  try:
    print_info('Press Ctrl+C to cancel')
    while True:
      n = input_number('Number: ')
      if is_prime(n):
        print_command('prime')
      else:
        print_command('not prime')
  except KeyboardInterrupt:
    pass
