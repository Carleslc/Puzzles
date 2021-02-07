#!/usr/bin/python3
# -*- coding: utf-8 -*-

# https://ktane.timwi.de/HTML/Seven%20Deadly%20Sins.html

from colorama import Fore
from utils import print_info, print_command

class Sin:
  def __init__(self, name, color, fore):
    self.name = name
    self.color = color
    self.fore = fore
  
  def colored_name(self):
    return f'{self.fore}{self.name}{Fore.RESET}'
  
  def __repr__(self):
    tab = '\t\t' if len(self.color) < 5 else '\t'
    return f'{self.fore}{self.color}{Fore.RESET}{tab}{self.name}'

SINS = {
  '1': Sin('Lust', 'Yellow', Fore.LIGHTYELLOW_EX),
  '2': Sin('Gluttony', 'Lilac', Fore.MAGENTA),
  '3': Sin('Greed', 'Red', Fore.LIGHTRED_EX),
  '4': Sin('Sloth', 'Light Red', Fore.LIGHTMAGENTA_EX),
  '5': Sin('Wrath', 'Light Green', Fore.LIGHTGREEN_EX),
  '6': Sin('Envy', 'Dark Green', Fore.GREEN),
  '7': Sin('Pride', 'Brown', Fore.RED)
}

GRAPH = {
  '1': [2, 3, 5],
  '2': [3, 4, 6],
  '3': [4, 5, 7],
  '4': [5, 6, 1],
  '5': [6, 7, 2],
  '6': [7, 1, 3],
  '7': [1, 2, 4]
}

def is_next(a, b):
  return int(b) in GRAPH[str(a)]

def get_adjacents(buttons, button):
  N = len(buttons)
  pos = buttons.index(button)
  return set([buttons[(pos + 1) % N], buttons[(pos - 1) % N]])

def get_available_adjacents(buttons, pressed):
  adjacents = set()
  for button in pressed:
    adjacents.update(get_adjacents(buttons, button))
  return [adjacent for adjacent in adjacents if adjacent not in pressed]

def solve(buttons):
  if len(set(buttons)) != len(buttons):
    return []
  def get_order(pressed):
    adjacents = get_available_adjacents(buttons, pressed)
    candidates = adjacents if pressed else buttons
    for candidate in candidates:
      if not pressed or is_next(pressed[-1], candidate):
        order = get_order(pressed + [candidate])
        if len(order) == len(buttons):
          return order
    return pressed
  return get_order([])

def get_positions(order, buttons):
  return map(lambda button: order.index(button) + 1, buttons)

def get_sin_list(buttons):
  return ' '.join(map(lambda n: SINS[n].colored_name(), list(buttons)))

if __name__ == "__main__":
  print_info('Input buttons order going clockwise starting from the top left.')
  print_info('\n'.join(map(lambda sin: f'{sin[0]}: {sin[1]!r}', SINS.items())))
  buttons = input('Current buttons order: ').replace(' ', '')
  print_info(get_sin_list(buttons))
  solution = solve(buttons)
  if solution:
    print_command(f"press {' '.join(list(map(str, get_positions(buttons, solution))))}")
    print(get_sin_list(solution))
  else:
    print_info('No solution. Please check input.')
