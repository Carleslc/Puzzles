#!/usr/bin/python3
# -*- coding: utf-8 -*-

# https://ktane.timwi.de/HTML/Memory.html

from utils import input_number, print_info, print_command

class Button:
  def __init__(self, position, label):
    self.position = position
    self.label = label

stages = []

def next_button(position, label):
  stages.append(Button(position, label))

def get_button(stage):
  return stages[stage - 1]

def display(stage):
  print_info(f'Stage {stage}')
  return int(input('Display: '))

def get_position_or_label(stage, display):
  position = None
  label = None
  if stage == 1:
    position = 2 if display <= 2 else display
  elif stage == 2:
    if display == 1:
      label = 4
    elif display == 3:
      position = 1
    else:
      position = get_button(1).position
  elif stage == 3:
    if display == 1:
      label = get_button(2).label
    elif display == 2:
      label = get_button(1).label
    elif display == 3:
      position = 3
    else:
      label = 4
  elif stage == 4:
    if display == 1:
      position = get_button(1).position
    elif display == 2:
      position = 1
    else:
      position = get_button(2).position
  else:
    if display == 3:
      label = get_button(4).label
    elif display == 4:
      label = get_button(3).label
    else:
      label = get_button(display).label
  return position, label

def solve(stage):
  position, label = get_position_or_label(stage, display(stage))
  if position is not None:
    print_command(f'position {position}')
  else:
    print_command(f'label {label}')
  if stage < 5:
    if label is None:
      label = input_number('Label: ', size=1)
    else:
      position = input_number('Position: ', size=1)
    next_button(position, label)
    print()

if __name__ == "__main__":
  try:
    for stage in range(1, 6):
      solve(stage)
  except KeyboardInterrupt:
    pass
