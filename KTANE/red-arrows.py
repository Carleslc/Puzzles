#!/usr/bin/python3
# -*- coding: utf-8 -*-

# https://ktane.timwi.de/HTML/Red%20Arrows.html

import numpy as np
from collections import deque
from utils import print_info, print_command

# n = m = 10
# Size (2n-1)*(2m-1)
MAZE = [
  '.......#.#........0',
  '.###.#.#.#.###.####',
  '.#...#...#...#.....',
  '.#.#####.#.#.#####.',
  '1#.#..4#...#.#...#6',
  '##.#.###.#####.#.##',
  '...#...........#...',
  '.#########.#######.',
  '..9#.....#.#....3#.',
  '####.###.#.#.#####.',
  '...#.#5#...#.......',
  '.#.#.#.###########.',
  '.#...#.......#7....',
  '.###########.#####.',
  '.#.....#..8#.....#.',
  '.#.###.#.#######.#.',
  '.#.#...#.......#...',
  '.#.#.#########.###.',
  '...#......2#.......'
]
N = len(MAZE) # 2n-1 = 2m-1

DIRECTIONS = {
  (-1, 0): 'u',
  (0, 1): 'r',
  (1, 0): 'd',
  (0, -1): 'l'
}

def get(pos):
  return MAZE[pos[0]][pos[1]]

def get_direction(start, end):
  return DIRECTIONS.get((end[0] - start[0], end[1] - start[1]))

def search_position(d):
  for row in range(N):
    for col in range(N):
      pos = (row, col)
      if get(pos) == d:
        return pos

def get_neighbours(pos):
  neighbours = []
  for dir_pos in DIRECTIONS.keys():
    row = pos[0] + dir_pos[0]
    col = pos[1] + dir_pos[1]
    neighbour = (row, col)
    if row >= 0 and col >= 0 and row < N and col < N and get(neighbour) != '#':
      neighbours.append(neighbour)
  return neighbours

def get_path(start, end):
  start_pos = search_position(start)
  end_pos = search_position(end)

  if start_pos is None or end_pos is None:
    return None

  previous = np.empty((N, N), dtype=tuple)
  previous[start_pos[0]][start_pos[1]] = start_pos

  def build_path():
    path = []
    current = end_pos
    prev = previous[current[0]][current[1]]
    while current != start_pos:
      path.append((get_direction(prev, current), current))
      current = prev
      prev = previous[current[0]][current[1]]
    path.append(('S', start_pos))
    path.reverse()
    return path

  # BFS
  q = deque()
  q.append(start_pos)
  
  while len(q) > 0:
    pos = q.popleft()
    for neighbour in get_neighbours(pos):
      if previous[neighbour[0]][neighbour[1]] is None:
        previous[neighbour[0]][neighbour[1]] = pos
        if neighbour == end_pos:
          return build_path()
        q.append(neighbour)

if __name__ == "__main__":
  start = input('Display: ')
  end = input('Last digit of Serial#: ')
  path = get_path(start, end)
  if path:
    dir_path = ''.join(list(map(lambda dir_pos: dir_pos[0], path[1:])))
    dir_path = dir_path[::2] # Remove wall rows/cols
    print_command(dir_path)
  else:
    print_info('No solution. Please check input.')
