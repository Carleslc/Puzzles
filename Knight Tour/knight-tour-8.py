#!/usr/bin/python3
# -*- coding: utf-8 -*-

# KNIGHT'S TOUR PROBLEM
# https://en.wikipedia.org/wiki/Knight%27s_tour

# This program solves the problem using Warndsdorff's rule for a 8x8 board

# Further investigation: https://github.com/douglassquirrel/warnsdorff/blob/master/5_Squirrel96.pdf?raw=true

import argparse

import chess  # python-chess (https://python-chess.readthedocs.io/en/latest/)
import chess.svg

from app import App
from window import Window

def set_args():
  global args
  parser = argparse.ArgumentParser()
  parser.add_argument("--start", help="start position (default = a8)", default="a8")
  parser.add_argument("--reverse", action='store_true', help="reverse move ordering for another solution")
  parser.add_argument("--verbose", action='store_true', help="log moves")
  args = parser.parse_args()

def initial_board():
  global board, move_board, position, move_index
  board = chess.Board.empty()
  start = chess.__dict__.get(args.start.upper())
  position = start
  board.set_piece_at(start, chess.Piece(chess.KNIGHT, chess.WHITE))
  move_board = [None] * 64 # each square position is the n-th knight's move
  move_index = 0
  move(board, move_board, chess.Move(start, position)) # first move (start)
  return board

def print_board():
  for i in range(8):
    for j in range(8):
      print(move_board[(8 - i - 1) * 8 + j], end=' ')
    print()

def move(board, move_board, move):
  global move_index
  board.push(move)
  board.turn = chess.WHITE
  move_index += 1
  move_board[move.to_square] = move_index

def available_moves(move_board, moves):
  return list(filter(lambda position: move_board[position] is None, moves))

def warndsdorff_move(board, move_board, from_position):
  moves = available_moves(move_board, board.attacks(from_position))

  if len(moves) == 0:
    return None
  
  if args.reverse:
    moves.reverse()

  next_available_moves_count = []
  
  for to_position in moves:
    board.push(chess.Move(from_position, to_position))
    next_available_moves_count.append(len(available_moves(move_board, board.attacks(to_position))))
    board.pop()
  
  next_position = moves[next_available_moves_count.index(min(next_available_moves_count))]
  next_move = chess.Move(from_position, next_position)

  return next_move

def board_svg(board, squares = [], lastmove = None):
  return chess.svg.board(
    board=board,
    squares=chess.SquareSet(squares),
    lastmove=lastmove,
    coordinates=args.verbose).encode("UTF-8")

def next_board(board, move_board, position):
  next_move = warndsdorff_move(board, move_board, position)
  
  if next_move is None:
    if args.verbose:
      if len(list(filter(lambda check: check is None, move_board))) > 0:
        print("No moves available")
      print_board()
    return None, None

  move(board, move_board, next_move)
  
  if args.verbose:
    print(next_move)

  return next_move, board_svg(
    board=board,
    squares=[check[0] for check in enumerate(move_board) if check[1] is not None and check[0] != next_move.to_square],
    lastmove=next_move)

def display(svg):
  if svg is None:
    exit()
  app.window.display_svg(svg)

def display_next_board():
  global position
  next_move, svg = next_board(board, move_board, position)
  display(svg)
  position = next_move.to_square

if __name__ == "__main__":
  global app, board, move_board, position

  set_args()

  app = App()

  board = initial_board()

  display(board_svg(board))

  app.window.add_button("Next move", display_next_board)

  app.exec()
