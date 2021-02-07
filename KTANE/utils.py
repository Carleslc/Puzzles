import sys
from colorama import Fore, Style, init as init_console_colors

def google_cloud_api_key():
  import os
  key_filename = '.googleapikey'
  if os.path.isfile(key_filename):
    with open(key_filename) as google_api_key_file:
      return google_api_key_file.read().strip()
  return None

def get_id():
  if 'args' in globals():
    return args.id if 'id' in args else None
  if len(sys.argv) > 1:
    return sys.argv[1]
  return None

def print_id():
  identifier = get_id()
  if identifier is not None:
    print(f'!{identifier}', end=' ')

def print_styled(style, *s):
  print(f"{style}{' '.join(map(str, s))}{Style.RESET_ALL}")

def print_command(*s):
  init_console_colors()
  print(Fore.GREEN, end='')
  print_id()
  print_styled(Style.BRIGHT, *s)

def print_info(*s):
  init_console_colors()
  print_styled(Style.DIM, *s)

def input_number(msg, size=None):
  n = ''
  while not n.isdigit() or (size is not None and len(n) != size):
    n = input(msg)
  return int(n)

def input_float(msg):
  while True:
    try:
      return float(input(msg))
    except ValueError:
      pass

def ask(msg):
  ans = ''
  while not ans or (ans[0] != 'y' and ans[0] != 'n'):
    ans = input(f'{msg} (y/n): ').lower()
  return ans[0] == 'y'

def capitalize_all(s):
  return ' '.join(map(str.capitalize, s.split()))
