#!/usr/bin/python3
# -*- coding: utf-8 -*-

# https://ktane.timwi.de/HTML/The%20Samsung.html
# https://ktane.timwi.de/HTML/The%20Samsung%20no%20copyright.html

'''
!x open <duo/maps/kindle/auth/photo/spotify/arts/discord/settings> [Opens the specified app]
!x play [Presses the play button if Spotify is open]
!x start [Presses the start button if Photomath is open]
!x mathsub <digits> [Presses the specified buttons 'digits' in reading order (0-9) and submits the input to Photomath if Photomath is open]
!x call <user> [Calls the specified user 'user' if Discord is open]
!x mute <#> [Presses the mute button when the last digit of the bomb's timer is '#' if Discord is open]
!x symbol <#> [Presses the mute button when the specified symbol '#' is shown if Discord is open]
!x color <col> [Presses the mute button when the specified color 'col' is shown if Discord is open]
!x home [Goes back to the home screen]
!x submit <digits> [Submits the pin 'digits']
!x regenerate [Presses the notification LED]
'''

from PIL import Image
from math import sqrt
from math_utils import digital_root
from utils import ask, input_number, input_float, print_info, print_command, capitalize_all, google_cloud_api_key

APPS = dict([(name, None) for name in ['Maps', 'Auth', 'Arts']]) # 'Duolingo', 'Maps', 'Kindle', 'Auth', 'Photo', 'Spotify', 'Arts', 'Discord'
PIN = ''

def remaining_apps():
  if not PIN:
    remaining = [name for name, digit in APPS.items() if digit is None]
    return remaining + ['Settings']
  return []

def input_app(apps, msg='App'):
  app_name = None
  while not app_name:
    app_name = input(f'{msg}: ').capitalize()
    if app_name not in apps:
      print_info('Invalid App')
      app_name = None
  return app_name

def print_apps(apps):
  print_info(', '.join(apps))

def save(name, digit):
  APPS[name] = digit
  print_info(f'{name}: {digit}')

def duolingo():
  # TODO
  save('Duolingo', 0)

def maps():
  import requests, webbrowser
  COUNTRIES = ['United States', 'Canada', 'Mexico', 'Russia', 'Germany', 'Australia', 'United Kingdom', 'China', 'Brazil', 'South Africa']
  def country_digit(country):
    return COUNTRIES.index(country)
  def input_coordinates(n):
    tag = f'Coordinates {n}'
    latitude = input_float(f'{tag} Latitude: ')
    longitude = input_float(f'{tag} Longitude: ')
    return (latitude, longitude)
  api_key = google_cloud_api_key()
  browser_opened = False
  def get_country(coords):
    nonlocal api_key, browser_opened
    country = None
    if api_key:
      print_info('Using Google Maps Geocode API')
      response = requests.get('https://maps.googleapis.com/maps/api/geocode/json', {
        'latlng': f'{coords[0]},{coords[1]}',
        'result_type': 'country',
        'key': api_key
      })
      if response:
        response = response.json()
        if response['status'] == 'OK':
          country = response['results'][0]['formatted_address']
          print_info(country)
          if country not in COUNTRIES:
            print_info('Invalid Country. Must be one of:')
            country = None
        else:
          print_info(response.status)
    if country is None:
      if not browser_opened:
        webbrowser.open(f'https://maps.google.com/maps/place/{coords[0]},{coords[1]}')
        browser_opened = True
      print_info(', '.join(COUNTRIES))
      print_info(f'Coordinates: {coords[0]}, {coords[1]}')
      countries_lower = set(map(str.lower, COUNTRIES))
      while country not in countries_lower:
        country = input('Country: ').lower()
      country = capitalize_all(country)
    return country
  country1 = get_country(input_coordinates(1))
  country2 = get_country(input_coordinates(2))
  save('Maps', digit = abs(country_digit(country1) - country_digit(country2)))

def kindle():
  # TODO
  save('Kindle', 0)

def auth():
  def process_code(n):
    code = input_number(f'Code {n}: ', 6)
    matches = set()
    root = digital_root(code)
    if root == 8: # digital root is 8
      matches.add(0)
    if sqrt(n) == int(sqrt(code)): # perfect square
      matches.add(1)
    if code % 7 == 0: # divisible by 7
      matches.add(2)
    if (code % 5) % 2 == 1: # modulo 5 is odd
      matches.add(3)
    if root == 3 or root == 4: # digital root is 3 or 4
      matches.add(4)
    if code % 6 == 0: # divisible by 6
      matches.add(5)
    if root == 7: # digital root is 7
      matches.add(6)
    if code % 9 == 0: # divisibile by 9
      matches.add(7)
    if root == 5: # digital root is 5
      matches.add(8)
    if (code % 6) % 2 == 0 or code ** (1/3) == int(code ** (1/3)): # modulo 6 is even (Standard version) or perfect cube (No Copyright version)
      matches.add(9)
    return matches
  matches1 = process_code(1)
  matches2 = process_code(2)
  matches3 = process_code(3)
  save('Auth', digit = max([d for d in matches1 if d in matches2 and d in matches3]))

def photo():
  # TODO
  save('Photo', 0)

def spotify():
  # TODO
  save('Spotify', 0)

def arts():
  ARTISTS = {
    'Bob Ross': (1942, 1995),
    'Picasso': (1881, 1973),
    'Da Vinci': (1452, 1519),
    'Van Gogh': (1853, 1890)
  }
  images = Image.open('./images/Samsung_Google_Arts.png')
  images.show()
  shown = ask('Is the displayed painting shown in the table?')
  if not shown:
    digit = input_number('Last digit of Serial#: ')
  else:
    pos = 0
    while pos == 0 or pos == 5:
      pos = input_number('Position of Arts app on the home screen (1-9 reading order): ', size=1)
    artist = ''
    artists = set(map(str.lower, ARTISTS.keys()))
    print_info(', '.join(ARTISTS.keys()))
    while artist.lower() not in artists:
      artist = input("Identify the artist of the painting: ")
    artist_name = capitalize_all(artist)
    artist = ARTISTS[artist_name]
    n = 0
    while n < 1 or n > 5:
      n = input_number("Displayed Painting's Index (1-5): ", size=1)
    if pos < 5:
      birth_year = str(artist[0])
      digit = int(birth_year[pos - 1])
    else:
      death_year = str(artist[1])
      digit = int(death_year[pos - 5 - 1])
    if ask(f'Displayed Artist is {artist_name}?'):
      digit += 2*(n - 1) # nth even digit in 0-9
    else: # lying
      digit += 2*(n - 1) + 1 # nth odd digit in 0-9
  save('Arts', digit % 10)

def discord():
  # TODO
  # 2x3 top-left https://ktane.timwi.de/HTML/Braille.html
  save('Discord', 0)

direction = None

def settings():
  global direction, PIN

  DIRECTIONS = {
    '0': 'North',
    '1': 'East',
    '2': 'South',
    '3': 'West'
  }

  def get_direction():
    serial = ''
    while len(serial) < 2:
      serial = input('First 2 characters of Serial#: ')
    serial = serial[:2]
    if serial[0].isdigit():
      if serial[1].isdigit():
        return 3
      return 2
    if serial[1].isdigit():
      return 1
    return 0
  
  if direction is None:
    # Obtain direction
    direction = get_direction()
    if ask('Parallel port?'):
      direction = ((direction - 1) + 4) % 4 # move counter-clockwise
    elif ask('Serial port?'):
      direction = (direction + 1) % 4 # move clockwise
  direction_name = DIRECTIONS[str(direction)]
  print_info(direction_name)

  def append_pin_digit(apps, msg):
    global PIN
    print_apps(apps)
    app = input_app(apps, msg)
    PIN += str(APPS[app])
    apps.remove(app)
  
  remaining = remaining_apps()
  if len(remaining) == 1 and remaining[0] == 'Settings':
    # Determine order of submission
    apps = set(APPS.keys())
    append_pin_digit(apps, f'App ({direction_name} from center)')
    while len(apps) > 1:
      append_pin_digit(apps, 'Next App (Clockwise)')
    if len(apps) == 1:
      PIN += str(APPS[apps.pop()])
    print_command(f'submit {PIN}')
  else:
    print_info('Solve remaining apps before submit')

if __name__ == "__main__":
  print_info('Enter the name of an app to enter')
  try:
    apps = remaining_apps()
    while apps:
      print_apps(apps)
      app_name = input_app(apps)
      app = globals().get(app_name.lower())
      app()
      apps = remaining_apps()
  except KeyboardInterrupt:
    pass
