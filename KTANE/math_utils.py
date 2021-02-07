def get_divisors(n, candidates):
  return list(filter(lambda d: n % d == 0, candidates))

def is_prime(n):
  if n <= 3:
    return n > 1
  if n % 2 == 0 or n % 3 == 0:
    return False
  d = 5
  while d*d <= n:
    if n % d == 0 or n % (d + 2) == 0:
      return False
    d += 6
  return True

def get_primes():
  yield 2
  yield 3
  n = 5
  while True:
    if is_prime(n):
      yield n
    if is_prime(n + 2):
      yield n + 2
    n += 6

def get_prime(n):
  prime_generator = get_primes()
  i = 1
  while i < n:
    next(prime_generator)
    i += 1
  return next(prime_generator)

def sum_digits(n):
  total = 0
  while n > 0:
    total += n % 10
    n //= 10
  return total

def digital_root(n):
  root = sum_digits(n)
  while root >= 10:
    root = sum_digits(root)
  return root

def int_val(char):
  if type(char) != str or len(char) != 1:
    raise ValueError(f'{char} is not a char')
  if char >= '0' and char <= '9':
    return ord(char) - ord('0')
  if char >= 'a' and char <= 'z':
    return ord(char) - ord('a') + 10
  if char >= 'A' and char <= 'Z':
    return ord(char) - ord('A') + 10
  return ord(char)

def char_val(n):
  if type(n) != int:
    raise ValueError(f'{n} is not an integer')
  if n < 0:
    n = -n
  if n >= 0 and n <= 9:
    return str(n)
  return chr(n - 10 + ord('a'))

def change_base(n, from_b=10, to_b=10):
  if from_b == to_b:
    return n
  if from_b == 10:
    n = int(n)
    if n == 0:
      return '0'
    if to_b == 2:
      return bin(n)
    if to_b == 8:
      return oct(n)
    if to_b == 16:
      return hex(n)
    negative = n < 0
    if negative:
      n = -n
    result = ''
    while n > 0:
      result = char_val(n % to_b) + result
      n = n // to_b
  elif to_b == 10:
    n = str(n)
    negative = n[0] == '-'
    if negative:
      n = n[1:]
    result = 0
    base_multiplier = 1 # from_b ** 0
    i = 1
    while i <= len(n):
      d_value = int_val(n[-i])
      if d_value >= from_b:
        raise ValueError(f'{n} is not a number in base {from_b}')
      result += base_multiplier*d_value
      base_multiplier *= from_b
      i += 1
    result = str(result)
  else:
    n = change_base(n, from_b, to_b=10)
    return change_base(n, from_b=10, to_b=to_b)
  if negative:
    result = '-' + result
  return result
