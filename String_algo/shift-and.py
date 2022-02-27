#!/usr/bin/env python3

from time import time
import sys


def to_bin(n: int, width=16):
  s = bin(n).replace("0b", "")
  return (("%0" + str(width) + "d") % int(s))


def shift_and(substr: str, text: str, trace=False):
  m = len(substr)
  n = len(text)
  result = []

  bitmask_table = {}
  for i in range(m):
      bitmask_table[substr[i]] = (bitmask_table.get(substr[i], 0) | (1 << i))

  for key in bitmask_table:
    print(f"{key}: {to_bin(bitmask_table[key])}")

  # search
  matrix = 0
  for i in range(n):
      # use 1 bit and left shift because of dynamic ints in python
      matrix = ((matrix << 1) | 1) & (bitmask_table.get(text[i], 0))

      if trace:
          print(f"{to_bin(matrix, m)} & bitmask_table[{text[i]}] : {to_bin(bitmask_table.get(text[i], 0), m)}")

      if matrix & (1 << (m - 1)):
          result.append(i - m + 1)
          print(f"Found at {i - m + 1}")
  return result


if __name__ == "__main__":
  if len(sys.argv) == 3:
    string = str(sys.argv[1])
    substring = str(sys.argv[2])
  else:
    string = str(input("Input string to search in: "))
    substring = str(input("Input substring to search for: "))

  t0 = time()
  result = shift_and(substring, string, True)
  t1 = time()

  print(result)