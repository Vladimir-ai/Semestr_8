#! /usr/bin/env python3

from time import time
from sys import argv


def compare_str_concurrent(string: str, str_ptr_1: int, str_ptr_2: int):
  length = 0

  while str_ptr_1 < len(string) and str_ptr_2 < len(string) and string[str_ptr_1] == string[str_ptr_2]:
    length += 1
    str_ptr_1 += 1
    str_ptr_2 += 1

  return length


def generate_z_blocks(string: str):
  str_len = len(string)
  right_ptr = 0
  left_ptr = 0

  zb = [0] * str_len

  for idx in range(1, str_len):
    if (idx >= right_ptr):
      zb[idx] = compare_str_concurrent(string, 0, idx)
      left_ptr = idx
      right_ptr = left_ptr + zb[idx]

    else:
      j = idx - left_ptr

      if zb[j] < right_ptr - idx:
        zb[idx] = zb[j]

      else:
        zb[idx] = right_ptr - idx + compare_str_concurrent(string, right_ptr - idx, right_ptr)
        left_ptr = idx
        right_ptr = left_ptr + zb[idx]

  return zb


def find_all_entries(string: str, substring: str):
  next_symbol = chr(ord(max(string + substring)) + 1)
  zb = generate_z_blocks(substring + next_symbol + string)

  indexes = list()
  for idx, zblock in enumerate(zb):
    if zblock == len(substring):
      indexes.append(idx - len(substring) - 1)

  return indexes


if __name__ == '__main__':
  if len(argv) == 3:
    string = str(argv[1])
    substring = str(argv[2])
  else:
    string = str(input("Input string to search in: "))
    substring = str(input("Input substring to search for: "))

  t0 = time()
  indexes = find_all_entries(string, substring)
  t1 = time()

  print(f"Substring indexes: {indexes}")
  print(f"Running time: {t1 - t0}")



