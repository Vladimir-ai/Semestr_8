#!/usr/bin/env python3

from operator import le
import sys
from time import time
from typing import List

def reverse_check(str1: str, str2: str):
  idx = len(str1) - 1

  while idx >= 0 and str1[idx] == str2[idx]:
    idx -= 1

  return idx


def bad_char_preprocess(substr: str):
  bad_char_tbl = dict()

  for idx in range(0, len(substr)):
    if substr[idx] in bad_char_tbl:
      bad_char_tbl[substr[idx]].append(len(substr) - idx - 1)
    else:
      bad_char_tbl[substr[idx]] = [len(substr) - idx - 1]

  return bad_char_tbl


def shift_bad_char_rule(bad_char, idx: int, bad_char_tbl, debug = False):
  if idx < 0:
    return 1

  left_pos = -1

  for elem in bad_char_tbl.get(bad_char, list()):
    if elem < idx:
      left_pos = elem
      break

  return idx - left_pos


def good_suffix_weak_preprocess(substr: str):
  n = len(substr)
  bs = [0] * n

  for idx in range(n - 2, -1, -1):
    bs_left = bs[idx + 1]

    while bs_left and substr[idx] != substr[n - bs_left - 1]:
      bs_left = bs[n - bs_left]

    if substr[idx] == substr[n - bs_left - 1]:
      bs[idx] = bs_left + 1
    else:
      bs[idx] = 0

  return bs


def good_suffix_strong_preprocess(bs: List[int]):
  current_border = bs[0]
  k = 0
  border_shift = [0] * len(bs)

  while current_border:
    while k < len(bs) - current_border:
      border_shift[k] = current_border
      current_border = bs[k]
      k += 1

  while k < len(bs):
    border_shift[k] = 0
    k += 1

  return border_shift


def shift_good_suffix_rule(ns, br, bad_pos):
  if bad_pos == len(br) - 1:
    return 1

  if bad_pos < 0:
    return len(br) - br[0]

  copy_pos = ns[bad_pos]
  if copy_pos >= 0:
    shift = bad_pos - copy_pos + 1
  else:
    shift = len(bad_pos) - br[bad_pos]

  return shift


def boyer_moore(substr: str, text: str, debug = False):
  n = len(text)
  m = len(substr)
  idx = m # tail idx
  result = []

  bad_char_tbl = bad_char_preprocess(substr)
  border_pos = good_suffix_weak_preprocess(substr)
  shift_arr = good_suffix_strong_preprocess(border_pos)

  while(idx <= n):
    if (bad_pos := reverse_check(substr, text[idx - m : idx])) == -1:
      if debug:
        print(f"Found at {idx - m}")

      result.append(idx - m)

    shift = max(1,\
                # shift_bad_char_rule(substr[bad_pos], bad_pos, bad_char_tbl),
                shift_good_suffix_rule(shift_arr, border_pos, bad_pos))
    idx += shift

  return result


if __name__ == "__main__":
  if len(sys.argv) == 3:
    string = str(sys.argv[1])
    substring = str(sys.argv[2])
  else:
    string = str(input("Input string to search in: "))
    substring = str(input("Input substring to search for: "))

  t0 = time()
  result = boyer_moore(substring, string, True)
  t1 = time()

  print(result)





