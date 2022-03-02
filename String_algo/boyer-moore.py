#!/usr/bin/env python3

from operator import le
import sys
from time import time

def check(str1: str, str2: str):
  for idx in range(len(str1)):
    if str1[idx] != str2[idx]:
      return False

  return True


def bad_char_preprocess(substr: str):
  bad_char_tbl = dict()
  
  for idx in range(0, len(substr)-1):
    bad_char_tbl[substr[idx]] = len(substr) - idx - 1
  
  return bad_char_tbl


def shift_bad_char_rule(substr: str, text: str, idx: int, bad_char_tbl, debug = False):
  shift = 0
  m = len(substr)
  j = m - 1

  while j >= 0 and substr[j] == text[idx + j]:
    j -= 1

  if j < 0:
    if idx + m < len(text):
      shift = m - bad_char_tbl.get(text[idx + m], 0)

  else:
    if idx + m < len(text):
      shift = j - bad_char_tbl.get(text[idx + m], 0)

  if debug:
    print(f"bad_char: idx = {idx}, j = {j}, shift = {shift}")

  return shift


def good_suffix_weak_preprocess(substr: str):
  border_position = [0] * (len(substr) + 1)
  shift_arr = [0] * (len(substr) + 1)

  idx = len(substr)
  j = len(substr) + 1

  border_position[idx] = j

  while idx > 0:
    while j <= len(substr) and substr[idx - 1] != substr[j - 1]:
      if shift_arr[j] == 0:
        shift_arr[j] = j - idx
      j = border_position[j]

    idx -= 1
    j -= 1
    border_position[idx] = j

  return border_position, shift_arr


def good_suffix_strong_preprocess(substr: str, border_position, shift_arr):
  j = border_position[0]
  for idx in range(0, len(substr) + 1):
    if shift_arr[idx] == 0:
      shift_arr[idx] = j
    if idx == j:
      j = border_position[j]

  return shift_arr


def shift_good_suffix_rule(substr: str, text: str, idx: int, shift_arr):
  shift = 0
  j = len(substr) - 1

  while j >= 0 and substr[j] == text[idx + j]:
    j-= 1

  if j < 0:
    shift = shift_arr[0]
  else:
    shift = shift_arr[j + 1]

  return shift


def boyer_moore(substr: str, text: str, debug = False):
  n = len(text)
  m = len(substr)
  idx = m # tail idx
  result = []

  bad_char_tbl = bad_char_preprocess(substr)
  border_pos, shift_arr = good_suffix_weak_preprocess(substr)
  shift_arr = good_suffix_strong_preprocess(substr, border_pos, shift_arr)

  while(idx <= n):
    if check(text[idx - m : idx], substr):
      if debug:
        print(f"Found at {idx - m}")

      result.append(idx - m)

    shift = max(1,\
                shift_bad_char_rule(substr, text, idx - m, bad_char_tbl), \
                shift_good_suffix_rule(substr, text, idx - m, shift_arr))

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





