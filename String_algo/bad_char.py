#!/usr/bin/env python3
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


def bad_char(substr: str, text: str, debug = False):
  n = len(text)
  m = len(substr)
  idx = m # tail idx
  result = []

  bad_char_tbl = bad_char_preprocess(substr)
  
  while(idx <= n):
    if check(text[idx - m : idx], substr):
      if debug:
        print(f"Found at {idx - m}")

      result.append(idx - m)

    shift = max(1, shift_bad_char_rule(substr, text, idx - m, bad_char_tbl))

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
  result = bad_char(substring, string, True)
  t1 = time()

  print(result)
