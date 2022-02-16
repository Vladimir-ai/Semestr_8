#! /usr/bin/env python3

from time import time
import sys

prime_63_bit = 9223372036854775783


def gorner_2_mod (string: str, m, q):
  res = 0
  for i in range(m):
    res = (res * 2 + ord(string[i])) % q
  return res


def karp_rabin(substr: str, string: str, q):
  m = len(substr)
  n = len(string)
  occur_list = list()
  p2m = 1

  for i in range(m - 1):
    p2m = (p2m * 2) % q

  hash_substr = gorner_2_mod(substr, m, q)
  hash_str = gorner_2_mod(string, m, q)

  for j in range(n - m + 1):
    if hash_str == hash_substr:
      k = 0
      while k < m and substr[k] == string[j+k]:
        k += 1

      if k == m:
        occur_list.append(j)

    if j + m < n :
      hash_str = ((hash_str - p2m * ord(string[j])) * 2 + ord(string[j + m])) % q

    if hash_str < 0:
      hash_str += q

  return occur_list


if __name__ == '__main__':
  assert sys.maxsize >= prime_63_bit, "Prime is greater than one memory cell"

  if len(sys.argv) == 3:
    string = str(sys.argv[1])
    substring = str(sys.argv[2])
  else:
    string = str(input("Input string to search in: "))
    substring = str(input("Input substring to search for: "))

  t0 = time()
  result = karp_rabin(substring, string, prime_63_bit)
  t1 = time()

  print(f"The result is {result}")
  print(f"Running time: {t1 - t0}")