#!/usr/bin/env python3

from matplotlib.backends.backend_pdf import PdfPages
from typing import List
import matplotlib.pyplot as plt
import sys


def get_next(u: int, m: int, p: int):
  u_next = u * m % p
  r_next = u_next / p
  return u_next, r_next


def generate_int(u0: int, m: int, p: int, n: int):
  res = [0] * n
  u = u0
  res[0] = u

  for idx in range(1, n):
    u, _ = get_next(u, m, p)
    res[idx] = u

  return res


def generate_float(u0: int, m: int, p: int, n: int):
  res = [0.0] * n
  u = u0
  res[0] = u

  for idx in range(1, n):
    u, res[idx] = get_next(u, m, p)

  return res


def compute_aperiodic_len(u0: int, m: int, p: int):
  res = set()
  aperiodic_len = 0
  u = u0

  while u not in res:
    aperiodic_len += 1
    res.add(u)
    u, _ = get_next(u, m, p)

  return aperiodic_len


def get_expected_val(u0: int, m: int, p: int, n: int):
  ret = generate_float(u0, m, p, n)
  return sum(ret) / len(ret)


def line_indexN(array: List[float], q: int):
  part_id = 0x00
  power = 0x01

  for num in array[::-1]:
    part_id += int(num * q) * power
    power *= q

  if part_id >= power:
    return power - 1
  else:
    return part_id


def compute_xi2(random_array: List[int], q: int = 2, dims: int = 3):
  n = len(random_array) // dims
  m = int(pow(q, dims))
  idx = 0x00
  points_num_list = [0] * m
  xi2 = 0.0
  n_by_m = n / m

  while idx < dims * n:
    p1 = line_indexN(random_array[idx:idx + dims], q)

    points_num_list[line_indexN(random_array[idx:idx + dims], q)] += 1
    idx += dims

  for point_count in points_num_list:
    xi2 += pow(point_count - n_by_m, 2)

  return xi2 / n_by_m


def compute_min_xi2(start_idx: List[int], m: int, p: int, length: int):
  min_xi2 = sys.float_info.max
  u0 = 0

  for u in start_idx:
    xi2 = compute_xi2(generate_float(u, m, p, length))

    if xi2 < min_xi2:
      min_xi2 = xi2
      u0 = u

  return u0, xi2


def generate_table(start_idx: List[int], m: int, p: int, length: int):
  xi2_arr = [0] * len(start_idx)
  idx = 0

  for u in start_idx:
    xi2_arr[idx] = compute_xi2(generate_float(u, m, p, length))
    idx += 1

  return xi2_arr


if __name__ == '__main__':
  p = 10007
  m = 28
  length = 20000

  # print(get_expected_val(7, 5, 10007, 100000))

  min_xi2 = sys.float_info.max
  best_u0 = 0

  result_arr = generate_table(range(1, p), m, p, length)

  with PdfPages('plot.pdf') as export_pdf:
    plt.scatter(range(1, p), result_arr)
    plt.grid(True)

    export_pdf.savefig()
    plt.close()

  print(best_u0)
  print(min_xi2)

