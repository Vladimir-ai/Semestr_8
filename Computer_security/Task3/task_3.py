#!/usr/bin/env python3

import matplotlib
matplotlib.use('QtAgg')

from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cm
from typing import List

import numpy as np
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
  res[0] = u / p

  for idx in range(1, n):
    u, res[idx] = get_next(u, m, p)

  return res


def compute_aperiodic_len(u0: int, m: int, p: int):
  elem_dict = dict()
  aperiodic_len = 0
  current_idx = 0
  u = u0

  while True:
    if u not in elem_dict.keys():
      elem_dict[u] = current_idx
    else:
      aperiodic_len = len(elem_dict) - elem_dict[u]
      break

    current_idx += 1
    u, _ = get_next(u, m, p)

  return aperiodic_len, len(elem_dict)


def get_expected_val_from_seq(sequence: List[float]):
  return np.mean(sequence)


def get_dispersion_from_seq(sequence: List[float]):
  exp_val = get_expected_val_from_seq(sequence)

  square_sum = 0.0

  for variable in sequence:
    square_sum += pow(variable - exp_val, 2)

  return square_sum / len(sequence)


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


def get_points_for_cube(random_array: List[int], q: int = 6, dims: int = 2):
  n = len(random_array) // dims
  m = int(pow(q, dims))
  idx = 0x00
  points_num_list = [0] * m

  while idx < dims * n:
    points_num_list[line_indexN(random_array[idx:idx + dims], q)] += 1
    idx += dims

  return points_num_list


def compute_xi2(random_array: List[float], q: int = 6, dims: int = 2):
  n = len(random_array) // dims
  m = int(pow(q, dims))
  idx = 0x00
  xi2 = 0.0
  n_by_m = n / m

  points_num_list = get_points_for_cube(random_array, q, dims)

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


def generate_table(start_idx: List[int], m: int, p: int, length: int, func):
  xi2_arr = [0] * len(start_idx)
  idx = 0

  for u in start_idx:
    xi2_arr[idx] = func(generate_float(u, m, p, length))
    idx += 1

  return xi2_arr


def plot_3d_graph(arr: List[int], size: int):
  xs = []
  for idx in range(size):
    xs.extend([idx + 1] * size)

  ys = [i for i in range(1, size + 1)] * size


  cmap = cm.get_cmap('hot')
  norm = colors.Normalize(min(arr), max(arr))
  colors_arr = [cmap(norm(num)) for num in arr]

  fig = plt.figure(1)
  ax = fig.add_subplot(121, projection='3d')
  ax.bar3d(xs, ys, np.zeros(size * size), np.ones(size * size), np.ones(size * size), arr, color=colors_arr)
  ax.grid(True)

  ax.set_xticks(np.arange(1, size + 1, 1))
  ax.set_yticks(np.arange(1, size + 1, 1))
  ax.set_zticks(np.arange(1, max(arr), max(arr) // 3))

  ax.set_xlabel('Row')
  ax.set_ylabel('Column')
  ax.set_zlabel('Occurrence')


def plot_2d_graph(arr: List[int], size: int):
  x_array = []

  for idx in range(size):
    x_array.extend([idx + 1.5] * size)

  y_array = [i + 0.5 for i in range(1, size + 1)] * size

  fig = plt.figure(2)
  ax = fig.add_subplot()
  ax.axis([1, size + 1, 1, size + 1])
  ax.grid(True)

  ax.set_xticks(np.arange(1, size + 1, 1))
  ax.set_yticks(np.arange(1, size + 1, 1))

  ax.set_xlabel('Row')
  ax.set_ylabel('Column')
  ax.scatter(x_array, y_array, c=arr, cmap="gist_yarg")


def plot_exp_val_xi2_dispersion(u_values: List[int], m: int, p: int, length: int, q: int = 2, dims: int = 3):
  mean_values = []
  dispersion = []
  xi2 = []
  aperiodic_len = []
  unique_len = []

  for u in u_values:
    res = generate_float(u, m, p, length)
    mean_values.append(get_expected_val_from_seq(res))
    dispersion.append(get_dispersion_from_seq(res))
    xi2.append(compute_xi2(res, q, dims))
    aper_len, uni_len = compute_aperiodic_len(u, m, p)
    aperiodic_len.append(aper_len)
    unique_len.append(uni_len)

    print(f"{u}, {mean_values[-1]}, {dispersion[-1]}, {xi2[-1]}, {aper_len}, {uni_len}")

  print(res)

  fig_mean_val = plt.figure(3)
  mean_val_fig = fig_mean_val.add_subplot()
  mean_val_fig.grid(True)
  mean_val_fig.set_title(f"Mean values for {length}")
  mean_val_fig.set_xlabel("u0")
  mean_val_fig.set_ylabel("Mean value")
  mean_val_fig.scatter(u_values, mean_values)

  fig_dispersion = plt.figure(4)
  disp_fig = fig_dispersion.add_subplot()
  disp_fig.grid(True)
  disp_fig.set_title(f"Dispersion for length {length}")
  disp_fig.set_xlabel("u0")
  disp_fig.set_ylabel("Dispersion")
  disp_fig.scatter(u_values, dispersion)

  fig_xi2 = plt.figure(5)
  xi2_fig = fig_xi2.add_subplot()
  xi2_fig.grid(True)
  xi2_fig.set_title(f"xi2 for length {length}")
  xi2_fig.set_xlabel("u0")
  xi2_fig.set_ylabel("xi2 value")
  xi2_fig.scatter(u_values, xi2)


def plot_2d_histogram(arr: List[int]):
  fig = plt.figure(6)
  ax = fig.add_subplot()
  # ax.axis([1, len(arr) + 1, 1, len(arr) + 1])
  # ax.grid(True)

  # ax.set_xticks(np.arange(1, len(arr) + 1, 1))
  # ax.set_yticks(np.arange(1, max(arr) + 1, max(arr) // 100))
  # ax.set_ylim(len(arr));
  ax.set_xlabel('Count')
  ax.set_ylabel('Point Num')
  ax.bar(np.arange(1, len(arr) + 1, 1), arr, align='center')


def find_min_xi2_disp_mean(u_range: int, m_range: int, p: int, length: int, q:int = 2, dims: int = 3, debug = True):
  min_xi2 = 1000
  min_dispersion = 1000
  min_mean = 1

  min_arr = [None] * 3

  for m in m_range:
    for u in u_range:
      int_vect = generate_int(u, m, p, length)
      float_vect = [num / p for num in int_vect]

      temp_xi2 = compute_xi2(float_vect, q, dims)
      temp_mean = get_expected_val_from_seq(float_vect)
      temp_dispersion = get_dispersion_from_seq(float_vect)
      aper_len, uni_len = compute_aperiodic_len(u, m, p)

      if debug:
        print(f"{u}, {temp_mean}, {temp_dispersion}, {temp_xi2}, {aper_len}, {uni_len}, {m}")

      if temp_xi2 < min_xi2:
        min_xi2 = temp_xi2
        min_arr[0] = (u, m)

      if temp_dispersion < min_dispersion:
        min_dispersion = temp_dispersion
        min_arr[1] = (u, m)

      if abs(temp_mean - 0.5) < abs(min_mean - 0.5):
        min_mean = temp_mean
        min_arr[2] = (u, m)

      if (uni_len != p - 1):
        break


  print(min_arr)


if __name__ == '__main__':
  p = 10007
  m = 214 # 28
  length = 15000
  u0 = 1275
  q = 6
  dims = 2

  # print(get_expected_val(7, 5, 10007, 100000))


  # find_min_xi2_disp_mean(range(1, p // 2), range(2, p // 2), p, length, q, dims)

  plot_exp_val_xi2_dispersion(range(1, p), m, p, length)

  random_vect = generate_float(u0, m, p, length)

  cube_points = get_points_for_cube(random_vect, q, dims)
  plot_3d_graph(cube_points, q)
  plot_2d_graph(cube_points, q)
  # cube_points = get_points_for_cube(random_vect, q ** dims, 1)
  plot_2d_histogram(cube_points)

  aper_len, uni_len = compute_aperiodic_len(u0, m, p)
  print(f"exp_val={get_expected_val_from_seq(random_vect)}, "
        f"disp={get_dispersion_from_seq(random_vect)}, "
        f"xi2={compute_xi2(random_vect, q, dims)}, "
        f"aperiodic_len={aper_len}, "
        f"unique_len={uni_len}", file=sys.stderr)
  plt.show()

  # with PdfPages('plot.pdf') as export_pdf:
  #   # print(result_arr)
  #   plt.scatter(range(1, p), result_arr)
  #   plt.grid(True)
  #   plt.show()

  #   export_pdf.savefig()
  #   plt.close()
