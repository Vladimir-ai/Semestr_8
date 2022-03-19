#!/usr/bin/env python3

from audioop import byteswap
import binascii
from random import randbytes, random
from PIL import Image
import numpy as np
import matplotlib
matplotlib.use('QtAgg')
import matplotlib.pyplot as plt
import os
import crcmod

def get_bit_from_arr(arr, num):
  return int(bool(arr[num // 8] & (1 << num % 8)))


def get_bit_from_num(num: int, bit_num: int):
  return int(bool(num & (1 << bit_num)))


def compute_crc(array: bytearray):
  crc = crcmod.predefined.mkCrcFun('xmodem')
  return crc(array)


def cell_from_rgb(cell):
  return np.dot([0.2989, 0.587, 0.114], cell)


def array_from_rgb(arr):
  res = np.zeros((arr.shape[0], arr.shape[1], 1), dtype=float)

  for row in range(res.shape[0]):
    for col in range(res.shape[1]):
      res[row, col] = cell_from_rgb(arr[row, col, :])

  return res


def cipher_img(ct: bytearray, path: str, C: int = 2, q: float = 0.01, save_name: str = "Lenna1.png",
                random = False, first_num_len_bytes = 2, crc_len_bytes = 2):
  with Image.open(path) as base_img:
    img_array = np.array(base_img) / 255

    fig = plt.figure(1)
    clean_img_plot = fig.add_subplot(121)
    clean_img_plot.title.set_text("Container without data")
    clean_img_plot.imshow(img_array)

    blue = img_array[:, :, 2].copy()

    # bit count
    n = (img_array.shape[1] - 2 * C) * (img_array.shape[0] - 2 * C)

    if random:
      s = randbytes(n // 8 - first_num_len_bytes - 2)
      print(s)
    else:
      s = ct
      # if len(s) * 8 < n:
      #   s.extend(bytearray(0) * ((n - len(s) * 8) // 8))


    if (len(s) * 8 > n):
      raise ValueError("ct len too big")

    curr_bit = 0
    text_len_bytes = len(s)

    crc_value = compute_crc(text_len_bytes.to_bytes(first_num_len_bytes * 8, 'little') + s)

    for row in range(C + 1, img_array.shape[1] - C, 1):
      for col in range(C + 1, img_array.shape[0] - C, 1):
        last_val = blue[row, col]
        rgb_cell = cell_from_rgb(img_array[row, col, :])

        if curr_bit < first_num_len_bytes * 8:
          new_val = blue[row, col] + (2 * get_bit_from_num(text_len_bytes, curr_bit) - 1) * q * cell_from_rgb(img_array[row, col, :])
          blue[row, col] = blue[row, col] + (2 * get_bit_from_num(text_len_bytes, curr_bit) - 1) * q * cell_from_rgb(img_array[row, col, :])

        elif (offset := (curr_bit - first_num_len_bytes * 8)) < len(s) * 8:
          new_val = blue[row, col] + (2 * get_bit_from_arr(s, offset) - 1) * q * cell_from_rgb(img_array[row, col, :])
          blue[row, col] = blue[row, col] + (2 * get_bit_from_arr(s, offset) - 1) * q * cell_from_rgb(img_array[row, col, :])

        elif (offset := (curr_bit - (first_num_len_bytes - len(s)) * 8)) < crc_len_bytes * 8:
          new_val = blue[row, col] + (2 * get_bit_from_num(crc_value, offset) - 1) * q * cell_from_rgb(img_array[row, col, :])
          blue[row, col] = blue[row, col] + (2 * get_bit_from_num(crc_value, offset) - 1) * q * cell_from_rgb(img_array[row, col, :])

        else:
          blue[row, col] = blue[row, col] + (-1) * q * cell_from_rgb(img_array[row, col, :])

        curr_bit += 1

    img_copy = img_array.copy()
    img_copy[:, :, 2] = blue

    xa = fig.add_subplot(122)
    xa.title.set_text("Container with data")
    xa.imshow(img_copy)

    dispersion = np.max(np.abs(np.subtract(array_from_rgb(img_copy), array_from_rgb(img_array))))
    deviation = np.std(np.subtract(array_from_rgb(img_copy), array_from_rgb(img_array)))

    print(f"dispersion: {dispersion}")
    print(f"std: {deviation}")

    img_copy = (img_copy * 255).astype(np.uint8)
    img = Image.fromarray(img_copy)
    img.save(save_name)



def decipher_img(path: str, C: int = 2, q: float = 2, first_num_len_bytes = 2, crc_len_bytes = 2):
  with Image.open(path) as base_img:
    img_array = np.array(base_img) / 255
    n = (img_array.shape[1] - 2 * C) * (img_array.shape[0] - 2 * C)
    s = bytearray()
    blue = img_array[:, :, 2].copy()

    curr_bit = 0
    text_len = 0
    crc_expected = 0
    for row in range(C + 1, img_array.shape[1] - C, 1):
      for col in range(C + 1, img_array.shape[0] - C, 1):
        vertical = blue[row - C : row + C, col]
        horizontal = blue[row, col - C : col + C]

        curr_blue = blue[row, col]
        b_ = (sum(vertical) + sum(horizontal) - 2 * blue[row, col]) / (4 * C)
        res = int(blue[row, col] > b_)
        if curr_bit < first_num_len_bytes * 8:
          text_len |= res << curr_bit

          if curr_bit + 1 == first_num_len_bytes * 8:
            s.extend(bytearray(text_len))

        elif (offset := (curr_bit - first_num_len_bytes * 8)) < text_len * 8:
          s[offset // 8] |= res << (offset % 8)

        elif (offset := (curr_bit - (first_num_len_bytes - text_len) * 8)) < crc_len_bytes * 8:
          crc_expected |= res << offset

        else:
          break

        curr_bit += 1

    if crc_expected != compute_crc(text_len.to_bytes(first_num_len_bytes * 8, 'little') + s):
      print("ERROR: CRC mismatch")
    return s


def test(text_len_arr: range, img_path: str):
  import shutil
  shutil.rmtree('test_dir')
  os.mkdir('test_dir')
  plot_arr = []

  for text_len in text_len_arr:
    bytes_arr = randbytes(text_len)
    cipher_img(bytes_arr, img_path, save_name="test_dir/"+img_path[:-3]+str(text_len)+".png")
    result_arr = decipher_img("test_dir/"+img_path[:-3]+str(text_len)+".png")
    xor_result = bytes(a ^ b for (a, b) in zip(bytes_arr, result_arr[:text_len]))
    error_ct = sum([bin(x).count("1") for x in xor_result])
    plot_arr.append(error_ct)

  fig = plt.figure(2)
  sub_plt = fig.add_subplot(121)
  sub_plt.set_xlabel('Text length')
  sub_plt.set_ylabel('Error count')
  print(list(text_len_arr))
  print(plot_arr)
  sub_plt.scatter(list(text_len_arr), plot_arr)


if __name__ == "__main__":
  cipher_img("aboba".encode('ascii'), "Lenna.png")
  s = decipher_img("Lenna1.png")
  print(s)
  # test(range(10, 12), "Lenna.png")

  plt.show()


