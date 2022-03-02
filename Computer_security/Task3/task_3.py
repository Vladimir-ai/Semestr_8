#!/usr/bin/env python3


def get_next(u: int, m: int, p: int):
  u_next = u * m % p
  r_next = u_next / p
  return u_next, r_next


def generate_int(u0: int, m: int, p: int, n: int):
  res = [0] * n
  u = u0
  
  for idx in range(1, n):
    u, _ = get_next(u, m, p)
    res[idx] = u
    
  return res


def generate_float(u0: int, m: int, p: int, n: int):
  res = [0.0] * n
  u = u0
  
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
  

if __name__ == '__main__':
  print(get_expected_val(7, 5, 10007, 100000))
    