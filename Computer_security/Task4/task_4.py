#!/usr/bin/env python3

from sympy import totient


def gen_arr(ct: int, n: int):
  arr = []

  while ct > 0:
    temp_num = 0
    power = 1

    if ct > n:
      while ct % power < n:
        temp_num = ct % power
        power *= 10
    else:
      temp_num = ct
      power = 10**20

    arr.append(temp_num)
    ct //= power // 10

  arr.reverse()
  return arr


n = 889577666850907
e = 13971

ct = 403013074606912545180648978557219641194372024501606729868202878976557455422
# http://www.factordb.com/index.php?query=889577666850907+
p = 2432279
q = 365738333

# https://codeiiest-dev.github.io/Algorithms/Algebra/EulerTotient/EulerTotient.html
def phi(n):
    result = n
    i = 2
    while i*i <= n:
        if n % i == 0:
            result -= result//i
            while n % i == 0:
                n //= i
        i += 1
    if n != 1:
        result -= result//n
    return result

arr = gen_arr(ct, n)

print(arr)

d = pow(e, -1, phi(n))

print(f"d: {d}")

result = str()
for num in arr:
  power = str(pow(num, d, n))
  result += power

print(str().join([chr(int(result[idx: idx + 2], base=10)) for idx in range(0, len(result) - 1, 2)]))


