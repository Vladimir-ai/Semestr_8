#!/usr/bin/env python3

from numpy import block
from sympy import totient

n = 889577666850907
arr = [403013074606912, 545180648978557, 219641194372024, 501606729868202, 878976557455422]
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

d = pow(e, -1, phi(n))

print(f"d: {d}")

result = str()
for num in arr:
  power = str(pow(num, d, n))
  result += power

print(str().join([chr(int(result[idx: idx + 2], base=10)) for idx in range(0, len(result) - 1, 2)]))


