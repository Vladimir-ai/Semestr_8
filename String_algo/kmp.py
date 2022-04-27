import lab3
import border

def kmp(p, t):
    bpm = lab2.prefix_border_array(p)
    m = len(p)
    n = len(t)
    res = []
    bpm = lab3.bp_to_bpm(bpm, m)
    k = 0
    for i in range(n):
        while k > 0 and p[k] != t[i]:
            k = bpm[k - 1]
        if p[k] == t[i]:
            k += 1
        if k == m:
            res.append(i - k + 1)
    return res