def prefix_border_array(sting: str):
    bp = []
    bp.append(0)
    for i in range(1, len(sting)):
        right = bp[i - 1]
        while right and sting[i] != sting[right]:
            right = bp[right - 1]
        if sting[i] == sting[right]:
            bp.append(right + 1)
        else:
            bp.append(0)
    return bp

def prefix_border_arrayM(s, bp):
    bpm = [ 0 for i in bp]
    bpm[0] = 0
    n = len(s)
    bpm[n-1] = bp[n-1]
    for i in range(1, n - 1):
        if (bp[i] and (s[bp[i]] == s[i+1])):
            bpm[i] = bpm[bp[i] - 1]
        else:
            bpm[i] = bp[i]
    return bpm

def bp_to_bpm(bp, n):
    bpm = [0 for i in range(n)]
    bpm[n - 1] = bp[n - 1]
    for i in range(1, n - 1):
        if bp[i] and bp[i] + 1 == bp[i + 1]:
            bpm[i] = bpm[bp[i] - 1]
        else:
            bpm[i] = bp[i]
    return bpm
