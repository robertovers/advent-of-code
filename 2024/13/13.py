import sys
import os
import math
import re
from copy import copy, deepcopy
from itertools import permutations, combinations, product, accumulate
from collections import Counter, defaultdict, deque
from functools import lru_cache, reduce
from heapq import heappush, heappop


def solve(lines, part2 = False):

    res = 0

    mach = []
    a,b,p = None,None,None
    for l in lines:
        if "A" in l:
            a = [int(s[2:]) for s in l[10:].strip().split(", ")]
        elif "B" in l:
            b = [int(s[2:]) for s in l[10:].strip().split(", ")]
        elif "Prize" in l:
            p = [int(s[2:]) for s in l[6:].strip().split(", ")]
        else:
            mach.append({"a":a,"b":b,"p":p})
    mach.append({"a":a,"b":b,"p":p})
    
    for i,m in enumerate(mach):

        px, py = m["p"]
        ls = [(*m["a"], 3), (*m["b"], 1)]

        if part2:
            px += 10000000000000
            py += 10000000000000

        (cx1, cy1, c1), (cx2, cy2, c2) = ls
        b = py - (cy2/cx2) * px
        ix = b / ((cy1/cx1) - (cy2/cx2))
        iy = cy1 / cx1 * ix
        cc1, cc2 = iy / cy1, (py - iy) / cy2

        if round(cc1, 3).is_integer() and round(cc2, 3).is_integer():
            res += cc1 * c1 + cc2 * c2
        
    return res


if __name__ == "__main__":

    lines = []
    file = "input.txt"#sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
