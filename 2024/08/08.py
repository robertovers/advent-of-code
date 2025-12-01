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
    coords = set()
    at = {}
    
    for y, l in enumerate(lines):
        for x, c in enumerate(l.strip()):
            coords.add((y,x))
            if c != ".":
                at[(y,x)] = c

    ans = set()
    ats = list(at.items())
    for i, ((y1,x1),a1) in enumerate(ats):
        for j in range(i+1, len(ats)):
            ((y2,x2)),a2 = ats[j]
            if a1 == a2:
                if part2:
                    for i in range(max(len(lines[0]), len(lines))):
                        an1 = (y1+i*(y1-y2), x1+i*(x1-x2)) 
                        an2 = (y1-i*(y1-y2), x1-i*(x1-x2)) 
                        ans.add(an1)
                        ans.add(an2)
                else:
                    an1 = (y1+(y1-y2), x1+(x1-x2)) 
                    an2 = (y1-2*(y1-y2), x1-2*(x1-x2)) 
                    ans.add(an1)
                    ans.add(an2)

    for ax in ans:
        res += ax in coords

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
