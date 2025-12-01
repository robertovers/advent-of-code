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

    coords = {}
    for y, l in enumerate(lines):
        l = l.strip()
        for x in range(len(l)):
            coords[(x,y)] = l[x]

    if part2:
        dirs = [((1,1),(-1,-1)),
                ((-1,-1),(1,1)),
                ((-1,1),(1,-1)),
                ((1,-1),(-1,1))]
        for y, l in enumerate(lines):
            l = l.strip()
            for x in range(len(l)):
                if coords[(x,y)] == "A":
                    ct = 0
                    for d in dirs:
                        c1 = (x+d[0][0],y+d[0][1])
                        c2 = (x+d[1][0],y+d[1][1])
                        if c1 in coords and c2 in coords:
                            if (coords[c1] == "M" and coords[c2] == "S"):
                                ct += 1
                    if ct > 1:
                        res += 1

        return res

    dirs = [(0,1),(1,0),(-1,0),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
    for y, l in enumerate(lines):
        l = l.strip()
        for x in range(len(l)):
            for d in dirs:
                s = ""
                for i in range(0, 4):
                    if (x+d[0]*i,y+d[1]*i) in coords:
                        s += coords[(x+d[0]*i,y+d[1]*i)]
                if s == "XMAS":
                    res += 1

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
