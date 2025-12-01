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

    def search(i):
        G = [["." for _ in range(71)] for _ in range(71)]

        for l in lines[:i]:
            x,y = l.strip().split(",")
            x,y = int(x),int(y)
            G[y][x] = "#"

        dirs = [(1,0), (0,1), (-1,0), (0,-1)]

        Q = [(0,0,0)]
        fin = {}
        eps = []

        while Q:
            (uc,ux,uy) = heappop(Q)
            if (ux,uy) in fin:
                continue

            fin[(ux,uy)] = uc

            for d, (dx,dy) in enumerate(dirs):
                dx, dy = dirs[d]
                xn, yn = ux+dx, uy+dy
                if yn in range(0, 71) and xn in range(0, 71):
                    if G[yn][xn] != "#":
                        heappush(Q, (uc+1,xn,yn))

        return fin
    
    if part2:

        for i in range(1024, len(lines)):
            fin = search(i)
            if (70,70) not in fin:
                print(lines[i-1])
                return 0

    fin = search(1024)
    res = fin[(70,70)]
    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
