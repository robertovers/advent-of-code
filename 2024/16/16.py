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

    # takes a while to run :/

    G = [l.strip() for l in lines]
    dirs = [(1,0), (0,1), (-1,0), (0,-1)]

    for y, row in enumerate(G):
        for x, r in enumerate(row):
            if G[y][x] == "E":
                ey, ex = y, x
            elif G[y][x] == "S":
                sy, sx = y, x

    Q = [(0,sx,sy,0,[(sx,sy)])]
    fin = {}
    eps = []

    while Q:
        (uc,ux,uy,ud,up) = heappop(Q)
        if (ux,uy,ud) in fin and fin[(ux,uy,ud)] < uc:
            continue
        elif (ux,uy) == (ex,ey):
            eps.append((up,uc))
            fin[(ux,uy,ud)] = uc
            continue

        fin[(ux,uy,ud)] = uc

        dx, dy = dirs[ud]
        xn, yn = ux+dx, uy+dy

        if (xn in range(0, len(G[0])) and yn in range(0, len(G))
            and G[yn][xn] != "#"):
            heappush(Q, (uc+1,xn,yn,ud,up+[(xn,yn)]))

        for d, (dx,dy) in enumerate(dirs):
            if d != ud and (d+2)%4 != (ud+2)%4:
                heappush(Q, (uc+1000,ux,uy,d,up))

    cs = []
    for d in range(4):
        if (ex,ey,d) in fin:
            cs.append(fin[(ex,ey,d)])

    if part2:
        mps = []
        for p, c in eps:
            if c == min(cs):
                mps.append(p)
        return len(set([x for xs in mps for x in xs]))

    return min(cs)


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
