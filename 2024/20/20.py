import sys
import os
import math
import re
from copy import copy, deepcopy
from itertools import permutations, combinations, product, accumulate
from collections import Counter, defaultdict, deque
from functools import lru_cache, reduce
from heapq import heappush, heappop
from suffix_tree import Tree


def solve(lines, part2 = False):

    G = [l.strip() for l in lines]

    for y, row in enumerate(G):
        for x, r in enumerate(row):
            if G[y][x] == "E":
                ey, ex = y, x
            elif G[y][x] == "S":
                sy, sx = y, x
    
    dirs = [(1,0), (0,1), (-1,0), (0,-1)]

    from_start = search(G, sx, sy)
    from_end = search(G, ex, ey)

    Q = [(0,0,sx,sy,(-1,-1),(-1,-1))] # cost, length, x, y, start of cheat, end of cheat
    fin = {}
    chx = []
    cmax = 20 if part2 else 2
    k = 100

    while Q:
        (uc,ch,ux,uy,sc,ec) = heappop(Q)
        if (ux,uy,sc,ec) in fin:
            continue
        elif ec != (-1,-1):
            # finished cheat, add cost of path to end
            ucx = uc + from_end[ec]
            chx.append((sc,ec,ch,ucx))
            fin[(ux,uy,sc,ec)] = ucx
            continue
        elif uc + (abs(ex-ux) + abs(ey-uy)) > from_start[(ex,ey)] - k:
            # can't possibly save 100 picoseconds
            continue

        #print(uc,ch,ux,uy,sc,ec)
        fin[(ux,uy,sc,ec)] = uc

        for d, (dx,dy) in enumerate(dirs):
            dx, dy = dirs[d]
            xn, yn = ux+dx, uy+dy
            if yn in range(len(G)) and xn in range(len(G[0])):
                # start cheat
                if ch == 0:
                    heappush(Q, (uc+1,1,xn,yn,(ux,uy),ec))
                # currently cheating, continue cheat
                if ch in range (1,cmax-1) and ec == (-1,-1):
                    heappush(Q, (uc+1,ch+1,xn,yn,sc,ec))
                # currently cheating, end cheat
                if ch in range (1,cmax) and ec == (-1,-1) and G[yn][xn] != "#":
                    heappush(Q, (uc+1,ch+1,xn,yn,sc,(xn,yn)))
                # not cheating, move forward
                if (ch == 0 or ec != (-1,-1)) and G[yn][xn] != "#":
                    heappush(Q, (uc+1,ch,xn,yn,sc,ec))

    ucs = []
    for (sc,ec,ch,uc) in chx:
        if from_start[(ex,ey)] - uc >= k:
            ucs.append(from_start[(ex,ey)]-uc)
    return len(ucs)


def search(G, sx,sy):

    dirs = [(1,0), (0,1), (-1,0), (0,-1)]
    Q = [(0,sx,sy)]
    fin = {}

    while Q:
        (uc,ux,uy) = heappop(Q)
        if (ux,uy) in fin:
            continue

        fin[(ux,uy)] = uc

        for d, (dx,dy) in enumerate(dirs):
            dx, dy = dirs[d]
            xn, yn = ux+dx, uy+dy
            if yn in range(len(G)) and xn in range(len(G[0])):
                if G[yn][xn] != "#":
                    heappush(Q, (uc+1,xn,yn))
 
    return fin


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
