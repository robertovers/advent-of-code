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

    rs = []
    for l in lines:
        xs = [int(x)
              for s in l.strip().split(" ")
              for x in s[2:].split(",")]
        rs += [xs]

    Gx,Gy=101,103

    #for _ in range(100):
    s = 0
    
    while True:
        G = [["." for _ in range(Gx)] for _ in range(Gy)]
        for x,y,_,_ in rs:
            G[y][x] = "0"

        # used trial and error for p2 lol
        ct = 0
        for y in range(Gy):
            for x in range(Gx//2,Gx//2+Gx//4):
                if G[y][x] == "0":
                    ct += 1
        if ct > 280:
            print(s)
            for row in G:
                print("".join(c for c in row))

        for i, (x,y,dx,dy) in enumerate(rs):
            rs[i] = ((x+dx)%Gx, (y+dy)%Gy, dx, dy)
        s += 1

    xmid = Gx//2
    ymid = Gy//2
    cts = [0,0,0,0]
    for x,y,_,_ in rs:
        if x < xmid and y < ymid:
            G[y][x] = "0"
            cts[0] += 1
        elif x < xmid and y > ymid:
            G[y][x] = "0"
            cts[1] += 1
        elif x > xmid and y < ymid:
            G[y][x] = "0"
            cts[2] += 1
        elif x > xmid and y > ymid:
            G[y][x] = "0"
            cts[3] += 1

    res = 1
    for c in cts:
        res *= c
    
    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    #print(solve(lines, True))
