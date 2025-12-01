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
    G = [[int(c) for c in l.strip("\n")] for l in lines]
    dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    for sy, l in enumerate(G):
        for sx, v in enumerate(l):

            if G[sy][sx] != 0:
                continue

            Q = [(sx,sy,"")]
            fin = set()

            while Q:
                (ux,uy,id) = heappop(Q)

                if G[uy][ux] == 9:
                    fin.add((ux,uy,id))
                    continue

                for d, (dx,dy) in enumerate(dirs):
                    xn, yn = ux+dx, uy+dy
                    if xn in range(0, len(l)) and yn in range(0, len(G)):
                        el = G[yn][xn] - G[uy][ux]
                        if el == 1:
                            if part2:
                                heappush(Q, (xn,yn,id+str(d)))
                            else:
                                heappush(Q, (xn,yn,""))

            res += len(fin)

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
