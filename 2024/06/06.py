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
    visited = {}
    sy, sx = 0, 0
    lines = [l.strip() for l in lines]

    for y in range(len(lines)):
        for x in range(len(lines[0])):
            coords[(y,x)] = lines[y][x]
            if lines[y][x] == "^":
                sy, sx = y, x

    gy, gx = sy, sx
    dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    di = 0
    dx, dy = dirs[0]
    visited[(gy,gx)] = 0

    while (gy+dy,gx+dx) in coords:
        if coords[(gy+dy,gx+dx)] == "#":
            di = (di+1)%4
            dx, dy = dirs[di]
        else:
            gx += dx
            gy += dy
            visited[(gy,gx)] = 1

    if not part2:
        return len(visited.keys())

    res = 0
    del visited[(sy,sx)]

    for ky,kx in visited.keys():

        coords[(ky,kx)] = "#"
        gy, gx = sy, sx
        di = 0
        dx, dy = dirs[di]
        seen = {}
        seen[(gy,gx,di)] = 1

        while (gy+dy,gx+dx) in coords:
            if coords[(gy+dy,gx+dx)] == "#":
                di = (di+1)%4
                dx, dy = dirs[di]
            else:
                gx += dx
                gy += dy
            if (gy,gx,di) in seen:
                res += 1
                break
            seen[(gy,gx,di)] = 1

        coords[(ky,kx)] = "."

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
