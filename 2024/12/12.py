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

    G = [l.strip() for l in lines]
    dirs = [(1,0), (0,1), (-1,0), (0,-1)]
    regions = {}
    id = 0

    for sy, row in enumerate(G):
        for sx, r in enumerate(row):

            seen = False
            for _, plots in regions.items():
                if (sx,sy) in plots:
                    seen = True

            if seen:
                continue

            Q = [(sx,sy)]
            visited = set()

            while Q:
                (ux,uy) = Q.pop(0)
                visited.add((ux,uy))
                for d, (dx,dy) in enumerate(dirs):
                    xn, yn = ux+dx, uy+dy
                    if xn in range(0, len(G[0])) and yn in range(0, len(G)):
                        if G[yn][xn] == r and (xn,yn) not in visited and ((xn,yn)) not in Q:
                            Q.append((xn,yn))
            
            regions[id] = set(visited)
            id += 1

    res = 0

    if not part2:

        for id, plots in regions.items():
            perim = 0
            for (px,py) in plots:
                for d, (dx,dy) in enumerate(dirs):
                    xn, yn = px+dx, py+dy
                    if (xn,yn) not in plots:
                        perim += 1
            res += len(plots) * perim

    else:

        cdirs = [(1,1), (1,-1), (-1,1), (-1,-1)]

        # increase res to avoid 1 or 2-narrow paths
        regions_s = {}
        for id, plots in regions.items():
            for (x,y) in plots:
                if id not in regions_s:
                    regions_s[id] = []
                regions_s[id] += [
                    (3*x+1,3*y+1),(3*x+1,3*y+2),(3*x+1,3*y+3),
                    (3*x+2,3*y+1),(3*x+2,3*y+2),(3*x+2,3*y+3),
                    (3*x+3,3*y+1),(3*x+3,3*y+2),(3*x+3,3*y+3),
                ]

        for id, plots in regions_s.items():

            # get points around perimeter incl. corners
            out = set()
            for (px,py) in plots:
                for d, (dx,dy) in enumerate(dirs + cdirs):
                    xn, yn = px+dx, py+dy
                    if (xn,yn) not in plots:
                        out.add((xn,yn))

            sides = 0

            while out:

                visited = set()

                # set starting direction
                di = None
                ax, ay = list(out)[0]
                for d,(dx,dy) in enumerate(dirs):
                    xn, yn = ax+dx, ay+dy
                    if (xn,yn) in out and (xn,yn) not in visited:
                        di = d
                        break
                
                ax,ay = xn,yn
                aax, aay = ax, ay

                # walk around and count direction changes
                while (ax, ay, di) not in visited:
                    visited.add((ax,ay,di))
                    if (ax,ay) in out and (ax,ay)!=(aax,aay):
                        out.remove((ax,ay))
                    xn, yn = ax+dx, ay+dy
                    if (xn,yn) in out:
                        ax,ay = xn,yn
                    else:
                        for d,(dx,dy) in enumerate(dirs):
                            dx, dy = dirs[d]
                            xn, yn = ax+dx, ay+dy
                            if (xn,yn) in out and (xn,yn,(d+2)%4) not in visited:
                                di = d
                                sides += 1
                                break
                
                out.remove((aax, aay))

            res += len(plots) // 9 * sides
    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
