import sys
import os
import math
import re
from copy import copy, deepcopy
from itertools import permutations, combinations, product, accumulate
from collections import Counter, defaultdict, deque
from functools import lru_cache, reduce
from heapq import heappush, heappop


def solve2(lines):
    G = []
    Gy = 0
    Gx = len(lines[0].strip())

    while lines[Gy] != "\n":
        G.append(list(lines[Gy].strip()))
        Gy += 1

    G2 = [["." for _ in range(Gx*2)] for _ in range(Gy)]

    coords = set()

    sx,sy = None, None
    for y, row in enumerate(G):
        for x, c in enumerate(row):
            coords.add((x*2,y))
            coords.add((x*2+1,y))
            if c == "@":
                sx, sy = x*2, y
                G2[sy][sx] = "@"
            elif c == "O":
                G2[y][x*2] = "[" 
                G2[y][x*2+1] = "]" 
            elif c == "#":
                G2[y][x*2] = "#"
                G2[y][x*2+1] = "#"

    G = G2
    
    mv = [c for l in lines[Gy+1:] for c in l.strip()]

    dirs = {"^":(0,-1), ">":(1,0), "v":(0,1), "<":(-1,0)}

    x, y = sx, sy
    for m in mv:
        print(m)
        dx, dy = dirs[m]
        nx, ny = x+dx, y+dy
        if G[ny][nx] in "[]":
            i = 0
            if dy == 0:
                while (nx+dx*i, ny+dy*i) in coords and G[ny+dy*i][nx+dx*i] in "[]":
                    i += 1
            else:
                boxes = []
                bx, by = nx, ny
                Q = [(bx,by)]
                while Q:
                    bx,by = Q.pop()
                    if G[by][bx] == "[":
                        d = 1
                        boxes.append(((bx,by,bx+1,by)))
                    elif G[by][bx] == "]":
                        d = -1
                        boxes.append(((bx-1,by,bx,by)))
                    if (bx,by+dy) in coords and G[by+dy][bx] in "[]":
                        Q.append((bx, by+dy))
                    if (bx+d,by+dy) in coords and G[by+dy][bx+d] in "[]":
                        Q.append((bx+d, by+dy))
                for (bx1,by1,bx2,by2) in boxes:
                    if G[by1+dy][bx1] == "#" or G[by2+dy][bx2] == "#":
                        boxes = []
            if dy == 0:
                if G[ny+dy*i][nx+dx*i] != "#":
                    ux, uy = nx+dx*i, ny+dy*i
                    for j in range(i):
                        tmp = G[uy-dy*j][ux-dx*j]
                        G[uy-dy*j][ux-dx*j] = G[uy-dy*(j+1)][ux-dx*(j+1)]
                        G[uy-dy*(j+1)][ux-dx*(j+1)] = tmp

                    G[ny][nx] = "@"
                    G[y][x] = "."
                    x, y = nx, ny
            else:
                if boxes:
                    for (bx1,by1,bx2,by2) in reversed(boxes):
                        G[by1][bx1] = "."
                        G[by2][bx2] = "."
                        G[by1+dy][bx1] = "["
                        G[by2+dy][bx2] = "]"
                    for (bx1,by1,bx2,by2) in boxes:
                        if G[by1+dy][bx1] == ".":
                            G[by1+dy][bx1] = "["
                        if G[by2+dy][bx2] == ".":
                            G[by2+dy][bx2] = "]"
                    G[ny][nx] = "@"
                    G[y][x] = "."
                    x, y = nx, ny

        elif G[ny][nx] == ".":
            G[ny][nx] = "@"
            G[y][x] = "."
            x, y = nx, ny
        for row in G:
            print("".join(c for c in row))

    res = 0
    for y, row in enumerate(G):
        for x, c in enumerate(row):
            if G[y][x] == "[":
                res += 100 * y + x

    return res


def solve1(lines):
    G = []
    Gy = 0
    Gx = len(lines[0].strip())

    while lines[Gy] != "\n":
        G.append(list(lines[Gy].strip()))
        Gy += 1

    coords = set()

    sx,sy = None, None
    for y, row in enumerate(G):
        for x, c in enumerate(row):
            coords.add((x,y))
            if G[y][x] == "@":
                sx, sy = x, y

    mv = [c for l in lines[Gy+1:] for c in l.strip()]

    dirs = {"^":(0,-1), ">":(1,0), "v":(0,1), "<":(-1,0)}

    x, y = sx, sy
    for m in mv:
        #for row in G:
        #    print("".join(c for c in row))
        #print(m)
        dx, dy = dirs[m]
        nx, ny = x+dx, y+dy
        if G[ny][nx] == "O":
            i = 0
            while (nx+dx*i, ny+dy*i) in coords and G[ny+dy*i][nx+dx*i] == "O":
                i += 1
            if G[ny+dy*i][nx+dx*i] != "#":
                ux, uy = nx+dx*i, ny+dy*i
                for j in range(i):
                    tmp = G[uy-dy*j][ux-dx*j]
                    G[uy-dy*j][ux-dx*j] = G[uy-dy*(j+1)][ux-dx*(j+1)]
                    G[uy-dy*(j+1)][ux-dx*(j+1)] = tmp
                G[ny][nx] = "@"
                G[y][x] = "."
                x, y = nx, ny
        elif G[ny][nx] == ".":
            G[ny][nx] = "@"
            G[y][x] = "."
            x, y = nx, ny

    res = 0
    for y, row in enumerate(G):
        for x, c in enumerate(row):
            if G[y][x] == "O":
                res += 100 * y + x

    return res



if __name__ == "__main__":

    lines = []
    file = "input.txt"#sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    #print(solve1(lines))
    print(solve2(lines))
