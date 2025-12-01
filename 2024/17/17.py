import sys
import os
import math
import re
from copy import copy, deepcopy
from itertools import permutations, combinations, product, accumulate
from collections import Counter, defaultdict, deque
from functools import lru_cache, reduce
from heapq import heappush, heappop


def solve1(lines):

    A = int(lines[0][12:])
    B, C = 0, 0

    ip = 0
    prog = [int(i) for i in lines[4][8:].strip().split(",")]

    def combo(x, regs):
        if x < 4:
            return x
        return regs[x-4]

    out = []
    while ip < len(prog)-1:

        i = prog[ip]
        o = prog[ip+1]

        jumped = False
        regs = [A, B, C]
        match i:
            case 0:
                A = (A / 2 ** combo(o, regs)) // 1
            case 1:
                B = int(B) ^ int(o)
            case 2:
                B = combo(o, regs) % 8
            case 3:
                if A != 0:
                    jumped = True
                    ip = o
            case 4:
                B = int(B) ^ int(C)
            case 5:
                out.append(combo(o, regs) % 8)
            case 6:
                B = (A / 2 ** combo(o, regs)) // 1
            case 7:
                C = (A / 2 ** combo(o, regs)) // 1
            case _:
                break

        if not jumped:
            ip += 2

    res = "".join(str(int(c))+"," for c in out)[:-1]

    return res


def solve2(lines):

    # required some reddit assistance for this one :/

    prog = [int(i) for i in lines[4][8:].strip().split(",")]

    def iter(A):
        B = A % 8
        B = int(B) ^ int(2)
        C = A / (2 ** B)
        B = int(B) ^ int(3)
        B = int(B) ^ int(C)
        out = B % 8
        return A, out

    def recur(A, i):
        A, out = iter(A)
        if out != prog[i]:
            return
        if i == 0:
            Ax.append(A)
            return
        
        for k in range(8):
            recur(A*8+k, i-1)

    Ax = []
    for A in range(8):
        recur(A, len(prog) - 1)

    return min(Ax)


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve1(lines))
    print(solve2(lines))
