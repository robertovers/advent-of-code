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

    if part2:

        l = "".join(ln for ln in lines)
        on = True
        xs = re.findall(r"do\(\)|don't\(\)|mul\([0-9]+,[0-9]+\)", l)

        for x in xs:
            if x == "do()":
                on = True
            elif x == "don't()":
                on = False
            else:
                if on:
                    a = x[4:-1]
                    b = a.split(",")
                    res += int(b[0]) * int(b[1])

    for l in lines:
        xs = re.findall(r"mul\([0-9]+,[0-9]+\)", l)
        for x in xs:
            a = x[4:-1]
            b = a.split(",")
            res += int(b[0]) * int(b[1])

    return res

if __name__ == "__main__":
    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()
    print(solve(lines))
    print(solve(lines, True))
