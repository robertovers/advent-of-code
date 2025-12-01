import sys
import os
import math
import re
from copy import copy, deepcopy
from itertools import permutations, combinations, product, accumulate
from collections import Counter, defaultdict, deque
from functools import lru_cache, reduce
from heapq import heappush, heappop


def safe(xs):
    safe = True
    for i in range(1, len(xs)):
        d = abs(xs[i] - xs[i-1])
        if d == 0 or d > 3:
            safe = False
    if safe and (xs == sorted(xs) or xs == sorted(xs, reverse=True)):
        return True
    return False


def solve(lines, part2 = False):

    res = 0
    for l in lines:
        xs = [int(x) for x in l.strip().split()]
        if safe(xs):
            res += 1
        elif part2:
            for i in range(len(xs)):
                xss = copy(xs)
                xss.pop(i)
                if safe(xss):
                    res += 1
                    break

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
