import sys
import os
import math
import re
from copy import copy, deepcopy
from itertools import permutations, combinations, product, accumulate
from collections import Counter, defaultdict, deque
from functools import lru_cache, reduce
from heapq import heappush, heappop


def is_valid(before, xs):
    for i, x in enumerate(xs):
        if x in before:
            for j in range(i+1, len(xs)):
                if xs[j] in before[x]:
                    return False
    return True


def solve(lines, part2 = False):
    res = 0
    before = {}
    i = False

    for l in lines:
        if l == "\n":
            i = True
            continue

        if i:
            xs = l.strip().split(",")
            for x in xs:
                if x not in before:
                    before[x] = []
            if part2:
                valid = True
                for i in range(len(xs)):
                    if xs[i] in before:
                        for j in range(i+1, len(xs)):
                            if xs[j] in before[xs[i]]:
                                valid = False
                                for k in range(j, i, -1):
                                    xs[k-1], xs[k] = xs[k], xs[k-1]
                if not valid:
                    res += int(xs[len(xs) // 2])
            else:
                if is_valid(before, xs):
                    res += int(xs[len(xs) // 2])

        if not i:
            a, b = l.strip().split("|")
            if b in before:
                before[b] += [a]
            else:
                before[b] = [a]

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
