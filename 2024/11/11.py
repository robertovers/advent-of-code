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

    l = lines[0].strip()

    @lru_cache(maxsize=None)
    def transform(x):
        if x == 0:
            return [1]
        elif len(str(x)) % 2 == 0:
            return [
                int(str(x)[:len(str(x))//2]), 
                int(str(x)[len(str(x))//2:])]
        else:
            return [x*2024]

    xss = [int(x) for x in l.split(" ")]
    k = 75
    counts = {}

    for x in xss:
        counts[x] = 1

    for _ in range(k):
        ncounts = {}
        for kx in counts.keys():
            nx = transform(kx)
            for jx in nx:
                if jx in ncounts:
                    ncounts[jx] += counts[kx]
                else:
                    ncounts[jx] = counts[kx]
        counts = ncounts

    return sum([v for v in counts.values()])


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    #print(solve(lines, True))
