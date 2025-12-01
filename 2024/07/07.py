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
    for l in lines:
        xs = l.strip().split(" ")
        k = int(xs[0][:-1])
        cs = [int(c) for c in xs[1:]]

        def recurse(k, eq, rest):
            if len(rest) == 0:
                if eq == k:
                    return [k]
                return [0]

            if part2:
                cc = int(str(eq) + str(rest[0]))
                return (
                    recurse(k, cc, rest[1:])
                    + recurse(k, eq+rest[0], rest[1:])
                    + recurse(k, eq*rest[0], rest[1:])
                )
            return (
                recurse(k, eq+rest[0], rest[1:])
                + recurse(k, eq*rest[0], rest[1:])
            )

        res += sum(list(set(recurse(k, 0, cs))))

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    #print(solve(lines))
    print(solve(lines, True))
