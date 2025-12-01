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

    inp = lines[0].strip()
    s = []
    x = 0
    for i, d in enumerate(inp):
        if i%2 == 1:
            s += int(d) * ["."]
        else:
            s += int(d) * [str(x)]
            x += 1
    
    if not part2:

        a, b = 0, len(s)-1
        while s[b] == ".":
            b -= 1
        while b > a:
            if s[a] == ".":
                s[a], s[b] = s[b], s[a]
                a += 1
                while s[b] == ".":
                    b -= 1
            else:
                while s[a] != ".":
                    a += 1

    else:

        mv = set()
        rb = len(s)-1

        while rb > 0:

            # go to next unmoved block
            while rb > 0 and (s[rb] == "." or s[rb] in mv):
                rb -= 1
            lb = rb
            while s[lb] == s[lb-1]:
                lb -= 1
            mv.add(s[lb])

            la = 0
            # try fit block in each space
            while lb > la:

                # go to next space
                while s[la] != ".":
                    la += 1
                ra = la
                while ra < len(s) and s[ra] == ".":
                    ra += 1
                
                if ra > lb:
                    continue

                if ra - la >= rb - lb + 1:
                    # move block to space
                    for _ in range(rb - lb + 1):
                        s[la], s[lb] = s[lb], s[la]
                        la += 1
                        lb += 1
                    break

                # skip remainder of current space
                while la < len(s) and s[la] == ".":
                    la += 1

    res = 0
    for i, c in enumerate(s):
        if c != ".":
            res += i * int(c)

    return res


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
