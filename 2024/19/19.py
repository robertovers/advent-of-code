import sys
import os
import math
import re
from copy import copy, deepcopy
from itertools import permutations, combinations, product, accumulate
from collections import Counter, defaultdict, deque
from functools import lru_cache, reduce
from heapq import heappush, heappop
from suffix_tree import Tree


def solve(lines, part2 = False):

    pat = lines[0].strip().split(", ") 
    des = [l.strip() for l in lines[2:]]

    pats = set()
    for p in pat:
        pats.add(p)

    memo = {}
    def n_ways(d):
        if d in memo:
            return memo[d]
        
        res = int(d in pats)
        for p in pats:
            if d != p and d.startswith(p):
                res += n_ways(d[len(p):])
        memo[d] = res
        return res

    res = 0
    for d in des:
        if part2:
            res += n_ways(d)
        else:
            res += n_ways(d) > 0
    
    return res


if __name__ == "__main__":

    lines = []
    file = "19/input.txt"#sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
