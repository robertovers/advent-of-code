import sys

def solve(lines, part2 = False):

    ax = []
    bx = []

    for line in lines:
        a, b = line.strip().split("   ")
        ax.append(int(a))
        bx.append(int(b))

    if part2:

        res = 0
        bd = {}

        for b in bx:
            if b not in bd:
                bd[b] = 1
            else:
                bd[b] += 1
        
        for a in ax:
            if a in bd:
                res += a * bd[a]

        return res

    ax.sort()
    bx.sort()

    yx = []

    for i in range(len(ax)):
        yx.append(abs(ax[i] - bx[i]))

    return sum(yx)


if __name__ == "__main__":

    lines = []
    file = sys.argv[1]
    with open(file, "r") as f:
        lines = f.readlines()

    print(solve(lines))
    print(solve(lines, True))
