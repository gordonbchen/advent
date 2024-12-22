import sys
from functools import cache

num_keypad = [
    list("789"),
    list("456"),
    list("123"),
    [None, "0", "A"],
]

dir_keypad = [
    [None, "^", "A"],
    list("<v>")
]

def get_coords(keypad):
    coords = {}
    for i, row in enumerate(keypad):
        for j, c in enumerate(row):
            if c == None:
                continue
            coords[c] = (i, j)
    return coords

@cache
def get_sub_path(start, end, nums: bool = True):
    dy = start[0] - end[0]
    dx = end[1] - start[1]

    ups = abs(dy) * (dy > 0) * ["^"]
    downs = abs(dy) * (dy < 0) * ["v"]
    rights = abs(dx) * (dx > 0) * [">"]
    lefts = abs(dx) * (dx < 0) * ["<"]

    inv_coords = num_inv_coords if nums else dir_inv_coords

    if (max(start[0], end[0]), min(start[1], end[1])) not in inv_coords:
        path = ups + lefts + rights + downs + ["A"]
    elif (min(start[0], end[0]), min(start[1], end[1])) not in inv_coords:
        path = downs + lefts + rights + ups + ["A"]
    else:
        path = lefts + downs + ups + rights + ["A"]
    return path

@cache
def get_ndirs(c1, c2, n):
    sub_path = get_sub_path(c1, c2, n==N_ROBOTS)

    if n == 0:
        return len(sub_path)
    
    total_ndirs = 0
    for i in range(len(sub_path)):
        start = dir_coords["A"] if i == 0 else dir_coords[sub_path[i - 1]]
        end = dir_coords[sub_path[i]]
        total_ndirs += get_ndirs(start, end, n - 1)

    return total_ndirs
    

if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        data = f.read()
    codes = data.split()

    num_coords = get_coords(num_keypad)
    num_inv_coords = {v: k for k, v in num_coords.items()}

    dir_coords = get_coords(dir_keypad)
    dir_inv_coords = {v: k for k, v in dir_coords.items()}

    N_ROBOTS = 25

    complexity = 0
    for code in codes:
        print(f"\n{code}")

        n_dirs = 0
        for i in range(len(code)):
            start = num_coords["A"] if i == 0 else num_coords[code[i - 1]]
            end = num_coords[code[i]]
            n_dirs += get_ndirs(start, end, N_ROBOTS)

        code_num = int(code[:-1])
        print(n_dirs, code_num)
        complexity += n_dirs * code_num

    print(f"\nComplexity: {complexity}")
