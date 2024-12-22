import sys

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

def dist(c1, c2):
    return abs(c1[0] - c2[0]) + abs(c1[1] - c2[1])

def get_path(code, coords, inv_coords):
    path = []
    for i in range(len(code)):
        start = coords["A"] if i == 0 else coords[code[i - 1]]
        end = coords[code[i]]

        dy = start[0] - end[0]
        dx = end[1] - start[1]

        ups = abs(dy) * (dy > 0) * ["^"]
        downs = abs(dy) * (dy < 0) * ["v"]
        rights = abs(dx) * (dx > 0) * [">"]
        lefts = abs(dx) * (dx < 0) * ["<"]
        
        if (max(start[0], end[0]), min(start[1], end[1])) not in inv_coords:
            path += ups + lefts + rights + downs + ["A"]
        elif (min(start[0], end[0]), min(start[1], end[1])) not in inv_coords:
            path += downs + lefts + rights + ups + ["A"]
        else:
            path += lefts + downs + ups + rights + ["A"]
            
    return path

if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        data = f.read()
    codes = data.split()

    num_coords = get_coords(num_keypad)
    num_inv_coords = {v: k for k, v in num_coords.items()}

    dir_coords = get_coords(dir_keypad)
    dir_inv_coords = {v: k for k, v in dir_coords.items()}

    complexity = 0
    for code in codes:
        print(f"\n{code}")
        path = get_path(code, num_coords, num_inv_coords)
        print(f"num_path: {''.join(path)}")

        for i in range(2):
            path = get_path(path, dir_coords, dir_inv_coords)
            print(f"path: {''.join(path)}")

        complexity += len(path) * int(code[:-1])
        print(len(path), int(code[:-1]))

    print(f"\nComplexity: {complexity}")
