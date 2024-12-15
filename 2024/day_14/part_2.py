import sys
import re

filename = sys.argv[1]
with open(filename, "r") as f:
    text = f.read()

pattern = re.compile(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")

if filename == "test.txt":
    max_x, max_y = 11, 7
else:
    max_x, max_y = 101, 103
mid_x, mid_y = max_x // 2, max_y // 2

coords = []
speeds = []
for nums in pattern.findall(text):
    x, y, vx, vy = (int(i) for i in nums)
    coords.append((x, y)) 
    speeds.append((vx, vy))

def is_interesting(coords):
    coords = set(coords)
    for coord in coords:
        for j in range(1, 10):
            if (coord[0], coord[1] + j) not in coords:
                break
        else:
            return True

        for i in range(1, 10):
            if (coord[0] + i, coord[1]) not in coords:
                break
        else:
            return True
        
    return False


with open(f"tree.txt", "w") as f:
    for second in range(1, max_x * max_y):
        for i in range(len(coords)):
            x, y = coords[i]
            vx, vy = speeds[i]
            coords[i] = ((x + vx) % max_x, (y + vy) % max_y)

        if not is_interesting(coords):
            continue

        f.write(f"\n{second}\n")
        for j in range(max_y):
            for i in range(max_x):
                if (i, j) in coords:
                    f.write("#")
                else:
                    f.write(".")
            f.write("\n")
