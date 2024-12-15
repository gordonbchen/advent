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
seconds = 100

quadrant_counts = [0 for i in range(4)]  # top left to bottom right.

coords = {}
for nums in pattern.findall(text):
    x, y, vx, vy = (int(i) for i in nums)
    x = (x + (100 * vx)) % max_x
    y = (y + (100 * vy)) % max_y

    if (x == mid_x) or (y == mid_y):
        continue

    print(x, y)
    if (x, y) not in coords:
        coords[(x, y)] = 0
    coords[(x, y)] += 1

    x_quad = x > mid_x
    y_quad = y > mid_y
    quad = (int(x_quad) * 2) + int(y_quad)

    quadrant_counts[quad] += 1

for j in range(max_y):
    for i in range(max_x):
        print(coords.get((i, j), "."), end="")
    print()

print(quadrant_counts)
safety_factor = 1
for count in quadrant_counts:
    safety_factor *= count

print(f"Safety factor: {safety_factor}")
