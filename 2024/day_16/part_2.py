import sys
import heapq
from collections import defaultdict

with open(sys.argv[1], "r") as f:
    text = f.read()

obj_map = [list(line) for line in text.split()]
for line in obj_map:
    print("".join(line))

walls = set()
for (i, line) in enumerate(obj_map):
    for (j, c) in enumerate(line):
        coord = i + (j * 1j)
        if c == "S":
            start = coord
        elif c == "E":
            end = coord
        elif c == "#":
            walls.add(coord)

to_visit = [(0, t := 0, start, 1j, set())]
best_path_coords = set()
coord_costs = defaultdict(lambda: float("inf"))
best_cost = float("inf")

while to_visit:
    cost, _, coord, direction, path = heapq.heappop(to_visit)
    
    if cost > coord_costs[(coord, direction)]:
        continue
    coord_costs[(coord, direction)] = cost
    path.add(coord)

    if (coord == end) and (cost <= best_cost):
        best_path_coords.update(path)
        best_cost = cost
        continue

    for (rotation, cost_inc) in [(1, 1), (1j, 1001), (-1j, 1001)]:
        next_direction = rotation * direction
        next_coord = coord + next_direction
        if next_coord not in walls:
            heapq.heappush(to_visit, (cost + cost_inc, t := t+1, next_coord, next_direction, path.copy()))

print(f"Total best visited coords: {len(best_path_coords)}")
