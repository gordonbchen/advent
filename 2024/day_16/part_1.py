import sys
from queue import PriorityQueue

with open(sys.argv[1], "r") as f:
    text = f.read()

obj_map = [list(line) for line in text.split()]
for line in obj_map:
    print(line)

walls = set()
for (i, line) in enumerate(obj_map):
    for (j, c) in enumerate(line):
        coord = (i, j)
        if c == "S":
            start = coord
        elif c == "E":
            end = coord
        elif c == "#":
            walls.add(coord)

STEP_COST = 1
TURN_COST = 1_000

DIRECTIONS = [(-1, 0), (0, 1), (1, 0), (0, -1)]
START_DIR = DIRECTIONS[1]

to_visit = PriorityQueue()
to_visit.put((0, (start, START_DIR)))

visited = set()

while True:
    cost, (coord, direction) = to_visit.get()
    
    if (coord, direction) in visited:
        continue
    visited.add((coord, direction))

    if coord == end:
        print(f"\n\nCost: {cost}")
        break

    next_coord = (coord[0] + direction[0], coord[1] + direction[1])
    if (next_coord not in walls) and ((next_coord, direction) not in visited):
        to_visit.put((cost + STEP_COST, (next_coord, direction)))

    for dir_inc in (-1, 1):
        next_direction = DIRECTIONS[(DIRECTIONS.index(direction) + dir_inc) % len(DIRECTIONS)]
        if (coord, next_direction) not in visited:
            to_visit.put((cost + TURN_COST, (coord, next_direction)))
