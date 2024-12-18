import sys
import heapq

filename = sys.argv[1]
with open(filename, "r") as f:
    lines = f.read().split()

MAX_COORD = 6 if filename == "test.txt" else 70
coord_range = range(0, MAX_COORD + 1)

def dist(i, j):
    return abs(j.real - i.real) + abs(j.imag - i.imag)

def bfs(start, end, bad):
    visited = {}
    min_path = []

    n = 0
    to_visit = [(dist(start, end), n, start, 0, [])]

    while to_visit:
        _, _, coord, i, path = heapq.heappop(to_visit)

        if i >= visited.get(coord, float("inf")):
            continue
        visited[coord] = i
        if coord == end:
            min_path = path
    
        new_i = i + 1
        for direction in range(4):
            next_coord = coord + (1j ** direction)
            if ((next_coord not in bad) and
                (next_coord.real in coord_range) and (next_coord.imag in coord_range)):
                n += 1
                heapq.heappush(to_visit, (dist(next_coord, end) + new_i, n, next_coord, new_i, path + [next_coord]))

    return min_path

start = 0 + 0j
end = MAX_COORD + (MAX_COORD * 1j)

N_STARTING_BYTES = 12 if filename == "test.txt" else 1024

bad = set()
path = None
for i, line in enumerate(lines):
    coord = list(map(int, line.split(",")))
    coord = coord[1] + (coord[0] * 1j)
    bad.add(coord)

    if i < N_STARTING_BYTES:
        continue

    if (not path) or (coord in path):
        path = bfs(start, end, bad)
        if not path:
            print(int(coord.imag), int(coord.real), sep=",")
            break
