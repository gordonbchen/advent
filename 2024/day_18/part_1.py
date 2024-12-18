import sys
import heapq

filename = sys.argv[1]
with open(filename, "r") as f:
    lines = f.read().split()

N_BYTES = 12 if filename == "test.txt" else 1024

bad = set()
for i in range(N_BYTES):
    coord = list(map(int, lines[i].split(",")))
    bad.add(coord[1] + (coord[0] * 1j))

MAX_COORD = 6 if filename == "test.txt" else 70
coord_range = range(0, MAX_COORD + 1)

print(bad)
for i in coord_range:
    for j in coord_range:
        if (i + (j * 1j)) in bad:
            print("#", end="")
        else:
            print(".", end="")
    print()

def dist(i, j):
    return abs(j.real - i.real) + abs(j.imag - i.imag)

def bfs(start, end, bad):
    visited = {}

    n = 0
    to_visit = [(dist(start, end), n, start, 0)]

    while to_visit:
        _, _, coord, i = heapq.heappop(to_visit)

        if i >= visited.get(coord, float("inf")):
            continue
        visited[coord] = i
    
        new_i = i + 1
        for direction in range(4):
            next_coord = coord + (1j ** direction)
            if ((next_coord not in bad) and
                (next_coord.real in coord_range) and (next_coord.imag in coord_range)):
                n += 1
                heapq.heappush(to_visit, (dist(next_coord, end) + new_i, n, next_coord, new_i))

    return visited[end]

start = 0 + 0j
end = MAX_COORD + (MAX_COORD * 1j)
print(bfs(start, end, bad))
