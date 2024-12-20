import sys

with open(sys.argv[1], "r") as f:
    data = f.read()

path_times = {}
for i, line in enumerate(data.split()):
    for j, c in enumerate(line):
        if c in ("S", "E", "."):
            coord = i + (j * 1j)
            if c == "S":
                start = coord
            elif c == "E":
                end = coord

            path_times[coord] = None

def calc_time(start, end, path_times):
    coord = start
    time = 0

    while True:
        path_times[coord] = time
        time += 1

        if coord == end:
            break

        for rotation in range(4):
            adj_coord = coord + (1j ** rotation)
            if (adj_coord in path_times) and (path_times[adj_coord] == None):
                coord = adj_coord
                break

calc_time(start, end, path_times)
print(path_times)
print(path_times[end])

def manhattan_dist(c1, c2):
    return int(abs(c1.real - c2.real) + abs(c1.imag - c2.imag))

cheats = 0
time_diff_count = dict()
for coord, time in path_times.items():
    for skip_coord, skip_time in path_times.items():
        dist = manhattan_dist(coord, skip_coord)
        if dist > 20:
            continue

        skip_time += dist
        time_diff = time - skip_time

        if time_diff >= 50:
            if time_diff not in time_diff_count:
                time_diff_count[time_diff] = 0
            time_diff_count[time_diff] += 1 

        if time_diff >= 100:
            cheats += 1

print(time_diff_count)
print(cheats)