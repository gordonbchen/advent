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

cheats = 0
time_diff_count = dict()
for coord, time in path_times.items():
    for rotation in range(4):
        skip_coord = coord + 2 * (1j ** rotation)
        if skip_coord in path_times:
            skip_time = path_times[skip_coord] + 2
            time_diff = time - skip_time

            if time_diff > 0:
                if time_diff not in time_diff_count:
                    time_diff_count[time_diff] = 0
                time_diff_count[time_diff] += 1 

            if time_diff >= 100:
                cheats += 1

print(time_diff_count)
print(cheats)