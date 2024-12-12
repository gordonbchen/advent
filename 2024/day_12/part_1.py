import sys

# Parse data.
with open(sys.argv[1], mode="r") as f:
    data = f.read()

letters = [list(line) for line in data.split()]
print(letters)

# Do stuff.
neighbor_dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

def get_neighbors(coord, letters):
    neighbors = []
    for dir in neighbor_dirs:
        new_coord = (coord[0] + dir[0], coord[1] + dir[1])
        if not ((new_coord[0] in range(0, len(letters))) and
                (new_coord[1] in range(0, len(letters[0])))):
            continue

        if letters[new_coord[0]][new_coord[1]] == letters[coord[0]][coord[1]]:
            neighbors.append(new_coord)
    return neighbors

visited = set()

cost = 0
for i in range(len(letters)):
    for j in range(len(letters[0])):
        if (i, j) not in visited:
            area = 0
            perimeter = 0

            to_visit = [(i, j)]
            while to_visit:
                coord = to_visit.pop()
                if coord in visited:
                    continue
                visited.add(coord)

                neighbors = get_neighbors(coord, letters)
                to_visit.extend(neighbors)
                area += 1
                perimeter += 4 - len(neighbors)

            cost += area * perimeter

print(f"Cost: {cost}")
