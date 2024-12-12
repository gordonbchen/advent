import sys

# Parse data.
with open(sys.argv[1], mode="r") as f:
    data = f.read()

letters = [list(line) for line in data.split()]

# Do stuff.
neighbor_dirs = [(0, -1), (-1, 0), (0, 1), (1, 0)]
diag_dirs = [(-1, -1), (-1, 1), (1, 1), (1, -1)]

def get_neighbors(coord, letters, visited):
    neighbors = []
    for direction in neighbor_dirs:
        new_coord = get_new_coord(coord, direction)
        if is_same_letter(coord, new_coord, letters) and (new_coord not in visited):
            neighbors.append(new_coord)
    return neighbors

def get_new_coord(coord, direction):
    new_coord = (coord[0] + direction[0], coord[1] + direction[1])
    return new_coord

def is_same_letter(coord, new_coord, letters):
    if not ((new_coord[0] in range(0, len(letters))) and
            (new_coord[1] in range(0, len(letters[0])))):
        return False

    return (letters[new_coord[0]][new_coord[1]] == letters[coord[0]][coord[1]])

all_visited = set()

cost = 0
for i in range(len(letters)):
    for j in range(len(letters[0])):
        if (i, j) in all_visited:
            continue

        area = 0
        visited = set()
        to_visit = [(i, j)]

        while to_visit:
            coord = to_visit.pop()
            if coord in visited:
                continue
            visited.add(coord)

            neighbors = get_neighbors(coord, letters, visited)
            to_visit.extend(neighbors)
            area += 1

        corners = 0
        for coord in visited:
            for k in range(4):
                is_outer_corner = not (
                    get_new_coord(coord, neighbor_dirs[k]) in visited or
                    get_new_coord(coord, neighbor_dirs[(k + 1) % 4]) in visited
                )
                is_inner_corner = (
                    (
                        get_new_coord(coord, neighbor_dirs[k]) in visited and
                        get_new_coord(coord, neighbor_dirs[(k + 1) % 4]) in visited
                    )
                    and not (get_new_coord(coord, diag_dirs[k]) in visited)
                )
                if is_outer_corner or is_inner_corner:
                    corners += 1 
                    print(coord)

        print(letters[i][j], area, corners)

        cost += area * corners
        all_visited.update(visited)

print(f"Cost: {cost}")
