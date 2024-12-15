import sys

def parse_obj_map(obj_map):
    boxes = set()
    walls = set()

    for i, line in enumerate(obj_map.split("\n")):
        for j, c in enumerate(line):
            if c == "#":
                walls.add((i, j))
            elif c == "O":
                boxes.add((i, j))
            elif c == "@":
                robot = (i, j)

    return robot, boxes, walls, (i + 1, j + 1)

def display(robot, boxes, walls, max_i, max_j):
    for i in range(max_i):
        for j in range(max_j):
            if (i, j) in boxes:
                c = "O"
            elif (i, j) in walls:
                c = "#"
            elif (i, j) == robot:
                c = "@"
            else:
                c = "."

            print(c, end="")
        print()
    print()

def move(coord, direction, boxes, walls):
    new_coord = (coord[0] + direction[0], coord[1] + direction[1])
    if new_coord in walls:
        return coord
    elif new_coord in boxes:
        if move(new_coord, direction, boxes, walls) != new_coord:
            if coord in boxes:
                boxes.remove(coord)
                boxes.add(new_coord)
            return new_coord
        else:
            return coord
    else:
        if coord in boxes:
            boxes.remove(coord)
            boxes.add(new_coord)
        return new_coord


if __name__ == "__main__":
    # Parse data.
    with open(sys.argv[1], "r") as f:
        data = f.read()

    obj_map, dir_symbols = data.split("\n\n")

    robot, boxes, walls, (max_i, max_j) = parse_obj_map(obj_map)
    display(robot, boxes, walls, max_i, max_j)

    dir_symbols = dir_symbols.replace("\n", "")
    print(dir_symbols)

    directions = {
        "<": (0, -1),
        ">": (0, 1),
        "^": (-1, 0),
        "v": (1, 0)
    }

    # Do stuff.
    for dir_symbol in dir_symbols:
        direction = directions[dir_symbol]
        robot = move(robot, direction, boxes, walls)

        # display(robot, boxes, walls, max_i, max_j)

    gps_sum = 0
    for box in boxes:
        gps_sum += (box[0] * 100) + box[1]

    print(f"\nGPS sum: {gps_sum}")