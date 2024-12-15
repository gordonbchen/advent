import sys

def parse_obj_map(obj_map):
    boxes = set()
    walls = set()

    for i, line in enumerate(obj_map.split("\n")):
        for j, c in enumerate(line):
            if c == "#":
                walls.add((i, 2 * j))
                walls.add((i, (2 * j) + 1))
            elif c == "O":
                boxes.add((i, 2 * j))
            elif c == "@":
                robot = (i, 2 * j)

    return robot, boxes, walls, (i + 1, 2 * (j + 1))

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

def get_to_move(coord, direction, boxes, walls, to_move):
    if coord in to_move:
        return
       
    to_move.add(coord)

    new_coord = (coord[0] + direction[0], coord[1] + direction[1])
    if new_coord in walls:
        return

    new_coords = []
    if new_coord in boxes:
        new_coords.append(new_coord)
        new_coords.append((new_coord[0], new_coord[1] + 1))
    else:
        left = (new_coord[0], new_coord[1] - 1)
        if left in boxes:
            new_coords.append(new_coord)
            new_coords.append(left)
        
    for new_coord in new_coords:
        get_to_move(new_coord, direction, boxes, walls, to_move)

def can_move(to_move, direction, walls):
    for coord in to_move:
        new_coord = (coord[0] + direction[0], coord[1] + direction[1])
        if new_coord in walls:
            return False
    return True

def move(to_move, direction, boxes, robot):
    moved = set()
    for coord in to_move:
        new_coord = (coord[0] + direction[0], coord[1] + direction[1])
        if coord in boxes:
            moved.add(new_coord)
        elif coord == robot:
            new_robot = new_coord

    boxes.difference_update(to_move)
    boxes.update(moved)
    return new_robot


if __name__ == "__main__":
    # Parse data.
    with open(sys.argv[1], "r") as f:
        data = f.read()

    obj_map, dir_symbols = data.split("\n\n")

    robot, boxes, walls, (max_i, max_j) = parse_obj_map(obj_map)
    display(robot, boxes, walls, max_i, max_j)

    dir_symbols = dir_symbols.replace("\n", "")
    print(dir_symbols, "\n\n")

    directions = {
        "<": (0, -1),
        ">": (0, 1),
        "^": (-1, 0),
        "v": (1, 0)
    }

    # Do stuff.
    for dir_symbol in dir_symbols:
        direction = directions[dir_symbol]
        
        to_move = set()
        get_to_move(robot, direction, boxes, walls, to_move)
        # print(to_move)

        if can_move(to_move, direction, walls):
            robot = move(to_move, direction, boxes, robot)

        # print(dir_symbol)
        # display(robot, boxes, walls, max_i, max_j)

    gps_sum = 0
    for box in boxes:
        gps_sum += (box[0] * 100) + box[1]

    print(f"\nGPS sum: {gps_sum}")