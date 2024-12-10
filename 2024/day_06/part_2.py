import sys
import time


def parse_data(filename: str) -> tuple[set[tuple[int, int]], tuple[int, int], tuple[int, int]]:
    with open(filename, "r") as f:
        text = f.read().split()

    obstructs = set()
    curr_pos: tuple[int, int] = tuple()

    n_rows = len(text)
    n_cols = len(text[0])

    for i, line in enumerate(text):
        for j, char in enumerate(line):
            if char == "#":
                obstructs.add((i, j))
            elif char == "^":
                curr_pos = (i, j)

    return obstructs, curr_pos, (n_rows, n_cols)


# Directions.
UP = (-1, 0)
DOWN = (1, 0)
LEFT = (0, -1)
RIGHT = (0, 1)
DIRECTIONS = [UP, RIGHT, DOWN, LEFT]

def get_next_dir(direction):
    return DIRECTIONS[(DIRECTIONS.index(direction) + 1) % len(DIRECTIONS)]


def get_next_pos(curr_pos, curr_dir):
    return (curr_pos[0] + curr_dir[0], curr_pos[1] + curr_dir[1])


def in_grid(pos, n_rows, n_cols):
    return ((pos[0] in range(0, n_rows)) and (pos[1] in range(0, n_cols)))


def is_loop(curr_pos, curr_dir, obstructs):
    visited = set()
    while True:
        next_pos = get_next_pos(curr_pos, curr_dir)
        
        if next_pos in obstructs:
            curr_dir = get_next_dir(curr_dir)
            continue
        elif not in_grid(next_pos, n_rows, n_cols):
            return False

        curr_pos = next_pos
        if (curr_pos, curr_dir) in visited:
            return True
        visited.add((curr_pos, curr_dir))


if __name__ == "__main__":
    obstructs, original_pos, (n_rows, n_cols) = parse_data(sys.argv[1])

    original_dir = UP
    curr_dir = original_dir

    curr_pos = original_pos

    visited = set()
    visited.add((curr_pos, curr_dir))

    t0 = time.time()

    new_obstructs = set()
    while True:
        next_pos = get_next_pos(curr_pos, curr_dir)
        
        if next_pos in obstructs:
            curr_dir = get_next_dir(curr_dir)
            continue
        elif not in_grid(next_pos, n_rows, n_cols):
            break

        # Check if adding an obstacle will result in an infinite loop.
        if next_pos not in new_obstructs:
            obstructs.add(next_pos)
            if is_loop(original_pos, original_dir, obstructs):
                new_obstructs.add(next_pos)
            obstructs.remove(next_pos)

        curr_pos = next_pos
        visited.add((curr_pos, curr_dir))

    print(f"\nTime elapsed: {time.time() - t0} sec")
    print(f"\nNew obstructs: {new_obstructs}")
    print(len(new_obstructs))  # too high 2119.
