import sys

with open(sys.argv[1], "r") as f:
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

print(obstructs)
print(curr_pos)

UP = (-1, 0)
DOWN = (1, 0)
LEFT = (0, -1)
RIGHT = (0, 1)
directions = [UP, RIGHT, DOWN, LEFT]
curr_dir = UP

visited = set()
visited.add(curr_pos)

while True:
    new_pos = (curr_pos[0] + curr_dir[0], curr_pos[1] + curr_dir[1])
    if new_pos in obstructs:
        curr_dir = directions[(directions.index(curr_dir) + 1) % len(directions)]
    elif (new_pos[0] < 0) or (new_pos[0] >= n_rows) or (new_pos[1] < 0) or (new_pos[1] >= n_cols):
        break
    else:
        visited.add(new_pos)
        curr_pos = new_pos

print(f"Visited squares: {len(visited)}")
for i in range(n_rows):
    for j in range(n_cols):
        if (i, j) in visited:
            print("X", end="")
        elif (i, j) in obstructs:
            print("#", end="")
        else:
            print(".", end="")
    print()
