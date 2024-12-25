import sys

def get_locks_keys():
    with open(sys.argv[1], "r") as f:
        text = f.read()

    locks = []
    keys = []

    for chunk in text.split("\n\n"):
        lines = chunk.split("\n")
        if lines[0][0] == "#":
            lock_heights = []
            for i in range(len(lines[0])):
                for j in range(len(lines)):
                    if lines[j][i] == ".":
                        lock_heights.append(j - 1)
                        break
            locks.append(lock_heights)
        else:
            key_heights = []
            for i in range(len(lines[0])):
                for j in range(len(lines)):
                    if lines[j][i] == "#":
                        key_heights.append(len(lines) - j - 1)
                        break
            keys.append(key_heights)

    return locks, keys, len(lines) - 2

def is_match(locks, keys, max_height):
    for l, k in zip(locks, keys):
        if l + k > max_height:
            return False
    return True

if __name__ == "__main__":
    locks, keys, max_height = get_locks_keys()
    print(locks)
    print(keys)
    print(max_height)

    matching = 0
    for lock in locks:
        for key in keys:
            matching += is_match(lock, key, max_height)

    print(f"Matching: {matching}")
