import sys
import numpy as np
from scipy.optimize import linprog

filename = "input.txt" if sys.argv[-1] == 'i' else "test.txt"
with open("inputs/" + filename, "r") as f:
    lines = f.read().split("\n")[:-1]

def to_vector(idxs: list[int], n: int) -> np.ndarray:
    x = np.zeros(n, dtype=np.float64)
    for i in idxs:
        x[i] = 1
    return x

total_presses = 0
for line in lines:
    parts = line.split()
    target = np.array([int(i) for i in parts[-1][1:-1].split(",")], dtype=np.float64)
    bases = np.stack([to_vector([int(i) for i in x[1:-1].split(",")], len(target)) for x in parts[1:-1]]).T
    print(target)
    print(bases)
    print()

    c = np.ones(bases.shape[1], dtype=np.float64)
    print(c)

    x = linprog(c, A_eq=bases, b_eq=target, integrality=1).x
    z = x.sum()
    print(f"z: {z}")
    print(x)
    total_presses += z
print(f"total_presses: {total_presses}")
