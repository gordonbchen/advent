import sys
import re
import sympy

with open(sys.argv[1], "r") as f:
    text = f.read()

pattern = re.compile(r"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)", re.MULTILINE)

cost = 0

for nums in pattern.findall(text):
    nums = [int(i) for i in nums]
    ax, ay, bx, by, x, y = nums

    augmented_matrix = sympy.Matrix([[ax, bx, x], [ay, by, y]])
    rref_matrix, pivot_cols = augmented_matrix.rref()
    ca, cb = rref_matrix[:, 2]

    if ca.is_integer and cb.is_integer:
        cost += (3 * ca) + cb

# LINEAR ALGEBRA FTW!
print(f"Cost: {cost}")