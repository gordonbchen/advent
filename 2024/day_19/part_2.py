import sys
from functools import cache

with open(sys.argv[1], "r") as f:
    data = f.read()

patterns, designs = data.split("\n\n")
patterns = set(patterns.split(", "))
designs = designs.split("\n")

print(patterns)
print(designs)

@cache
def is_possible(design):
    n_possible = 0

    if design in patterns:
        n_possible += 1

    filtered_patterns = patterns.copy()
    for i in range(len(design)):
        filtered_patterns = [p for p in filtered_patterns if (len(p) > i) and (p[i] == design[i])]
        if not filtered_patterns:
            break

        sub_design = design[:i+1]
        if sub_design in patterns:
            n_possible += is_possible(design[i+1:])
            
    return n_possible

possible = 0
for design in designs:
    n_possible = is_possible(design)
    print(design, n_possible)
    possible += n_possible

print(f"Possible: {possible}")