import sys
from functools import cache

with open(sys.argv[1], "r") as f:
    data = f.read()

patterns, designs = data.split("\n\n")
patterns = set(patterns.split(", "))
designs = designs.split("\n")

print(patterns)
print(designs)

# I love the cache decorator!
# Cache if designs are possible.
@cache
def is_possible(design):
    if design in patterns:
        return True

    filtered_patterns = patterns.copy()
    for i in range(len(design)):
        filtered_patterns = [p for p in filtered_patterns if (len(p) > i) and (p[i] == design[i])]
        if not filtered_patterns:
            break

        sub_design = design[:i+1]
        if (sub_design in patterns) and is_possible(design[i+1:]):
            return True
            
    return False

possible = 0
for design in designs:
    if is_possible(design):
        possible += 1

print(f"Possible: {possible}")