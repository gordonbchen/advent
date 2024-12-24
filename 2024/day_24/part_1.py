import sys

def eval_expr(name, exprs, vals):
    if name in vals:
        return vals[name]
    
    a, op, b = exprs[name]
    a = eval_expr(a, exprs, vals)
    b = eval_expr(b, exprs, vals)
    
    match op:
        case "AND":
            c = a and b
        case "OR":
            c = a or b
        case "XOR":
            c = a ^ b

    vals[name] = c
    return c

if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        data = f.read()
    state, conns = data.split("\n\n")
    
    vals = {}
    for line in state.split("\n"):
        name, val = line.split(": ")
        vals[name] = int(val)
    print(vals)

    exprs = {}
    for line in conns.split("\n"):
        a, op, b, _, c = line.split()
        exprs[c] = (a, op, b)
    print(exprs)

    z_vals = {}
    for name in exprs:
        if name[0] == "z":
            z_val = eval_expr(name, exprs, vals)
            z_vals[name] = z_val
    print(z_vals)

    bits = "".join([str(vals[z]) for z in sorted(z_vals, reverse=True)])
    print(bits)
    print(int(bits, 2))
    