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

def is_bad_out(out, op, a, b, max_z, exprs):
    if out[0] == "z":
        if (op != "XOR") and (out != max_z):
            # Output bit, not carry, calculated with XOR.
            # Most significant bit is carry bit. 
            return True
    else:
        if set([a[0], b[0]]) != set(list("xy")):
            if op == "XOR":
                # Carry bit, calculated with AND or OR.
                return True

    if (set((a, b)) != set(("x00", "y00"))) and (set([a[0], b[0]]) == set(list("xy"))):
        if (op == "XOR"):
            for _, (new_a, new_op, new_b) in exprs.items():
                if (new_op == "XOR") and (out in (new_a, new_b)):
                    break
            else:
                # Output bit, must be added (XOR) to carry bit.
                # 0 is excluded b/c there is no carry to add.
                return True

        elif op == "AND":
            for _, (new_a, new_op, new_b) in exprs.items():
                if (new_op == "OR") and (out in (new_a, new_b)):
                    break
            else:
                # Carry bit, must be an OR to complete the carry op.
                return True

if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        data = f.read()
    state, conns = data.split("\n\n")
    
    vals = {}
    for line in state.split("\n"):
        name, val = line.split(": ")
        vals[name] = int(val)

    exprs = {}
    for line in conns.split("\n"):
        a, op, b, _, c = line.split()
        exprs[c] = (a, op, b)

    max_z = "z00"
    for name in exprs:
        if name[0] == "z":
            eval_expr(name, exprs, vals)
            max_z = max(max_z, name)

    bad_outs = []
    for out, (a, op, b) in exprs.items():
        if is_bad_out(out, op, a, b, max_z, exprs):
            bad_outs.append(out)

    print(len(bad_outs))
    print(",".join(sorted(bad_outs)))