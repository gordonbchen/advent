import numpy as np


def pivot(T: np.ndarray, row: int, col: int) -> None:
    """Pivot on (row, col)."""
    T[row] /= T[row, col]
    sub_coeffs = T[:, col].copy()
    sub_coeffs[row] = 0
    T -= sub_coeffs[:, None] * T[row]


def simplex(T: np.ndarray, basic_vars: np.ndarray, entering_max: int, tol: float = 1e-12) -> None:
    """Run simplex in-place on the given tableau."""
    print("Simplex init:")
    print(f"basic vars: {basic_vars}")
    print(T)

    while (T[-1, :entering_max] < -tol).any():
        enter_idx = T[-1, :entering_max].argmin()
        if (T[:-1, enter_idx] <= tol).all():
            raise ValueError("Simplex fail: unbounded LP.")

        ratios = T[:-1, -1] / T[:-1, enter_idx]
        ratios[T[:-1, enter_idx] <= tol] = float("inf")
        leave_idx = ratios.argmin()

        pivot(T, leave_idx, enter_idx)
        basic_vars[leave_idx] = enter_idx

        print(f"\nenter={enter_idx}, leave={leave_idx}")
        print(f"basic vars: {basic_vars}")
        print(T, "\n")


def phase1(c: np.ndarray, A: np.ndarray, b: np.ndarray, tol: float = 1e-12) -> tuple[np.ndarray, np.ndarray]:
    """Simplex Phase 1: handles negative b, returns simplex tableau and basic vars."""
    b_pos_mask = b >= 0
    A_pos, b_pos = A[b_pos_mask], b[b_pos_mask]
    b_neg_mask = ~b_pos_mask
    A_neg, b_neg = A[b_neg_mask], b[b_neg_mask]
    (d_pos, n), (d_neg, _) = A_pos.shape, A_neg.shape

    # Tableau: [[ A_pos  I_s  0    0   |  b_pos],
    #           [-A_neg  0   -I_s  I_a | -b_neg],
    #           [ 0      0    0    1   |  0    ]]
    T = np.zeros((d_pos+d_neg+1, n+d_pos+(2*d_neg)+1), dtype=np.float64)
    T[:d_pos, :n] = A_pos
    T[:d_pos, n:n+d_pos] = np.eye(d_pos, dtype=np.float64)
    T[d_pos:-1, :n] = -A_neg
    T[d_pos:d_pos+d_neg, n+d_pos:n+d_pos+d_neg] = -np.eye(d_neg, dtype=np.float64)
    T[d_pos:d_pos+d_neg, n+d_pos+d_neg:-1] = np.eye(d_neg, dtype=np.float64)
    T[-1, n+d_pos+d_neg:-1] = np.ones(d_neg, dtype=np.float64)
    T[:d_pos, -1] = b_pos
    T[d_pos:d_pos+d_neg, -1] = -b_neg
    print("Phase 1")
    print("Tableau with artificials")
    print(T)

    # Eliminate artificials in objective row (artificials are basics).
    T[-1] -= T[d_pos:-1].sum(axis=0)
    print("\nEliminate artificials in objective")
    print(T, "\n")

    # Basic vars are slack vars for pos, artificials for neg.
    basic_vars = np.concat((np.arange(n, n+d_pos), np.arange(n+d_pos+d_neg, n+d_pos+(d_neg*2))), axis=0)

    # Maximize -sum of artificials, artificials = 0 finds BFS.
    simplex(T, basic_vars, entering_max=n+d_pos+d_neg)
    if abs(T[-1,-1]) > tol:
        raise ValueError("Phase 1 fail: cannot set artificials to 0, infeasible.")

    # Pivot out degenerates.
    keep_rows = []
    for i, bv in enumerate(basic_vars):
        # Non-artificial basic var. Do nothing.
        if bv < n+d_pos+d_neg:
            keep_rows.append(i)
            continue
        # Pivot artificial out.
        basic_cands = [j for j in range(n+d_pos+d_neg) if (j not in basic_vars) and (abs(T[i,j]) > tol)]
        if len(basic_cands) > 0:
            j = max(basic_cands, key=lambda j: abs(T[i, j]))
            pivot(T, i, j)
            basic_vars[i] = j
            keep_rows.append(i)
        elif (np.abs(T[i, :n+d_pos+d_neg]) > tol).any() or (abs(T[i, -1]) > tol):
            raise ValueError("Phase 1 fail: no non-artificial pivot and not redundant.")
    # Remove fully 0 rows.
    if len(keep_rows) < (T.shape[0] - 1):
        T = np.vstack([T[keep_rows, :], T[-1, :]])
        basic_vars = basic_vars[keep_rows]
        print(T)
        print(basic_vars)
    # Check that no artificials remain basic.
    if (basic_vars >= n+d_pos+d_neg).any():
        raise ValueError("Phase 1 fail: artificials not removed, still basic")

    # Remove artifical cols, use original objective row.
    newT = np.zeros((T.shape[0], n+d_pos+d_neg+1), dtype=np.float64)
    newT[:-1, :-1] = T[:-1, :n+d_pos+d_neg]
    newT[:-1, -1] = T[:-1, -1]
    newT[-1, :n] = -c
    T = newT
    print("Remove artificials, use original objective")
    print(T)

    # Eliminate basics in objective row.
    T[-1] -= (T[:-1] * T[-1, basic_vars][:, None]).sum(axis=0)
    print("\nEliminate basics in objective")
    print(T)
    return T, basic_vars


def linprog(c: np.ndarray, A: np.ndarray, b: np.ndarray) -> tuple[np.ndarray, float]:
    """Maximize c^T x, subject to Ax <= b, x >= 0."""
    (d, n) = A.shape
    if (b < 0).any():
        T, basic_vars = phase1(c, A, b)
    else:
        # Tableau: [[  A  I_s | b ],
        #           [ -c  0   | 0 ]]
        T = np.zeros((d + 1, n + d + 1), dtype=np.float64)
        T[:d, :n] = A
        T[-1, :n] = -c
        T[:-1, -1] = b
        T[:d, n:-1] = np.eye(d, dtype=np.float64)
        basic_vars = np.arange(n, n + d)

    print("\nPhase 2")
    simplex(T, basic_vars, entering_max=n)

    z = T[-1, -1]
    x = np.zeros(n, dtype=np.float64)
    for i, v in enumerate(basic_vars):
        if v < n: x[v] = T[i, -1]

    print("\nSol:")
    print(f"x = {x}")
    print(f"z = {z}\n")
    return x, z
