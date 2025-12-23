import numpy as np


def print_problem(c: np.ndarray, A: np.ndarray, b: np.ndarray):
    z_expr = " + ".join([f"{ci} x{i}" for (i, ci) in enumerate(c)])
    print(f"Maximize: z = {z_expr}.")
    print("Subject to:")
    for i, bi in enumerate(b):
        lhs = " + ".join([f"{aij} x{j}" for (j, aij) in enumerate(A[i]) if aij != 0])
        print(f"\t{lhs} <= {bi}")
    vars = ", ".join([f"x{i}" for i in range(len(c))])
    print(f"\t{vars} >= 0\n")


def print_sol(x: np.ndarray, z: float):
    print()
    print(", ".join([f"x{i}={v}" for (i, v) in enumerate(x)]))
    print(f"z={z}\n\n")


def solve_linprog(c: np.ndarray, A: np.ndarray, b: np.ndarray) -> tuple[np.ndarray, float]:
    """Maximize c^T x, subject to Ax <= b, x >= 0."""
    assert (b >= 0).all(), "Phase 1 not implemented, requires b >= 0"
    (d, n) = A.shape
    T = np.zeros((d + 1, n + d + 1), dtype=np.float64)
    T[:d, :n] = A
    T[-1, :n] = -c
    T[:-1, -1] = b
    T[:d, n:-1] = np.eye(d, dtype=np.float64)
    print(T)

    basic_vars = np.arange(n, n + d)
    print(basic_vars)
    while (T[-1, :n] < 0).any():
        enter_idx = T[-1, :n].argmin()

        if (T[:-1, enter_idx] <= 0).all(): raise ValueError("Unbounded LP solution.")
        ratios = T[:-1, -1] / T[:-1, enter_idx]
        ratios[T[:-1, enter_idx] <= 0] = float("inf")
        leave_idx = ratios.argmin()

        T[leave_idx] /= T[leave_idx, enter_idx]
        sub_coeffs = T[:, enter_idx].copy()
        sub_coeffs[leave_idx] = 0
        T -= sub_coeffs[:, None] * T[leave_idx]

        basic_vars[leave_idx] = enter_idx

        print(f"\nenter={enter_idx}, leave={leave_idx}")
        print(f"basic vars: {basic_vars}")
        print(T)

    z = T[-1, -1]
    x = np.zeros(n, dtype=np.float64)
    for i, v in enumerate(basic_vars):
        if v < n: x[v] = T[i, -1]
    return x, z


def test_linprog(c: np.ndarray, A: np.ndarray, b: np.ndarray, x_sol: np.ndarray, z_sol: float):
    print_problem(c, A, b)
    x, z = solve_linprog(c, A, b)
    print_sol(x, z)
    assert np.allclose(x, x_sol), f"x={x}, x_sol={x_sol}"
    assert np.allclose(z, z_sol), f"z={z}, z_sol={z_sol}"


def test0():
    c = np.array([3, 2], dtype=np.float64)
    A = np.array([[1, 1],
                  [1, 0],
                  [0, 1]], dtype=np.float64)
    b = np.array([4, 2, 3], dtype=np.float64)
    x_sol = np.array([2, 2], dtype=np.float64)
    z_sol = 10
    test_linprog(c, A, b, x_sol, z_sol)


def test1():
    c = np.array([1, 1], dtype=np.float64)
    A = np.array([[2, 1],
                  [1, 2]], dtype=np.float64)
    b = np.array([4, 4], dtype=np.float64)
    x_sol = np.array([4/3, 4/3], dtype=np.float64)
    z_sol = 8/3
    test_linprog(c, A, b, x_sol, z_sol)


def test2():
    c = np.array([4, 1, 4], dtype=np.float64)
    A = np.array([[2, 1, 1],
                  [1, 2, 3],
                  [2, 2, 1]], dtype=np.float64)
    b = np.array([2, 4, 8], dtype=np.float64)
    x_sol = np.array([0.4, 0, 1.2], dtype=np.float64)
    z_sol = 6.4
    test_linprog(c, A, b, x_sol, z_sol)


if __name__ == "__main__":
    test0()
    test1()
    test2()
