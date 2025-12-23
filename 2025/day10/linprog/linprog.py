import numpy as np


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
