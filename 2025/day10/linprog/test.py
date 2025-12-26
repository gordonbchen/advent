import numpy as np
from linprog import linprog, int_linprog


def print_problem(c: np.ndarray, A: np.ndarray, b: np.ndarray):
    z_expr = " + ".join([f"{ci} x{i}" for (i, ci) in enumerate(c)])
    print(f"Maximize: z = {z_expr}.")
    print("Subject to:")
    for i, bi in enumerate(b):
        lhs = " + ".join([f"{aij} x{j}" for (j, aij) in enumerate(A[i]) if aij != 0])
        print(f"\t{lhs} <= {bi}")
    vars = ", ".join([f"x{i}" for i in range(len(c))])
    print(f"\t{vars} >= 0\n")


def test_linprog(c: np.ndarray, A: np.ndarray, b: np.ndarray, x_sol: np.ndarray, z_sol: float, int_lp: bool = False):
    print_problem(c, A, b)
    lp_func = int_linprog if int_lp else linprog
    x, z = lp_func(c, A, b)
    print("\nSol:")
    print(f"x = {x}")
    print(f"z = {z}\n")
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


def test3():
    c = np.array([3, 2], dtype=np.float64)
    A = np.array([[-1, -1],
                  [1, 2]], dtype=np.float64)
    b = np.array([-2, 4], dtype=np.float64)
    x_sol = np.array([4, 0], dtype=np.float64)
    z_sol = 12
    test_linprog(c, A, b, x_sol, z_sol)


def test4():
    c = np.array([1, 1], dtype=np.float64)
    A = np.array([[8/5, 8/7    ],
                  [2,   16/15  ]], dtype=np.float64)
    b = np.array([4, 4], dtype=np.float64)
    x_sol = np.array([0, 3], dtype=np.float64)
    z_sol = 3
    test_linprog(c, A, b, x_sol, z_sol, int_lp=True)


if __name__ == "__main__":
    test0()
    test1()
    test2()
    test3()
    test4()
