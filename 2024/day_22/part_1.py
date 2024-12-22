import sys

PRUNE_NUM = 16_777_216
def calc_secret(num, n_steps):
    for i in range(n_steps):
        num ^= num * 64
        num %= PRUNE_NUM

        num ^= num // 32
        num %= PRUNE_NUM

        num ^= num * 2048
        num %= PRUNE_NUM

    return num

if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        nums = list(map(int, f.read().split()))

    N_STEPS = 2_000

    total = 0
    for num in nums:
        total += calc_secret(num, N_STEPS)

    print(total)
