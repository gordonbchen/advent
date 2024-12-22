import sys
import numpy as np
from collections import deque

PRUNE_NUM = 16_777_216
def calc_secret(num):
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

    all_window_vals = {}

    N_STEPS = 2_000
    for num in nums:
        window_vals = {}
        diff_window = deque([], maxlen=4)

        price = num % 10
        for i in range(N_STEPS):
            new_num = calc_secret(num)
            new_price = new_num % 10

            diff_window.append(new_price - price)

            if len(diff_window) == 4:
                diff_vals = tuple(diff_window)
                if diff_vals not in window_vals:
                    window_vals[diff_vals] = new_price

            price = new_price
            num = new_num

        for diff_vals, val in window_vals.items():
            if diff_vals not in all_window_vals:
                all_window_vals[diff_vals] = 0
            all_window_vals[diff_vals] += val

    max_window = max(all_window_vals, key=lambda x: all_window_vals[x]) 
    print(max_window, all_window_vals[max_window])
