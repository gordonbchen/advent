import numpy as np
import matplotlib.pyplot as plt

with open("inputs/input.txt", "r") as f:
    nums = np.array([list(map(int, l.split(","))) for l in f.read().split()])
print(nums.shape)

plt.plot(nums[:, 0], nums[:, 1])
plt.savefig("vis.png")
