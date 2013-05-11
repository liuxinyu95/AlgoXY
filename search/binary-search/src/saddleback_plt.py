#!/usr/bin/python

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

# instruction: change i from 0 to 2 to select plotting different function
i = 2

def brute_solve(f, z):
    return [(x, y) for x in range(z+1) for y in reversed(range (z+1)) if f(x, y) == z]

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

fs = [lambda x, y: x + y, lambda x, y: pow(2, x) + y, lambda x, y: x*x + y*y]
labels = ['x + y = 100', '2^x + y = 100', 'x^2 + y^2 = 100']

ps = brute_solve(fs[i], 100)
xs = [ x for x, _ in ps]
ys = [ y for _, y in ps]
zs = [ 100 for _ in ps]
ax.plot(xs, ys, zs)
ax.scatter(xs, ys, zs, label=labels[i])

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.legend()

plt.show()
