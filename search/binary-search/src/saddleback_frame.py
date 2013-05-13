#!/usr/bin/python

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

# instruction: change i from 0 to 2 to select plotting different function
i = 2

def wireframe(ax, f, m):
    xs = range(m)
    ys = range(m)
    xs, ys = np.meshgrid(xs, ys)
    zs = f(xs, ys)
    ax.plot_wireframe(xs, ys, zs)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

fs = [lambda x, y: x + y, lambda x, y: pow(2, x) + y, lambda x, y: x*x + y*y]

wireframe(ax, fs[i], 50)

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
#ax.legend()

plt.show()
