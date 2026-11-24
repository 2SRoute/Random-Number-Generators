import matplotlib.pyplot as plt
import numpy as np # importing relevant libraries for plotting

x, y = np.loadtxt( # reading data from the .txt file
    fname = 'uniform_distribution.txt',
    delimiter = ',',
    unpack = True
)

plt.plot( # plotting the data
    x,
    y,
    color = '#AA00FF',
    linewidth = 0.67
)

plt.title("Uniform Pseudo-random Number Distribution")
plt.xlabel("Value")
plt.ylabel("Probability")

plt.show()