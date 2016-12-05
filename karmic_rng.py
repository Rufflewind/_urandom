import random, sys
import numpy as np
import matplotlib.pyplot as plt

def run_trials(alpha, num_trials, karmic=True, display=False):
    p = alpha
    trials = []
    for i in range(num_trials):
        success = random.random() < p
        if success:
            trials.append(1)
            if display:
                sys.stdout.write("X")
            if karmic:
                p *= alpha
        else:
            trials.append(0)
            if display:
                sys.stdout.write(" ")
            if karmic:
                p = 1 - (1 - p) * (1 - alpha)
        if display:
            sys.stdout.flush()
    if display:
        sys.stdout.write("\n")
    return {
        "net_p": sum(np.array(trials)) / float(num_trials),
    }

alpha = np.linspace(0.0, 1.0)
num_trials = 10000
plt.plot(alpha, [run_trials(alpha_i, num_trials)["net_p"]
                 for alpha_i in alpha])
plt.show()
