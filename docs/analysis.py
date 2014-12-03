import numpy as np
import matplotlib.pyplot as plt

def plot_activation_functions(neuron_type):
    x = np.arange(-10.,10.01,0.01)
    y = np.loadtxt(neuron_type + ".txt")
    plt.figure()
    plt.title(neuron_type.title() + " Activation Graph")
    plt.plot(x, y)
    plt.savefig(neuron_type + ".png")

def main():
    for activation in ["sigmoid", "reclu", "tanh"]:
        plot_activation_functions(activation)

if __name__ == '__main__':
    main()
