#!/usr/bin/env python
import os, sys, warnings
import colorspacious
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

def get_rainbow(Cp, Jp, count):
    # Adapted from the demo in https://youtu.be/xAoljeRJ3lU?t=14m40s
    # Nathaniel Smith and Stéfan van der Walt
    # “A Better Default Colormap for Matplotlib”
    # SciPy 2015 Conference
    t = (np.linspace(0.0, 1.0, count)) * 2.0 * np.pi
    ap = Cp * np.cos(t)
    bp = Cp * np.sin(t)
    Jpapbp = np.column_stack([np.full(ap.shape, Jp), ap, bp])
    with np.errstate(divide="ignore", invalid="ignore"):
        rgb = colorspacious.cspace_convert(Jpapbp, "CAM02-UCS", "sRGB255")
    clipped_rgb = np.clip(rgb, 0.0, 255.0)
    return np.ma.masked_array(clipped_rgb, clipped_rgb - rgb)

def binary_search_float(is_before, left, right, repeats):
    for _ in range(repeats):
        mid = (left + right) / 2.0
        if is_before(mid):
            left = mid
        else:
            right = mid
    return left

def exponential_search_float(is_before, start, coefficient, shift, precision):
    candidate = start
    while is_before(candidate):
        start = candidate
        candidate = coefficient * start + shift
    candidate = max(candidate, coefficient * start + shift)
    repeats = int(np.ceil(np.log2((candidate - start) / precision)))
    return binary_search_float(is_before, start, candidate, repeats)

def print_color(rgb):
    if np.ma.is_masked(rgb):
        sys.stdout.write(" ")
    else:
        sys.stdout.write("\x1b[48;2;{};{};{}m \x1b[0m".format(*map(int, rgb)))

def print_gradient(rgbs):
    for rgb in rgbs:
        print_color(rgb)

def print_gradient_and_hexcodes(rgbs, num_cols):
    print_gradient(np.ma.getdata(rgbs))
    sys.stdout.write("\n")
    print_gradient(rgbs)
    sys.stdout.write("\n\n")
    num_rows = (len(rgbs) - 1) // num_cols + 1
    sys.stdout.write("[")
    for i in range(num_rows):
        if i != 0:
            sys.stdout.write(" ")
        for j in range(num_cols):
            k = i * num_cols + j
            rgb = np.ma.getdata(rgbs)[k]
            sys.stdout.write('\x1b[38;2;{};{};{}m"#{}"\x1b[0m'.format(
                int(rgb[0]),
                int(rgb[1]),
                int(rgb[2]),
                "".join("{:02x}".format(int(x)) for x in rgb),
            ))
            if k == len(rgbs) - 1:
                break
            sys.stdout.write(",")
            if j != num_cols - 1:
                sys.stdout.write(" ")
        if k != len(rgbs) - 1:
            sys.stdout.write("\n")
    sys.stdout.write("]\n")

warnings.filterwarnings("ignore",category=matplotlib.cbook.mplDeprecation)

if len(sys.argv) not in (1, 3):
    sys.stderr.write("""
usage: {} [<J′> <C′>]

Specify J′ (lightness, 0 to 100) and C′ (chroma, 0 to ≈25) to see a specific
rainbow.  Otherwise, the program will show the most colorful rainbows at
various levels of J′.

If the color is not representable in sRGB, the lower part of the gradient will
be omitted, leading to gaps.

The program uses the CIECAM02-UCS color space from doi:10.1002/col.20227
calculated via the colorspacious library.
"""[1:].format(os.path.basename(__file__)))
    sys.stderr.flush()
    exit(1)

count = 64
num_cols = 4
precision = 1e-5

if len(sys.argv) == 3:
    Jp = float(sys.argv[1])
    Cp = float(sys.argv[2])
    rgbs = get_rainbow(Cp, Jp, count)
    print_gradient_and_hexcodes(rgbs, num_cols)

else:
    plt.ion()
    sys.stdout.write(" " * count + " {:>7} {:>7}\n".format("Jp", "Cp"))

    Jps = []
    Cps = []
    fig, ax = plt.subplots()
    [line] = ax.plot(Jps, Cps, "k")
    ax.set_title("maximum chroma rainbow at each lightness")
    ax.set_xlabel("Jp (lightness)")
    ax.set_ylabel("Cp (chroma)")
    ax.set_xlim(0.0, 100.0)
    ax.set_ylim(0.0, 30.0)

    for Jp in np.linspace(0.0, 100.0, 32):

        def is_before(Cp):
            return not np.ma.is_masked(get_rainbow(Cp, Jp, count))
        Cp = exponential_search_float(is_before, 0.0, 2.0, 1.0, precision)

        rgbs = get_rainbow(Cp, Jp, count)
        print_gradient(rgbs)
        sys.stdout.write(" {Jp:7.3f} {Cp:7.3f}\n".format(**locals()))

        Jps.append(Jp)
        Cps.append(Cp)
        line.set_data(Jps, Cps)
        plt.pause(np.finfo(float).eps) # give matplotlib a chance to re-render

    plt.show(block=True)
