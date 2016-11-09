#!/usr/bin/env python

# ----------------------------------------------------------------------------
# Color space conversions

import numpy

WHITE_XYZ = numpy.array([
    0.95047,
    1.00000,
    1.08883,
])

CIELAB_D = 6.0 / 29.0
CIELAB_M = (29.0 / 6.0) ** 2 / 3.0
CIELAB_C = 4.0 / 29.0
CIELAB_A = 3.0
CIELAB_MATRIX = numpy.array([
    [0.0,  1.16,  0.0],
    [5.0, -5.0,   0.0],
    [0.0,  2.0,  -2.0],
]).transpose()
CIELAB_MATRIX_INV = numpy.linalg.inv(CIELAB_MATRIX)
CIELAB_OFFSET = numpy.array([
    -0.16,
    0.0,
    0.0,
])

def cielab_from_linear(x):
    return numpy.where(
        x <= CIELAB_D ** CIELAB_A,
        CIELAB_M * x + CIELAB_C,
        x ** (1.0 / CIELAB_A),
    )

def cielab_to_linear(y):
    return numpy.where(
        y <= CIELAB_D,
        (y - CIELAB_C) / CIELAB_M,
        y ** CIELAB_A,
    )

def cielab_to_xyz(lab):
    f_xyz = numpy.dot(lab - CIELAB_OFFSET, CIELAB_MATRIX_INV)
    return cielab_to_linear(f_xyz) * WHITE_XYZ

def cielab_from_xyz(xyz):
    f_xyz = cielab_from_linear(xyz / WHITE_XYZ)
    return numpy.dot(f_xyz, CIELAB_MATRIX) + CIELAB_OFFSET

SRGB_D = 0.04045
SRGB_M = 12.92
SRGB_A = 2.4
SRGB_K = 0.055
SRGB_MATRIX = numpy.array([
    [ 3.2406, -1.5372, -0.4986],
    [-0.9689,  1.8758,  0.0415],
    [ 0.0557, -0.2040,  1.0570],
]).transpose()
SRGB_MATRIX_INV = numpy.linalg.inv(SRGB_MATRIX)

def srgb_from_linear(x):
    x = numpy.clip(x, 0.0, 1.0)
    return numpy.where(
        x <= SRGB_D / SRGB_M,
        SRGB_M * x,
        (1.0 + SRGB_K) * x ** (1.0 / SRGB_A) - SRGB_K,
    )

def srgb_to_linear(y):
    y = numpy.clip(y, 0.0, 1.0)
    return numpy.where(
        y <= SRGB_D,
        y / SRGB_M,
        ((y + SRGB_K) / (1.0 + SRGB_K)) ** SRGB_A,
    )

def srgb_from_xyz(xyz):
    return srgb_from_linear(numpy.dot(xyz, SRGB_MATRIX))

def srgb_to_xyz(rgb):
    return numpy.dot(srgb_to_linear(rgb), SRGB_MATRIX_INV)

# ----------------------------------------------------------------------------
# Testing

import unittest
import numpy

class TestColorConversions(unittest.TestCase):

    def assertArrayEq(self, x, y, delta):
        self.assertLess(numpy.linalg.norm(x - y), delta)

    def test_cielab(self):
        white_cielab = cielab_from_xyz(WHITE_XYZ)
        self.assertArrayEq(white_cielab, [1.0, 0.0, 0.0], 1e-15)
        white_xyz = cielab_to_xyz(white_cielab)
        self.assertArrayEq(WHITE_XYZ, white_xyz, 1e-15)
        white_cielab2 = cielab_from_xyz(white_xyz)
        self.assertArrayEq(white_cielab, white_cielab2, 1e-15)

    def test_srgb(self):
        white_srgb = srgb_from_xyz(WHITE_XYZ)
        self.assertArrayEq(white_srgb, [1.0, 1.0, 1.0], 1e-4)
        white_xyz_srgb = srgb_to_xyz(white_srgb)
        white_srgb2 = srgb_from_xyz(white_xyz_srgb)
        self.assertArrayEq(white_srgb, white_srgb2, 1e-15)
        white_xyz_srgb2 = srgb_to_xyz(white_srgb2)
        self.assertArrayEq(white_xyz_srgb, white_xyz_srgb2, 1e-15)

# ----------------------------------------------------------------------------
# Demo

import matplotlib
import matplotlib.pyplot as plt
import numpy
import scipy.optimize

def srgb_closest_from_lab(lab):
    def badness(rgb):
        lab2 = cielab_from_xyz(srgb_to_xyz(rgb))
        return numpy.linalg.norm(lab - lab2, axis=-1)

    # initial guess
    rgb0 = srgb_from_xyz(cielab_to_xyz(lab))

    return scipy.optimize.minimize(badness, rgb0,
                                   bounds=((0.0, 1.0),) * 3, tol=1e-4).x

class LabColorSpacePlot(object):

    def __init__(self, fig, n=200,
                 background_color=(0.9, 0.9, 0.9),
                 interpolation="none",
                 transparency_factor=50.0,
                 optimize_rgb=False):
        '''
        fig: a matplotlib Figure object.

        n: density of the plot on each axis.

        background_color: color of the plot background in (r, g, b)
        with each component scaled between 0.0 and 1.0.

        interpolation: interpolation scheme used for matplotlib's imshow.

        transparency_factor: the transparency is determined by the distance
        between the approximate sRGB value and the true Lab value multiplied
        by this factor.  Using a larger factor will make the edge sharper.

        optimize_rgb: whether to optimize the sRGB values to minimize distance
        to the true CIELab values (in CIELab space), improving the appearance
        in regions where CIELab does not map onto sRGB exactly.  This can be
        very slow: it is best to reduce 'n' significantly and turn on
        interpolation to compensate.  If you turn this on, it's probably also
        a good idea to reduce transparency_factor to roughly 1 or so to
        actually see the effects of the changes.

        '''

        self.background_color = cielab_from_xyz(srgb_to_xyz(background_color))
        self.optimize_rgb = optimize_rgb
        self.transparency_factor = transparency_factor

        flags = []
        if interpolation != "none":
            flags.append("interpolation={!r}".format(interpolation))
        if optimize_rgb != False:
            flags.append("optimize_rgb={!r}".format(optimize_rgb))
        title_suffix = " ({})".format(", ".join(flags)) if flags else ""

        ax_image = fig.add_axes([0.1, 0.25, 0.8, 0.65])
        ax_slider = fig.add_axes([0.1, 0.1, 0.8, 0.05])

        l = 0.5
        a_max = 1.0
        b_max = a_max
        a_min = -a_max
        b_min = -b_max
        self.a, self.b = numpy.meshgrid(
            numpy.linspace(a_min, a_max, n),
            numpy.linspace(b_min, b_max, n),
        )
        self.image = ax_image.imshow(
            self.get_data(l),
            extent=(a_min, a_max, b_min, b_max),
            interpolation=interpolation,
            origin="lower",
        )
        ax_image.set_title("CIELab color space" + title_suffix)
        ax_image.set_xlabel("a")
        ax_image.set_ylabel("b")

        slider = matplotlib.widgets.Slider(
            ax_slider,
            "L",
            0.0,
            1.0,
            valinit=l,
        )
        slider.on_changed(self.on_slider_changed)

        # need to keep a reference to slider to avoid getting GC'ed
        fig.__l_slider = slider

    def on_slider_changed(self, value):
        self.image.set_data(self.get_data(value))

    def get_data(self, l):
        l = numpy.full(self.a.shape, l)
        lab = numpy.stack([l, self.a, self.b], axis=-1)
        return self.render_lab(lab)

    def render_lab(self, lab):
        if self.optimize_rgb:
            rgb = numpy.apply_along_axis(srgb_closest_from_lab, -1, lab)
        else:
            rgb = srgb_from_xyz(cielab_to_xyz(lab))

        # reverse the calculation to get the approximation error
        lab2 = cielab_from_xyz(srgb_to_xyz(rgb))
        err = numpy.linalg.norm(lab - lab2, axis=-1)

        # blend the plot and background in CIELab space;
        # note: c = complement of alpha
        c = numpy.clip(err * self.transparency_factor, 0.0, 1.0)
        c = c[..., numpy.newaxis]

        lab3 = (1.0 - c) * lab + c * self.background_color
        return srgb_from_xyz(cielab_to_xyz(lab3))

# ----------------------------------------------------------------------------
# Run tests + demo

import unittest

if __name__ == "__main__":
    t = unittest.main(exit=False)
    plt.style.use("ggplot")
    LabColorSpacePlot(
        plt.figure(),
        n=20,
        optimize_rgb=True,
        transparency_factor=0.0,
        interpolation="bicubic",
    )
    LabColorSpacePlot(
        plt.figure(),
        transparency_factor=100.0,
        interpolation="bicubic",
    )
    plt.show()
