import numpy as np
import matplotlib.pyplot as plt
import matplotlib.widgets

def plot_with_sliders(pxys, params, slider_height=0.05):
    '''
    Create a plot with sliders for controlling parameters.

    :param pxys: {...p} -> [([x], [y])]
    :param params: {param_name: {
        "min": float,
        "max": float,
        "init": float,
        "step": float,
        "slider_kw": dict,
    }}
    :param slider_height: between 0 and 1
    '''
    fig, axs = plt.subplots(
        1 + len(params),
        squeeze=False,
        gridspec_kw={
            "hspace": 0.1 * (1 + len(params)),
            "height_ratios": [1.0] + [slider_height] * len(params),
        },
    )
    main_ax = axs[0, 0]
    slider_axs = axs[1:,0]
    params_items = list(params.items())
    xys = pxys(**dict((name, param["init"]) for name, param in params_items))
    lines = [main_ax.plot(x, y)[0] for x, y in xys]
    sliders = [
        matplotlib.widgets.Slider(
            slider_ax,
            name,
            param["min"],
            param["max"],
            valinit=param["init"],
            valstep=param["step"],
            **param.get("slider_kw", {}),
        )
        for slider_ax, (name, param) in zip(slider_axs, params_items)
    ]
    def update(val):
        xys = pxys(**dict(
            (name, slider.val)
            for (name, _), slider in zip(params_items, sliders)
        ))
        if len(xys) != len(lines):
            raise ValueError("number of curves may not vary")
        for line, (x, y) in zip(lines, xys):
            line.set_xdata(x)
            line.set_ydata(y)
        fig.canvas.draw_idle()
    for slider in sliders:
        slider.on_changed(update)
    # Make sure sliders don't get GCed.
    fig._fyl_sliders = sliders

def rolling_shutter(a = 5, n = 6, i0 = 0.5):
    '''
    Rolling shutter effect.

    :param a: speed
    :param n: number of propellers
    :param i0: initial rotation
    :returns: list of x-y curves
    '''
    y = np.linspace(-1, 1, 1000)
    xys = []
    for i in range(n // 2):
        b = 2 * np.pi * (i + i0) / n
        r = y / np.sin(a * y + b)
        x = np.where(np.abs(r) <= 1, r, np.nan) * np.cos(a * y + b)
        xys.append((x, y))
    return xys

plot_with_sliders(
    lambda a, i0: rolling_shutter(a=a, n=6, i0=i0),
    {
        "a": {
            "init": 5.0,
            "min": 0.01,
            "max": 30.0,
            "step": 0.01,
        },
        "i0": {
            "init": 0.5,
            "min": 0.0,
            "max": 1.0,
            "step": 0.01,
        },
    },
)
plt.show()
