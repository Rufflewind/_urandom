"use strict";

// TODO add a way to disable the masking

var ciebase = require("ciebase");
var ciecam02 = require("ciecam02");
var cielab = require("./cielab");

var cam = ciecam02.cam({
    whitePoint: ciebase.illuminant.D65,
    adaptingLuminance: 40,
    backgroundLuminance: 20,
    surroundType: "average",
    discounting: false
}, ciecam02.cfs("JCh"));

var xyz = ciebase.xyz(ciebase.workspace.sRGB, ciebase.illuminant.D65);

function leadingZeros(n, s) {
    s = String(s);
    return Array(n - s.length + 1).join("0") + s;
}

function signedLeadingZeros(n, s) {
    s = String(s);
    if (s[0] == "-") {
        return "-" + Array(n - s.length + 2).join("0") + s.slice(1);
    } else {
        return "+" + Array(n - s.length + 1).join("0") + s;
    }
}

function labToJch(lab) {
    var j = lab[0] * 100.0;
    var c = Math.sqrt(lab[1] * lab[1] + lab[2] * lab[2]) * 100.0;
    var h = Math.atan2(lab[2], lab[1]) * 180.0 / Math.PI;
    if (h < 0) {
        h += 360.0;
    }
    return [j, c, h];
}

function jchToLab(jch) {
    var r = jch[1] / 100.0;
    var t = jch[2] * Math.PI / 180.0;
    return [
        jch[0] / 100.0,
        r * Math.cos(t),
        r * Math.sin(t)
    ];
}

function cabToJch(cab) {
    var x = cab[1];
    var y = cab[2];
    var h = Math.atan2(y, x);
    if (h < 0.0) {
        h += 2 * Math.PI;
    }
    return [
        100.0 * (1.0 - Math.sqrt(x * x + y * y)),
        100.0 * cab[0],
        h * 180.0 / Math.PI
    ];
}

function inpToLab(inp, maxChroma) {
    return [
        inp[2],
        (2.0 * inp[0] - 1.0) * maxChroma,
        (2.0 * inp[1] - 1.0) * maxChroma
    ];
}

function labToInp(lab, maxChroma) {
    return [
        (lab[1] / maxChroma + 1.0) / 2.0,
        (lab[2] / maxChroma + 1.0) / 2.0,
        lab[0]
    ];
}

function inpToCab(inp, maxChroma) {
    return [
        maxChroma * inp[2],
        2.0 * inp[0] - 1.0,
        2.0 * inp[1] - 1.0
    ];
}

function cabToInp(cab, maxChroma) {
    return [
        (cab[1] + 1.0) / 2.0,
        (cab[2] + 1.0) / 2.0,
        cab[0] / maxChroma
    ];
}

function clipRgb(rgb) {
    var inGamut = true;
    if (rgb[0] < 0.0) {
        inGamut = false;
        rgb[0] = 0.0;
    } else if (rgb[0] > 1.0) {
        inGamut = false;
        rgb[0] = 1.0;
    }
    if (rgb[1] < 0.0) {
        inGamut = false;
        rgb[1] = 0.0;
    } else if (rgb[1] > 1.0) {
        inGamut = false;
        rgb[1] = 1.0;
    }
    if (rgb[2] < 0.0) {
        inGamut = false;
        rgb[2] = 0.0;
    } else if (rgb[2] > 1.0) {
        inGamut = false;
        rgb[2] = 1.0;
    }
    return inGamut;
}

// https://stackoverflow.com/a/5624139
function hexToRgb(hex) {
    var m;
    m = /(\d+)\s*,\s*(\d+)\s*,\s*(\d+)/.exec(hex);
    if (m) {
        var rgb = [parseInt(m[1]) / 255.0,
                   parseInt(m[2]) / 255.0,
                   parseInt(m[3]) / 255.0];
        clipRgb(rgb);
        return rgb;
    }
    hex = hex.replace(/^#?([a-f\d])([a-f\d])([a-f\d])$/i, function(m, r, g, b) {
        return r + r + g + g + b + b;
    });
    m = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    if (!m) {
        return null;
    }
    return [parseInt(m[1], 16) / 255.0,
            parseInt(m[2], 16) / 255.0,
            parseInt(m[3], 16) / 255.0];
}

// https://stackoverflow.com/a/19765382
function rgbToHex(rgb) {
    var r = rgb[0] * 255.0;
    var g = rgb[1] * 255.0;
    var b = rgb[2] * 255.0;
    return "#" + (0x1000000 + ((r << 16) | (g << 8) | b)).toString(16).slice(1);
}

function trivialConverter(c) {
    return {value: c, inGamut: true};
}

function circularMask(ctx, width, height) {
    ctx.beginPath();
    ctx.arc(width / 2.0, height / 2.0, width / 2.0, 0, Math.PI * 2);
    ctx.closePath();
    ctx.clip();
}

var VIEW_SRGB = {
    mask: function(ctx) {},
    fromSrgb: trivialConverter,
    toSrgb: trivialConverter
};

var VIEW_LAB_HCL = {
    mask: circularMask,
    display: function(lab) {
        return "(L=" + leadingZeros(3, (lab[0] * 100.0).toFixed(0))
             + ", a=" + signedLeadingZeros(3, (lab[1] * 100.0).toFixed(0))
             + ", b=" + signedLeadingZeros(3, (lab[2] * 100.0).toFixed(0))
             + ")";
    },
    transform: inpToLab,
    untransform: labToInp,
    toSrgb: function(lab) {
        return cielab.cielabToSrgb(lab);
    },
    fromSrgb: function(rgb) {
        return cielab.srgbToCielab(rgb);
    }
};

var VIEW_LAB_HLC = {
    mask: circularMask,
    display: function(cab) {
        var x = cab[1];
        var y = cab[2];
        var r = Math.sqrt(x * x + y * y);
        var h = Math.atan2(y, x);
        if (h < 0.0) {
            h += 2 * Math.PI;
        }
        h *= 180.0 / Math.PI;
        return "(H=" + leadingZeros(3, h.toFixed(0))
             + "\xb0, C=" + leadingZeros(3, (cab[0] * 100.0).toFixed(0))
             + ", L=" + leadingZeros(3, ((1.0 - r) * 100.0).toFixed(0)) + ")";
    },
    transform: inpToCab,
    untransform: cabToInp,
    toSrgb: function(cab) {
        var x = cab[1];
        var y = cab[2];
        var r = Math.sqrt(x * x + y * y);
        var lab;
        if (r < 1e-8) {
            lab = [1.0, 0.0, 0.0];
        } else {
            lab = [1.0 - r, cab[0] * x / r, cab[0] * y / r];
        }
        return cielab.cielabToSrgb(lab);
    },
    fromSrgb: function(rgb) {
        var res = cielab.srgbToCielab(rgb);
        var lab = res.value;
        var r = 1.0 - lab[0];
        var c = Math.sqrt(lab[1] * lab[1] + lab[2] * lab[2]);
        var cab = [c, 0.0, 0.0];
        if (c < 1e-8) {
            cab[1] = r;
        } else {
            cab[1] = lab[1] / c * r;
            cab[2] = lab[2] / c * r;
        }
        return {
            value: cab,
            inGamut: res.inGamut
        };
    }
};

var VIEW_CAM02_HCL = {
    mask: circularMask,
    display: function(lab) {
        var jch = labToJch(lab);
        return "(J=" + leadingZeros(3, jch[0].toFixed(0))
             + ", C=" + leadingZeros(3, jch[1].toFixed(0))
             + ", h=" + leadingZeros(3, jch[2].toFixed(0)) + "\xb0)";
    },
    transform: inpToLab,
    untransform: labToInp,
    toSrgb: function(lab) {
        var jch = labToJch(lab);
        jch = {J: jch[0], C: jch[1], h: jch[2]};
        var rgb = xyz.toRgb(cam.toXyz(jch));
        var inGamut = clipRgb(rgb);
        return {value: rgb, inGamut: inGamut};
    },
    fromSrgb: function(rgb) {
        var jch = cam.fromXyz(xyz.fromRgb(rgb));
        return {value: jchToLab([jch.J, jch.C, jch.h]), inGamut: true};
    }
};

var VIEW_CAM02_HLC = {
    mask: circularMask,
    display: function(lab) {
        var jch = cabToJch(lab);
        return "(J=" + leadingZeros(3, jch[0].toFixed(0))
             + ", C=" + leadingZeros(3, jch[1].toFixed(0))
             + ", h=" + leadingZeros(3, jch[2].toFixed(0)) + "\xb0)";
    },
    transform: inpToCab,
    untransform: cabToInp,
    toSrgb: function(cab) {
        var jch = cabToJch(cab);
        jch = {J: jch[0], C: jch[1], h: jch[2]};
        var rgb = xyz.toRgb(cam.toXyz(jch));
        var inGamut = clipRgb(rgb);
        return {value: rgb, inGamut: inGamut};
    },
    fromSrgb: function(rgb) {
        var jch = cam.fromXyz(xyz.fromRgb(rgb));
        var r = 1.0 - jch.J / 100.0;
        var t = jch.h * Math.PI / 180.0;
        var cab = [jch.C / 100.0, r * Math.cos(t), r * Math.sin(t)];
        return {value: cab, inGamut: true};
    }
};

function ColorPicker(canvas) {
    this.ctx = canvas.getContext("2d");
    this.width = canvas.width;
    this.height = canvas.height;
    this.img = this.ctx.createImageData(this.width, this.height);

    this.colorState = new ColorState(VIEW_SRGB, [0.65, 0.65, 0.65]);
    this.view = VIEW_SRGB; // this needn't be the same as colorState.view!
    this.maxChroma = 0.75;
    this.currentHash = "";

    this.onsave = function(value) {};
    this.ondraw = function() {};

    this.drawScheduled = false;
    this.drawFull = false;

    this.redraw(true);
}

ColorPicker.prototype.cartesian = function(toSrgb, transform, sliderValue) {
    var data = this.img.data;
    var width = this.width;
    var height = this.height;
    var maxChroma = this.maxChroma;
    var inp = [0.0, 0.0, sliderValue];
    for (var y = 0; y < height; ++y) {
        inp[1] = 1.0 - y / height;
        for (var x = 0; x < width; ++x) {
            inp[0] = x / width;
            var r = toSrgb(transform(inp, maxChroma));
            var rgb = r.value;
            if (!r.inGamut) {
                var desat = 0.5;
                var dim = 0.1;
                var avg = (rgb[0] + rgb[1] + rgb[2]) / 3.0;
                rgb[0] = (rgb[0]) * (1.0 - desat) + avg * desat - dim;
                rgb[1] = (rgb[1]) * (1.0 - desat) + avg * desat - dim;
                rgb[2] = (rgb[2]) * (1.0 - desat) + avg * desat - dim;
            }
            data[0 + 4 * (x + width * y)] = 255.0 * rgb[0];
            data[1 + 4 * (x + width * y)] = 255.0 * rgb[1];
            data[2 + 4 * (x + width * y)] = 255.0 * rgb[2];
            data[3 + 4 * (x + width * y)] = 255.0;
        }
    }
};

ColorPicker.prototype.redraw = function(full) {
    this.drawFull = this.drawFull || full;
    if (this.drawScheduled) {
        return;
    }
    this.drawScheduled = true;
    var self = this;
    window.requestAnimationFrame(function() {
        self.ondraw();

        var inp = self.getColor();
        var cursorX = inp[0] * self.width;
        var cursorY = (1.0 - inp[1]) * self.height;

        if (self.drawFull) {
            console.time("redraw");
            self.cartesian(self.view.toSrgb, self.view.transform, inp[2]);
            console.timeEnd("redraw");
        }

        createImageBitmap(self.img).then(function(img) {
            var ctx = self.ctx;

            ctx.clearRect(0, 0, self.width, self.height);
            ctx.save();
            self.view.mask(ctx, self.width, self.height);
            ctx.drawImage(img, 0, 0);
            ctx.restore();

            ctx.strokeStyle = "white";
            ctx.beginPath();
            ctx.arc(cursorX, cursorY, 4, 0, 2 * Math.PI);
            ctx.stroke();

            ctx.strokeStyle = "black";
            ctx.beginPath();
            ctx.arc(cursorX, cursorY, 5, 0, 2 * Math.PI);
            ctx.stroke();
        });

        self.drawScheduled = false;
        self.drawFull = false;
    });
};

ColorPicker.prototype.getColor = function() {
    return this.view.untransform(this.colorState.get(this.view).value,
                                 this.maxChroma);
};

ColorPicker.prototype.setColor = function(inp) {
    var oldInpZ = this.getColorInpZ();
    this.colorState.set(this.view, this.view.transform(inp, this.maxChroma));
    this.redraw(oldInpZ != inp[2]);
};

ColorPicker.prototype.getColorInpZ = function() {
    return this.getColor()[2];
};

ColorPicker.prototype.setColorInpZ = function(value) {
    var inp = this.getColor().slice();
    inp[2] = value;
    this.setColor(inp);
};

ColorPicker.prototype.getColorDisplay = function() {
    return this.view.display(this.colorState.get(this.view).value);
};

ColorPicker.prototype.getTextColor = function() {
    var rgb = this.colorState.get(VIEW_SRGB).value;
    return {
        hex: rgbToHex(rgb),
        rgb: "rgb("
           + (rgb[0] * 255.0).toFixed(0) + ", "
           + (rgb[1] * 255.0).toFixed(0) + ", "
           + (rgb[2] * 255.0).toFixed(0)
           + ")",
    }
};

ColorPicker.prototype.saveTextColor = function(text) {
    var rgb = hexToRgb(text);
    if (rgb == null) {
        return;
    }
    this.colorState.set(VIEW_SRGB, rgb);
    this.save();
    this.redraw(true);
};

ColorPicker.prototype.setView = function(view) {
    this.view = view;
    this.redraw(true);
};

ColorPicker.prototype.setMaxChromaPercent = function(value) {
    this.maxChroma = Number(value) / 100.0;
    this.redraw(true);
};

ColorPicker.prototype.restore = function(hash) {
    if (hash == this.currentHash) {
        return;
    }
    var rgb = hexToRgb(hash);
    if (rgb == null) {
        return;
    }
    this.currentHash = hash;
    this.colorState.set(VIEW_SRGB, rgb);
    this.redraw(true);
};

ColorPicker.prototype.save = function() {
    this.currentHash = this.colorState.save()
    this.onsave(this.currentHash);
};

function ColorState(view, color) {
    this.set(view, color);
}

ColorState.prototype.get = function(view) {
    if (this.view == view) {
        return {value: this.color, inGamut: true};
    }
    var r1 = this.view.toSrgb(this.color);
    var r2 = view.fromSrgb(r1.value);
    return {value: r2.value, inGamut: r1.inGamut && r2.inGamut};
};

ColorState.prototype.set = function(view, color, save) {
    this.view = view;
    this.color = color;
};

ColorState.prototype.save = function() {
    return rgbToHex(this.get(VIEW_SRGB).value);
};

function main() {
    var canvas = document.getElementById("canvas");
    canvas.width = 400;
    canvas.height = 400;
    var colorPicker = new ColorPicker(canvas);

    colorPicker.onsave = function(value) {
        window.location.hash = value;
    };
    window.addEventListener("hashchange", function() {
        colorPicker.restore(window.location.hash);
    });
    colorPicker.restore(window.location.hash);

    var slider = document.getElementById("slider");
    var colorText = document.getElementById("color");
    slider.addEventListener("input", function(e) {
        colorPicker.setColorInpZ(slider.value / slider.max);
    });
    slider.addEventListener("change", function(e) {
        colorPicker.save();
    });
    colorText.addEventListener("blur", function(e) {
        colorPicker.saveTextColor(this.value);
    });
    colorText.addEventListener("keydown", function(e) {
        if (e.key == "Enter") {
            colorPicker.saveTextColor(this.value);
        }
    });
    colorPicker.ondraw = function() {
        slider.value = colorPicker.getColorInpZ() *  slider.max;
        document.getElementById("display").textContent =
            colorPicker.getColorDisplay();
        var color = colorPicker.getTextColor();
        colorText.value = color.hex;
        document.getElementById("preview").style.backgroundColor = color.hex;
        document.getElementById("display-rgb").textContent = color.rgb;
    };

    function setCursor(e) {
        colorPicker.setColor([
            e.offsetX / canvas.clientWidth,
            1.0 - e.offsetY / canvas.clientHeight,
            slider.value / slider.max
        ]);
    }
    var mouseDown = false;
    canvas.addEventListener("mousedown", function(e) {
        if (e.buttons & 1) {
            e.preventDefault();
            mouseDown = true;
            setCursor(e);
        }
    });
    canvas.addEventListener("mousemove", function(e) {
        if (mouseDown) {
            e.preventDefault();
            setCursor(e);
        }
    });
    canvas.addEventListener("mouseup", function(e) {
        e.preventDefault();
        colorPicker.save();
        mouseDown = false;
    });

    var maxChromaInput = document.getElementById("max-chroma");
    colorPicker.setMaxChromaPercent(maxChromaInput.value);
    maxChromaInput.addEventListener("input", function(e) {
        colorPicker.setMaxChromaPercent(this.value);
    });

    var radioViews = {
        "view-lab-hcl": VIEW_LAB_HCL,
        "view-lab-hlc": VIEW_LAB_HLC,
        "view-cam02-hcl": VIEW_CAM02_HCL,
        "view-cam02-hlc": VIEW_CAM02_HLC
    };
    var hasOwnProperty = Object.prototype.hasOwnProperty;
    for (var id in radioViews) {
        if (hasOwnProperty.call(radioViews, id)) {
            var elem = document.getElementById(id);
            if (elem.checked) {
                colorPicker.setView(radioViews[id]);
            }
            elem.addEventListener("change", (function(id) {
                colorPicker.setView(radioViews[id]);
            }).bind(null, id));
        }
    }
}

module.exports = {
    main: main
};
