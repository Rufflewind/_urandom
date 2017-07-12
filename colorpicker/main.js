"use strict";

// TODO add a way to disable the masking
// TODO implement masking for CIECAM02

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

var gamut = ciecam02.gamut(xyz, cam);

function leadingZeros(n, s) {
    s = String(s);
    return Array(n - s.length + 1).join("0") + s;
}

function labToJch(lab) {
    var j = lab[0] * 100.0;
    var c = Math.sqrt(lab[1] * lab[1] + lab[2] * lab[2]) * 100.0;
    var h = Math.atan2(lab[2], lab[1]) * 200.0 / Math.PI;
    if (h < 0) {
        h += 400.0;
    }
    return [j, c, h];
}

function jchToLab(jch) {
    var r = jch[1] / 100.0;
    var t = jch[2] * Math.PI / 200.0;
    return [
        jch[0] / 100.0,
        r * Math.cos(t),
        r * Math.sin(t)
    ];
}

// https://stackoverflow.com/a/5624139
function hexToRgb(hex) {
    var shorthandRegex = /^#?([a-f\d])([a-f\d])([a-f\d])$/i;
    hex = hex.replace(shorthandRegex, function(m, r, g, b) {
        return r + r + g + g + b + b;
    });
    var s = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    if (!s) {
        return null;
    }
    return [parseInt(s[1], 16) / 255.0,
            parseInt(s[2], 16) / 255.0,
            parseInt(s[3], 16) / 255.0];
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

var VIEW_HCL = {
    mask: circularMask,
    display: function(lab) {
        return "(L=" + lab[0].toFixed(2)
             + ", a=" + (lab[1] > 0 ? "+" : "") + lab[1].toFixed(2)
             + ", b=" + (lab[2] > 0 ? "+" : "") + lab[2].toFixed(2) + ")";
    },
    transform: function(inp, maxChroma) {
        return [
            inp[2],
            (2.0 * inp[0] - 1.0) * maxChroma,
            (2.0 * inp[1] - 1.0) * maxChroma
        ];
    },
    untransform: function(lab, maxChroma) {
        return [
            (lab[1] / maxChroma + 1.0) / 2.0,
            (lab[2] / maxChroma + 1.0) / 2.0,
            lab[0]
        ];
    },
    toSrgb: function(lab) {
        return cielab.cielabToSrgb(lab);
    },
    fromSrgb: function(rgb) {
        return cielab.srgbToCielab(rgb);
    }
};

var VIEW_HLC = {
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
             + "\xb0, C=" + cab[0].toFixed(2)
             + ", L=" + (1.0 - r).toFixed(2) + ")";
    },
    transform: function(inp, maxChroma) {
        return [
            maxChroma * inp[2],
            2.0 * inp[0] - 1.0,
            2.0 * inp[1] - 1.0
        ];
    },
    untransform: function(lab, maxChroma) {
        return [
            (lab[1] + 1.0) / 2.0,
            (lab[2] + 1.0) / 2.0,
            lab[0] / maxChroma,
        ];
    },
    toSrgb: function(cab) {
        var x = cab[1];
        var y = cab[2];
        var r = Math.sqrt(x * x + y * y);
        var lab;
        if (r < 1e-8) {
            lab = [1.0, 0.0, 0.0];
        } else {
            lab = [1.0 - r,
                   cab[0] * x / r,
                   cab[0] * y / r];
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
            inGamut: res.inGamut,
        };
    }
};

var VIEW_CAM02_HCL = {
    mask: circularMask,
    display: function(lab) {
        var jch = labToJch(lab);
        return "(J=" + leadingZeros(3, jch[0].toFixed(0))
             + ", C=" + leadingZeros(3, jch[1].toFixed(0))
             + ", h=" + leadingZeros(3, jch[2].toFixed(0)) + ")";
    },
    transform: function(inp, maxChroma) {
        return [
            inp[2],
            (2.0 * inp[0] - 1.0) * maxChroma,
            (2.0 * inp[1] - 1.0) * maxChroma
        ];
    },
    untransform: function(lab, maxChroma) {
        return [
            (lab[1] / maxChroma + 1.0) / 2.0,
            (lab[2] / maxChroma + 1.0) / 2.0,
            lab[0]
        ];
    },
    toSrgb: function(lab) {
        var jch = labToJch(lab);
        return {
            value: xyz.toRgb(cam.toXyz({J: jch[0], C: jch[1], H: jch[2]})),
            inGamut: true
        };
    },
    fromSrgb: function(rgb) {
        var jch = cam.fromXyz(xyz.fromRgb(rgb));
        return {
            value: jchToLab([jch.J, jch.C, jch.h]),
            inGamut: true
        };
    }
};

function cartesian(toSrgb, transform, sliderValue, maxChroma) {
    var inp = [0.0, 0.0, sliderValue];
    for (var y = 0; y < height; ++y) {
        inp[1] = 1.0 - y / height;
        for (var x = 0; x < width; ++x) {
            inp[0] = x / width;
            var r = toSrgb(transform(inp, maxChroma));
            var rgb = r.value;
            if (!r.inGamut) {
                var desat = 0.2;
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
}

var drawScheduled = false;
var drawFull = false;
function redraw(full) {
    drawFull = drawFull || full;
    if (drawScheduled) {
        return;
    }
    drawScheduled = true;
    window.requestAnimationFrame(function() {
        var hex = rgbToHex(currentColorState.get(VIEW_SRGB).value);
        colorText.value = hex;
        document.getElementById("preview").style.backgroundColor = hex;

        var raw = currentColorState.get(view).value;
        var inp = view.untransform(raw, maxChroma);
        var cursorX = inp[0] * width;
        var cursorY = (1.0 - inp[1]) * height;
        slider.value = slider.max * inp[2];
        document.getElementById("display").textContent = view.display(raw);

        if (drawFull) {
            console.time("redraw");
            cartesian(view.toSrgb, view.transform, inp[2], maxChroma);
            console.timeEnd("redraw");
        }

        createImageBitmap(img).then(function(img) {

            ctx.clearRect(0, 0, width, height);
            ctx.save();
            view.mask(ctx, width, height);
            ctx.drawImage(img, 0, 0);
            ctx.restore();

            ctx.strokeStyle = "white";
            ctx.beginPath();
            ctx.arc(cursorX, cursorY, 4, 0, Math.PI * 2);
            ctx.stroke();

            ctx.strokeStyle = "black";
            ctx.beginPath();
            ctx.arc(cursorX, cursorY, 5, 0, Math.PI * 2);
            ctx.stroke();

        });

        drawScheduled = false;
        drawFull = false;
    });
}

function ColorState(view, color) {
    this.set(view, color);
}

ColorState.prototype.set = function(view, color, save) {
    this.view = view;
    this.color = color;
};

ColorState.prototype.save = function() {
    currentHash = rgbToHex(this.get(VIEW_SRGB).value);
    window.location.hash = currentHash;
};

ColorState.prototype.get = function(view) {
    if (this.view == view) {
        return {value: this.color, inGamut: true};
    }
    var r1 = this.view.toSrgb(this.color);
    var r2 = view.fromSrgb(r1.value);
    return {value: r2.value, inGamut: r1.inGamut && r2.inGamut};
};

function setCursor(e) {
    if (!(e.buttons & 1)) {
        return;
    }
    var rx = e.offsetX / canvas.clientWidth;
    var ry = e.offsetY / canvas.clientHeight;
    var sliderValue = slider.value / slider.max;
    var inp = view.transform([rx, 1.0 - ry, sliderValue], maxChroma);
    currentColorState.set(view, inp);
    redraw(false);
}

var canvas = document.getElementById("canvas");
canvas.addEventListener("mousedown", setCursor);
canvas.addEventListener("mousemove", setCursor);
canvas.addEventListener("mouseup", function() {
    currentColorState.save();
});

var currentHash = "";
window.addEventListener("hashchange", function() {
    if (window.location.hash != currentHash) {
        currentColorState.set(VIEW_SRGB, hexToRgb(window.location.hash));
        redraw(true);
    }
});

var hclRadio = document.getElementById("view-hcl");
hclRadio.addEventListener("change", function() {
    view = VIEW_HCL;
    redraw(true);
});
var hlcRadio = document.getElementById("view-hlc");
hlcRadio.addEventListener("change", function() {
    view = VIEW_HLC;
    redraw(true);
});
var cam02HclRadio = document.getElementById("view-cam02-hcl");
cam02HclRadio.addEventListener("change", function() {
    view = VIEW_CAM02_HCL;
    redraw(true);
});

var slider = document.getElementById("slider");
slider.addEventListener("input", function(e) {
    var inp = view.untransform(currentColorState.get(view).value, maxChroma);
    inp[2] = slider.value / slider.max;
    currentColorState.set(view, view.transform(inp, maxChroma));
    redraw(true);
});
slider.addEventListener("change", function(e) {
    currentColorState.save();
});

var maxChromaInput = document.getElementById("max-chroma");
var maxChroma = Number(maxChromaInput.value);
maxChromaInput.addEventListener("input", function(e) {
    maxChroma = Number(maxChromaInput.value);
    redraw(true);
});

function setColor(e) {
    var rgb = hexToRgb(this.value);
    if (rgb == null) {
        return;
    }
    currentColorState.set(VIEW_SRGB, rgb);
    currentColorState.save();
    redraw(true);
}

var colorText = document.getElementById("color");
colorText.addEventListener("blur", setColor);
colorText.addEventListener("keydown", function(e) {
    if (e.key == "Enter") {
        setColor.apply(this, e);
    }
});

var currentColorState = new ColorState(VIEW_SRGB, [0.65, 0.65, 0.65]);
var view = VIEW_HCL;

var ctx = canvas.getContext("2d");
var width = 400;
var height = 400;
var cursorX = width / 2;
var cursorY = height / 2;
canvas.width = width;
canvas.height = height;

var img = ctx.createImageData(width, height);
var data = img.data;
redraw(true);
