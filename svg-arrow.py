cx = 100
cy = 50
w = 80
h = 40
s = 0.3
g = 0.4

print(
    '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n'
    '<svg xmlns="http://www.w3.org/2000/svg">\n'
    '  <path d="'
    f'M {cx},{cy} '
    f'L {cx - w},{cy - h} '
    f'C {cx - (1 + s) * w},{cy - (1 + s) * h} {cx - w},{cy - g * h} {cx - w},{cy} '
    f'C {cx - w},{cy + g * h} {cx - (1 + s) * w},{cy + (1 + s) * h} {cx - w},{cy + h} '
    'z"/>\n'
    '</svg>'
)
