def make_set(x_value=None):
    # [rank, parent, value]
    x = [0, None, x_value]
    x[1] = x
    return x

def find(x):
    if x[1] is not x:
        x[1] = find(x[1])
    return x[1]

def union(x, y):
    x_root = find(x)
    y_root = find(y)
    if x_root == y_root:
        return
    if x_root[0] < y_root[0]:
        x_root[1] = y_root
        return
    y_root[1] = x_root
    if x_root[0] == y_root[0]:
        x_root[0] += 1
