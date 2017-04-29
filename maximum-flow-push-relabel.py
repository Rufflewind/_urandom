import sys

def push(residual, excess, v, w, amount):
    residual[v][w] -= amount
    residual[w][v] += amount
    excess[v] -= amount
    excess[w] += amount

def push_relabel(residual, source, target, callback=None):
    '''A naive implementation of the push-relabel algorithm to find the
    maximum flow.

    `residual` should contain a row-major matrix of the residual graph edges
    (which initially contains the capacities of each edge).  The second last
    element is the source, and the last element is the target.  No capacity is
    allowed to be infinite.

    `source` and `target` can be either indices or or lists of indices.

    `callback` is a function that is called at every iteration with
    `(residual, excess, height)` positional arguments.  Useful for debugging
    or visualization.

    The function returns the maximum flow and `residual` will contain the
    residual graph of a maximum-flow configuration.'''

    multi = False
    if isinstance(source, list) or isinstance(source, tuple):
        multi = True
        if not (isinstance(target, list) or isinstance(target, tuple)):
            target = tuple(target)
    elif isinstance(target, list) or isinstance(target, tuple):
        multi = True
        source = tuple(source)

    if multi:
        num_vertices = len(residual) + 2
        residual = ([[0] * num_vertices] +
                    [[0] + row + [0] for row in residual] +
                    [[0] * num_vertices])
        for v in source:
            v += 1
            for w in range(num_vertices):
                residual[0][v] += residual[v][w]
        for v in target:
            v += 1
            for w in range(num_vertices):
                residual[v][num_vertices - 1] += residual[w][v]
        return push_relabel(residual, 0, num_vertices - 1, callback=callback)

    num_vertices = len(residual)
    excess = [0] * num_vertices
    height = [num_vertices if v == source else 0
              for v in range(num_vertices)]

    neighbors = [set() for _ in range(num_vertices)]
    for v in range(num_vertices):
        for w in range(num_vertices):
            if residual[v][w]:
                neighbors[v].add(w)
                neighbors[w].add(v)

    for v in neighbors[source]:
        push(residual, excess, source, v, residual[source][v])

    while True:
        if callback:
            callback(residual, excess, height)
        for v in range(num_vertices):
            if v in (source, target):
                continue
            excess_v = excess[v]
            if not excess_v:
                continue
            height_v = height[v]
            for w in neighbors[v]:
                if height_v > height[w] and residual[v][w] > 0:
                    push(residual, excess, v, w, min(excess_v, residual[v][w]))
                    break
            else:
                continue
            break
        else:
            for v in range(num_vertices):
                if v in (source, target):
                    continue
                if excess[v]:
                    height[v] = min(height[w]
                                    for w in neighbors[v]
                                    if residual[v][w] > 0) + 1
                    break
            else:
                return excess[target]

def dump(residual, excess=None, height=None):
    for v in range(len(residual)):
        if excess is not None and height is not None:
            sys.stdout.write("e={}\th={}\t".format(excess[v], height[v], v))
        else:
            sys.stdout.write("\t\t")
        sys.stdout.write("({})\t".format(v))
        first = True
        for w in range(v + 1, len(residual)):
            if residual[v][w] or residual[w][v]:
                if not first:
                    sys.stdout.write("\t\t\t")
                sys.stdout.write("{} ->\t<- {}\t({})\n"
                                 .format(residual[v][w], residual[w][v], w))
                first = False
    sys.stdout.write("\n")
    sys.stdout.flush()
