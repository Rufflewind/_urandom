def find_blocks(connected, n):
    '''
    connected : (Int, Int) -> Bool
    n : Int
    '''
    colors = [None] * n
    parts  = []
    others = set(range(n))
    group  = set()
    new    = set()
    while others:
        color = len(parts)
        i = others.pop()
        new.add(i)
        group.add(i)
        while new:
            i = new.pop()
            colors[i] = color
            new.update(j for j in others if connected(i, j))
            group.update(new)
            others.difference_update(new)
        parts.append(group)
        group.clear()
    return (colors, parts)
