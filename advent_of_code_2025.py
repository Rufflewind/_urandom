#!/usr/bin/env python3
# https://adventofcode.com/2025

def day1a(inp, dial=50, cycle=100):
    zeroes = 0
    for line in inp.splitlines():
        line = line.strip()
        rotation = {"L": -1, "R": +1}[line[0]] * int(line[1:])
        dial = (dial + rotation) % cycle
        zeroes += dial == 0
    return zeroes

assert day1a("L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82") == 3

def day1b(inp, dial=50, cycle=100):
    zeroes = 0
    for line in inp.splitlines():
        line = line.strip()
        rotation = {"L": -1, "R": +1}[line[0]] * int(line[1:])
        zeroes += abs((dial + rotation - (rotation <= 0)) // cycle) - (rotation < 0 and dial == 0)
        dial = (dial + rotation) % cycle
    return zeroes

assert day1b("R0") == 0
assert day1b("R49") == 0
assert day1b("R50") == 1
assert day1b("R149") == 1
assert day1b("R150") == 2
assert day1b("L49") == 0
assert day1b("L50") == 1
assert day1b("L149") == 1
assert day1b("L150") == 2
assert day1b("L50\nR100") == 2
assert day1b("L50\nR99") == 1
assert day1b("L50\nR1") == 1
assert day1b("L50\nR0") == 2
assert day1b("L50\nL1") == 1
assert day1b("L50\nL99") == 1
assert day1b("L50\nL100") == 2

def day2a(inp):
    sum_invalid = 0
    for rang in inp.split(","):
        min_id_str, max_id_str = rang.split("-")
        min_id = int(min_id_str)
        max_id = int(max_id_str)
        min_seq_len = -(-len(min_id_str) // 2)
        max_seq_len = len(max_id_str) // 2
        for seq_len in range(min_seq_len, max_seq_len + 1):
            min_seq = 10 ** (seq_len - 1)
            max_seq = 10 ** seq_len - 1
            for seq in range(min_seq, max_seq + 1):
                pid = seq * 10 ** seq_len + seq
                if pid >= min_id and pid <= max_id:
                    sum_invalid += pid
    return sum_invalid

assert  day2a("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124") == 1227775554

def day2b(inp):
    invalids = set()
    for rang in inp.split(","):
        min_id_str, max_id_str = rang.split("-")
        min_id = int(min_id_str)
        max_id = int(max_id_str)
        min_seq_len = 1
        max_seq_len = len(max_id_str) // 2
        for seq_len in range(min_seq_len, max_seq_len + 1):
            min_seq = 10 ** (seq_len - 1)
            max_seq = 10 ** seq_len - 1
            for repeats in range(2, len(max_id_str) // seq_len + 1):
                for seq in range(min_seq, max_seq + 1):
                    pid = seq
                    for _ in range(repeats - 1):
                        pid = pid * 10 ** seq_len + seq
                    if pid >= min_id and pid <= max_id:
                        invalids.add(pid)
    return sum(invalids)

assert day2b("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124") == 4174379265

def day3a(inp):
    joltage = 0
    for bank in inp.splitlines():
        batteries = [(int(j), -i, i) for i, j in enumerate(bank)]
        j1, _, i1 = max(batteries[:-1])
        j2, _, i2 = max(batteries[i1 + 1:])
        joltage += j1 * 10 + j2
    return joltage

assert day3a("987654321111111\n811111111111119\n234234234234278\n818181911112111") == 357

def day3b(inp, num_activated=12):
    total_joltage = 0
    for bank in inp.splitlines():
        batteries = [(int(j), -i, i + 1) for i, j in enumerate(bank)]
        joltage = 0
        i = 0
        for k in range(num_activated):
            j, _, i = max(batteries[i : len(batteries) - num_activated + 1 + k])
            joltage = joltage * 10 + j
        total_joltage += joltage
    return total_joltage

assert day3b("987654321111111\n811111111111119\n234234234234278\n818181911112111", num_activated=2) == 357
assert day3b("987654321111111\n811111111111119\n234234234234278\n818181911112111") == 3121910778619

def day4a(inp):
    rows = inp.splitlines()
    width = len(rows[0].strip())
    height = len(rows)
    accessibles = 0
    for y in range(height):
        for x in range(width):
            if rows[y][x] != "@":
                continue
            neighbors = 0
            for nx in range(max(0, x - 1), min(width, x + 2)):
                for ny in range(max(0, y - 1), min(height, y + 2)):
                    if nx != x or ny != y:
                        neighbors += rows[ny][nx] == "@"
            accessibles += neighbors < 4
    return accessibles

assert day4a("..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.") == 13

def day4b(inp):
    rows = [list(line.strip()) for line in inp.splitlines()]
    width = len(rows[0])
    height = len(rows)
    total_removed = 0
    while True:
        removed = []
        for y in range(height):
            for x in range(width):
                if rows[y][x] != "@":
                    continue
                neighbors = 0
                for nx in range(max(0, x - 1), min(width, x + 2)):
                    for ny in range(max(0, y - 1), min(height, y + 2)):
                        if nx != x or ny != y:
                            neighbors += rows[ny][nx] == "@"
                if neighbors < 4:
                    removed.append((x, y))
        for x, y in removed:
            rows[y][x] = "."
        if len(removed) == 0:
            return total_removed
        total_removed += len(removed)

assert day4b("..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.") == 43

def day5a(inp):
    fresh_lines, query_lines = inp.split("\n\n")
    fresh_ranges = []
    for fresh_line in fresh_lines.splitlines():
        a, b = fresh_line.split("-")
        fresh_ranges.append((int(a), int(b)))
    num_fresh = 0
    for query_line in query_lines.splitlines():
        query = int(query_line)
        is_fresh = False
        for a, b in fresh_ranges:
            if query >= a and query <= b:
                is_fresh = True
                break
        num_fresh += is_fresh
    return num_fresh

assert day5a("3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32") == 3

def day5b(inp):
    fresh_lines, _ = inp.split("\n\n")
    fresh_ranges = []
    for fresh_line in fresh_lines.splitlines():
        a, b = fresh_line.split("-")
        fresh_ranges.append((int(a), int(b)))
    fresh_ranges.sort(reverse=True)
    num_fresh = 0
    while fresh_ranges:
        a, b = fresh_ranges.pop()
        while fresh_ranges:
            if fresh_ranges[-1][0] <= b:
                _, b2 = fresh_ranges.pop()
                b = max(b, b2)
            else:
                break
        num_fresh += b - a + 1
    return num_fresh

assert day5b("3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32") == 14

def day6a(inp):
    rows = [line.split() for line in inp.splitlines()]
    subtotals = [{"+": 0, "*": 1}[op] for op in rows[-1]]
    for row in rows[:-1]:
        for j, op in enumerate(rows[-1]):
            value = int(row[j])
            if op == "+":
                subtotals[j] += value
            elif op == "*":
                subtotals[j] *= value
    return sum(subtotals)

assert day6a("123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ") == 4277556

def day6b(inp):
    import math
    rows = [line.rstrip("\n") for line in inp.splitlines()]
    total = 0
    args = []
    for i, op in reversed(list(enumerate(rows[-1]))):
        value = "".join(row[i] for row in rows[:-1])
        if value.strip() == "":
            continue
        args.append(int(value))
        if op == "+":
            total += sum(args)
            args = []
        elif op == "*":
            total += math.prod(args)
            args = []
    return total

assert day6b("123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ") == 3263827

def day7a(inp):
    rows = [line.strip() for line in inp.splitlines()]
    splits = 0
    beams = {rows[0].index("S")}
    for row in rows[1:]:
        next_beams = set()
        for beam in beams:
            if row[beam] == ".":
                next_beams.add(beam)
            elif row[beam] == "^":
                assert beam > 0 and beam < len(row) - 1
                next_beams.add(beam - 1)
                next_beams.add(beam + 1)
                splits += 1
        beams = next_beams
    return splits

assert day7a(".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............") == 21

def day7b(inp):
    rows = [line.strip() for line in inp.splitlines()]
    beam0 = rows[0].index("S")
    parents = {(-1, beam0): set()}
    beams = {beam0}
    for i, row in enumerate(rows[1:]):
        next_beams = set()
        for beam in beams:
            if row[beam] == ".":
                parents.setdefault((i, beam), set()).add((i - 1, beam))
                next_beams.add(beam)
            elif row[beam] == "^":
                assert beam > 0 and beam < len(row) - 1
                parents.setdefault((i, beam - 1), set()).add((i - 1, beam))
                parents.setdefault((i, beam + 1), set()).add((i - 1, beam))
                next_beams.add(beam - 1)
                next_beams.add(beam + 1)
            else:
                assert False, row[beam]
        beams = next_beams
    totals = {}
    for beam in beams:
        totals[(len(rows) - 2, beam)] = 1
    for node, pars in reversed(parents.items()):
        for p in pars:
            totals[p] = totals.get(p, 0) + totals[node]
    return totals[(-1, beam0)]

assert day7b(".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............") == 40

def day8a(inp, num_conns):
    import math
    positions = [tuple(int(x) for x in line.split(",")) for line in inp.splitlines()]
    dists = []
    for j, q in enumerate(positions):
        for i, p in enumerate(positions[:j]):
            dists.append((sum((px - qx) ** 2 for px, qx in zip(p, q)), i, j))
    dists.sort()
    conns = {}
    for _, i, j in dists[:num_conns]:
        conns.setdefault(i, set()).add(j)
        conns.setdefault(j, set()).add(i)
    circuits = []
    unseen = set(range(len(positions)))
    while unseen:
        stack = [next(iter(unseen))]
        circuit = 0
        while stack:
            i = stack.pop()
            if i not in unseen:
                continue
            unseen.remove(i)
            circuit += 1
            for j in conns.get(i, set()):
                stack.append(j)
        circuits.append(circuit)
    circuits.sort(reverse=True)
    return math.prod(circuits[:3])

assert day8a("162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689", 10) == 40

def day8b(inp):
    import math
    positions = [tuple(int(x) for x in line.split(",")) for line in inp.splitlines()]
    dists = []
    for j, q in enumerate(positions):
        for i, p in enumerate(positions[:j]):
            dists.append((sum((px - qx) ** 2 for px, qx in zip(p, q)), i, j))
    dists.sort()
    sizes = [1] * len(positions)
    parents = list(range(len(positions)))
    for _, i, j in dists:
        for k in [i, j]:
            while parents[parents[k]] != parents[k]:
                parents[k] = parents[parents[k]]
        if parents[i] == parents[j]:
            continue
        assert sizes[parents[i]] + sizes[parents[j]] <= len(positions)
        if sizes[parents[i]] + sizes[parents[j]] == len(positions):
            return positions[i][0] * positions[j][0]
        if sizes[parents[i]] < sizes[parents[j]]:
            j, i = i, j
        sizes[parents[i]] += sizes[parents[j]]
        parents[parents[j]] = parents[i]
    assert False

assert day8b("162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689") == 25272

def day9a(inp):
    import math
    coords = [tuple(int(s) for s in line.split(",")) for line in inp.splitlines()]
    max_area = 0
    for j, q in enumerate(coords):
        for i, p in enumerate(coords[:j]):
            area = abs(math.prod((px - qx + 1) for px, qx in zip(p, q)))
            max_area = max(max_area, area)
    return max_area

assert day9a("7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3") == 50

def day9b(inp):
    def calc_area(vertices):
        # https://en.wikipedia.org/wiki/Shoelace_formula#Trapezoid_formula
        n = len(vertices)
        area = 0
        for i, u in enumerate(vertices):
            v = vertices[(i + 1) % n]
            area += (u[1] + v[1]) * (u[0] - v[0])
        assert area % 2 == 0
        return area // 2

    def div(x, y):
        assert x % y == 0, (x, y)
        return x // y

    def utxk(t, u):
        # calculate UnitVector(u - t) \times (0, 0, 1)
        ut = (u[0] - t[0], u[1] - t[1])
        # taxicab distance for performance reasons
        utm = abs(ut[0]) + abs(ut[1])
        utd = (div(ut[0], utm), div(ut[1], utm))
        return (utd[1], -utd[0])

    coords = [tuple(int(s) for s in line.split(","))
              for line in inp.splitlines()]
    region_area = calc_area(coords)
    # ensure the loop is positively-oriented since we assume the interior is
    # on the right side of the curve as you walk along it (i.e. clockwise when
    # x-axis is rightward and y-axis is downward)
    assert region_area > 0
    max_area = 0
    for j, q in enumerate(coords):
        for i, p in enumerate(coords[:j]):
            a = tuple(min(px, qx) for px, qx in zip(p, q))
            b = tuple(max(px, qx) + 1 for px, qx in zip(p, q))
            clipped = []
            n = len(coords)
            # clip the entire region using the rectangle (a, b) using a
            # simplified algorithm inspired by:
            # https://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm
            for k, u in enumerate(coords):
                t = coords[(k - 1) % n]
                v = coords[(k + 1) % n]
                # adjust for the corner miters
                otu = utxk(t, u)
                ovw = utxk(u, v)
                cu = tuple(max(ax, min(bx, ux + div(1 + otux + ovwx, 2)))
                           for ax, bx, ux, otux, ovwx in zip(a, b, u, otu, ovw))
                clipped.append(cu)
            clipped_area = calc_area(clipped)
            assert clipped_area >= 0
            area = calc_area([a, (b[0], a[1]), b, (a[0], b[1])])
            assert clipped_area <= area
            if area == clipped_area:
                max_area = max(max_area, area)
    return max_area

assert day9b("7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3") == 24


def day10a(inp):
    import itertools, math, re

    def first_one(row):
        for i, x in enumerate(row):
            if x == 1:
                return i
        return len(row)

    total = 0
    for line in inp.splitlines():
        lights_str, wirings_str, _ = re.fullmatch(r"\s*\[([^]]*)\](.*)\{([^}]*)\}\s*", line).groups()
        lights = [int(c == "#") for c in lights_str]
        wirings = []
        for match in re.finditer("\(([^)]*)\)", wirings_str):
            wirings.append([int(s) for s in match.group(1).split(",")])
        # do Gaussian elimination on augmented matrix:
        # wirings X presses = lights
        matrix = [[0] * len(wirings) + [status] for status in lights]
        for button, wiring in enumerate(wirings):
            for light in wiring:
                matrix[light][button] = 1
        matrix.sort(key=first_one)
        m = len(matrix)
        n = len(matrix[0])
        for i in range(m):
            p = first_one(matrix[i])
            if p == n:
                continue
            for j in range(i + 1, m):
                if matrix[j][p] == 1:
                    for k in range(p, n):
                        matrix[j][k] = matrix[j][k] ^ matrix[i][k]
        matrix.sort(key=first_one)
        undetermined = []
        for i in range(m + 1):
            p = first_one(matrix[i][:-1]) if i < m else n - 1
            o = first_one(matrix[i - 1][:-1]) if i >= 0 else -1
            undetermined.extend(range(o + 1, p))
        min_buttons = math.inf
        for xs in itertools.product(*[[0, 1]] * len(undetermined)):
            solution = [0] * n
            solution[-1] = 1
            for v, x in zip(undetermined, xs):
                solution[v] = x
            for i in reversed(range(m)):
                p = first_one(matrix[i])
                for j in range(p + 1, n):
                    solution[p] ^= matrix[i][j] * solution[j]
            solution = solution[:-1]
            min_buttons = min(min_buttons, sum(solution))
        total += min_buttons
    return total

assert day10a("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}") == 7

def day10b(inp):
    import re
    from scipy import optimize          # requires scipy 1.9.0 or higher
    total = 0
    for line in inp.splitlines():
        _, wirings_str, joltages_str = re.fullmatch(r"\s*\[([^]]*)\](.*)\{([^}]*)\}\s*", line).groups()
        wirings = [[int(s) for s in match.group(1).split(",")]
                   for match in re.finditer("\(([^)]*)\)", wirings_str)]
        goal = tuple(int(j) for j in joltages_str.split(","))
        matrix = [[0] * len(wirings) for _ in goal]
        for button, wiring in enumerate(wirings):
            for machine in wiring:
                matrix[machine][button] = 1
        n = len(wirings)
        constraint = optimize.LinearConstraint(matrix, goal, goal)
        result = optimize.milp([1] * n, integrality=[1] * n, constraints=constraint)
        assert abs(round(result.fun) - result.fun) < 1e-6
        min_cost = round(result.fun)
        total += min_cost
    return total

assert day10b("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}") == 33
