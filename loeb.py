def memoize(f):
    result = []
    def wrapper(cells):
        if not len(result):
            result.append(f(cells))
        return result[0]
    return wrapper

def calc_spreadsheet(formulas):
    cells = [lambda formula=formula: memoize(formula)(cells)
             for formula in formulas]
    # force evaluation of every cell
    return [cell() for cell in cells]

# spreadsheet example
# A1:       2
# A2:       3
# A3:   =1/A4
# A4:  =A1+A2
print(calc_spreadsheet([
    lambda cells: 2.0,
    lambda cells: 3.0,
    lambda cells: 1.0 / cells[3](),
    lambda cells: cells[0]() + cells[1](),
]))
