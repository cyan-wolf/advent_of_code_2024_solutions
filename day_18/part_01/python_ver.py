import sys

def gen_grid(filename, size, coord_amt):
    coords = []
    with open(filename, "r") as f:
        for line in f.readlines():
            coord = tuple((int(s) for s in line.split(",")))
            coords.append(coord)

    grid = [['.' for _ in range(size)] for _ in range(size)]

    for coord in coords[:coord_amt]:
        (x, y) = coord
        grid[y][x] = '#'

    return grid


def pos_in_bounds(pos, grid):
    (r, c) = pos
    return r >= 0 and r < len(grid) and c >= 0 and c < len(grid[0])


def display_grid(grid, seen_positions):
    for r in range(len(grid)):
        row = [('O' if (r, c) in seen_positions else grid[r][c]) for c in range(len(grid[0]))]
        print("".join(row))


# TODO: Make this use iteration instead of recursion.
def find_exit(curr_pos, end_pos, grid, seen_positions, min_solution_ref):
    seen_positions.add(curr_pos)

    if curr_pos == end_pos:
        solution = len(seen_positions) - 1

        if solution < min_solution_ref["value"]:
            min_solution_ref["value"] = solution
            #display_grid(grid, seen_positions)
            print(solution)

        return

    (curr_r, curr_c) = curr_pos

    nbrs = [
        (curr_r, curr_c + 1),
        (curr_r, curr_c - 1),
        (curr_r + 1, curr_c),
        (curr_r - 1, curr_c),
    ]

    for nbr in nbrs:
        if pos_in_bounds(nbr, grid) and grid[nbr[0]][nbr[1]] != '#' and nbr not in seen_positions:
            find_exit(nbr, end_pos, grid, seen_positions.copy(), min_solution_ref)


def main():
    filename = sys.argv[1]
    size, coord_amt = [int(s) for s in sys.argv[2:]]

    grid = gen_grid(filename, size, coord_amt)

    min_solution_ref = { "value": float('inf') }

    find_exit((0, 0), (size - 1, size - 1), grid, set(), min_solution_ref)

if __name__ == "__main__":
    main()
