import sys

def read_input(filename):
    blocked_positions = set()

    with open(filename, "r") as f:
        lines = f.readlines()

        size = (len(lines), len(lines[0].strip()))
        
        for r in range(len(lines)):
            for c in range(len(lines[r])):
                if lines[r][c] in {'.', '\n'}:
                    continue

                if lines[r][c] == 'S':
                    start_pos = (r, c)

                elif lines[r][c] == 'E':
                    end_pos = (r, c)

                elif lines[r][c] == '#':
                    blocked_positions.add((r, c))

        return size, start_pos, end_pos, blocked_positions
    

def find_exit(start_pos, end_pos, blocked_positions, size):
    stk = [(start_pos, {})]

    while len(stk) != 0:
        curr_pos, seen_positions = stk.pop()

        seen_positions[curr_pos] = set(seen_positions.copy().keys())

        if curr_pos == end_pos:
            return seen_positions
        
        (r, c) = curr_pos

        nbrs = [
            (r, c + 1),
            (r, c - 1),
            (r + 1, c),
            (r - 1, c)
        ]

        for (nr, nc) in nbrs:
            if (0 <= nr < size[0] 
                and 0 <= nc < size[1] 
                and (nr, nc) not in seen_positions 
                and (nr, nc) not in blocked_positions
            ):
                stk.append(((nr, nc), seen_positions.copy()))

    return None


def find_exit_with_hacks(from_pos, end_pos, already_seen_positions, blocked_positions, size):
    is_hacking = True
    stk = [(from_pos, already_seen_positions)]

    while len(stk) != 0:
        curr_pos, seen_positions = stk.pop()

        seen_positions.add(curr_pos)

        if curr_pos == end_pos:
            return len(seen_positions)
        
        (r, c) = curr_pos

        nbrs = [
            (r, c + 1),
            (r, c - 1),
            (r + 1, c),
            (r - 1, c)
        ]

        for (nr, nc) in nbrs:
            if (0 <= nr < size[0] 
                and 0 <= nc < size[1] 
                and (nr, nc) not in seen_positions 
            ):
                if not is_hacking and (nr, nc) in blocked_positions:
                    continue
                    
                stk.append(((nr, nc), seen_positions.copy()))

        is_hacking = False

    return None


def main():
    filename = sys.argv[1]

    size, start_pos, end_pos, blocked_positions = read_input(filename)

    canon_path = find_exit(start_pos, end_pos, blocked_positions, size)

    normal_time = len(canon_path[end_pos]) - 1

    for seen_pos in canon_path[end_pos]:
        hacked_time = find_exit_with_hacks(seen_pos, end_pos, canon_path[seen_pos], blocked_positions, size)
        
        saved_time = normal_time - hacked_time
        if saved_time > 0:
            print(seen_pos, saved_time)


if __name__ == "__main__":
    main()

