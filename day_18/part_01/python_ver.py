import sys

def main():
    # Get the filename, size of the square grid, and 
    # the cutoff for the number of blocked positions.
    filename = sys.argv[1]
    size, cutoff = map(int, sys.argv[2:])

    # Collect all the blocked positions from the filename.
    blocked_positions = []
    with open(filename, "r") as f:
        blocked_positions.extend([tuple(map(int, line.split(","))) for line in f.readlines()])

    # Mark the blocked positions as "seen" since they are inaccessible anyways.
    seen = set(blocked_positions[:cutoff])

    # Set the start and end positions.
    start_pos = (0, 0)
    end_pos = (size - 1, size - 1)

    # Stack to store positions to process.
    stk = [(start_pos, 0)]

    solution = None

    # Process the positions in the stack.
    for (x, y), dist in stk:
        # Immediately exit the loop if the 
        # end position is reached.
        if (x, y) == end_pos:
            solution = dist
            break
        
        # Check all the neighbors.
        nbrs = [
            (x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)
        ]
        for nx, ny in nbrs:
            in_bounds = 0 <= nx < size and 0 <= ny < size

            if (nx, ny) not in seen and in_bounds:
                # Store the neighbor in the stack for later 
                # processing.
                stk.append(((nx, ny), dist + 1))

                # Mark the neighbor as seen.
                seen.add((nx, ny))

    print(solution)
    
if __name__ == "__main__":
    main()
