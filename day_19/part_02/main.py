import sys

# Read the available and needed towels from the input file.
def read_input(filename):
    read_available = False

    available = []
    needed = []

    with open(filename, "r") as f:
        for line in f:
            line = line.strip()

            if len(line) == 0:
                continue

            if not read_available:
                read_available = True
                available.extend(line.split(", "))
            else:
                needed.append(line)

    return available, needed


# Count all the ways of building the given towel.
def ways_of_building(available, towel, memoization):
    if towel in memoization:
        # Immediately return any already computed values.
        return memoization[towel]

    if towel == "":
        # There is only 1 way to "build" an empty towel.
        return 1

    ways = 0
    for av in available:
        if towel.startswith(av):
            # The number of ways of building this towel is the 
            # sum of the ways of building the sub-towels.
            ways += ways_of_building(available, towel.removeprefix(av), memoization)

    # Save the already computed value.
    memoization[towel] = ways
    return ways


def main():
    filename = sys.argv[1]
    available, needed = read_input(filename)

    memoization = {}

    ways = 0
    for towel in needed:
        ways += ways_of_building(available, towel, memoization)

    print(ways)


if __name__ == "__main__":
    main()


