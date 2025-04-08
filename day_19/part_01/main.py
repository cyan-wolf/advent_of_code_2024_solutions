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


# Determines whether the given towel is buildable using the 
# available ones.
def is_buildable(available, towel):
    if towel == "":
        # An empty towel is trivially buildable.
        return True

    for av in available:
        if towel.startswith(av):
            if is_buildable(available, towel.removeprefix(av)):
                # If any of the sub-towels after this point are buildable,
                # then this towel is also buildable.
                return True
        
    # Could not be built.
    return False


def main():
    filename = sys.argv[1]
    available, needed = read_input(filename)

    buildable_amt = 0

    for towel in needed:
        if is_buildable(available, towel):
            buildable_amt += 1

    print(buildable_amt)


if __name__ == "__main__":
    main()
