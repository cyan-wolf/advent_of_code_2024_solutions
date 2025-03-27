import sys

def read_input(lines):
    reading_rules = True
    rules = {}
    updates = []

    for line in lines:
        line = line.strip()

        if len(line) == 0:
            reading_rules = False
            continue

        if reading_rules:
            before, after = [int(s) for s in line.split("|")]
            
            rules.setdefault(before, [])
            rules[before].append(after)

        else:
            updates.append([int(s) for s in line.split(",")])

    return rules, updates

def try_sort_update(rules, update: list):
    needed_sort = False
    
    idx_curr = 0
    idx_cursor = 0

    while idx_curr < len(update) - 1:
        needed_insert = False
        idx_cursor = idx_curr + 1

        while idx_cursor < len(update):
            curr = update[idx_curr]
            cursor = update[idx_cursor]

            if cursor in rules and curr in rules[cursor]:
                # TODO: ...
                pass
            else:
                idx_cursor += 1
        
        if needed_insert:
            idx_curr += 1
        else:
            needed_sort = True

    return needed_sort

def test_sort(rules, update):
    print(update)
    needed_sort = try_sort_update(rules, update)
    print(needed_sort)
    print(update)

def main():
    filename = sys.argv[1]
    lines = None

    with open(filename, "r") as f:
        lines = f.readlines()
    
    rules, updates = read_input(lines)

    test_sort(rules, updates[0])

    # result = 0

    # for update in updates:
    #     needed_sort = try_sort_update(rules, update)
        
    #     if needed_sort:
    #         # print(update)
    #         # print(update[len(update) // 2])
    #         result += update[len(update) // 2]

    # print(result)


if __name__ == "__main__":
    main()