
# Determines whether a report is safe according to the problem statement.
def report_is_safe(line):
    should_be_increasing = None

    for i in range(len(line) - 1):
        step = abs(line[i + 1] - line[i])

        if step < 1 or step > 3:
            return False
        
        is_increasing = line[i] < line[i + 1]
        
        if should_be_increasing is None:
            should_be_increasing = is_increasing

        elif should_be_increasing != is_increasing:
            return False
        
    return True
        

# Generate all the possible ways of removing 
# a single element from the line.
def gen_line_retries(line):
    for i in range(len(line)):
        yield line[:i] + line[i + 1:]


def main():
    lines = None

    with open("input.txt") as f:
        lines = [[int(s) for s in line.split()] for line in f.readlines()]

    safe_reports = 0

    for line in lines:
        if report_is_safe(line):
            safe_reports += 1

        # If the line wasn't safe, check all the retries.
        else:
            for retry in gen_line_retries(line):
                if report_is_safe(retry):
                    safe_reports += 1
                    break

    print(safe_reports)

if __name__ == "__main__":
    main()
