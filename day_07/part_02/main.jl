
# Read the input file as an array of tuples.
function read_input()
    lines = []

    for line in readlines("../input.txt")
        test_val, nums = split(line, ": ")
        nums = split(nums, " ")

        test_val = parse(Int, test_val)
        nums = map(n -> parse(Int, n), nums)

        push!(lines, (test_val, nums))
    end

    return lines
end

# Generates an array of binary arrays representing 
# all the ways to choose different operators.
function generate_present_lists(num_amt)
    lists = []
    op_amt = num_amt - 1

    for i in range(0, 3^op_amt - 1)
        bin = digits(i, base=3, pad=op_amt)
        push!(lists, bin)
    end
    return lists
end

# Calculates the sum of the valid equation lines.
function sum_of_valid_lines(lines)
    acc = 0

    for line in lines
        # Extract the component of the line.
        test_val, nums = line

        # The amount of numbers in the equation.
        num_amt = length(nums)

        # The different ways of choosing operators.
        present_lists = generate_present_lists(num_amt)
        
        # Since the first number in `line_acc` is always
        # the same, we seperate out the rest of the numbers.
        nums_rest = nums[2:end] # 1-based indexing

        eq_is_valid = false
        for op_to_use in present_lists
            line_acc = nums[1] # 1-based indexing

            for i in range(1, num_amt - 1)
                # The `should_mul` array determines what operator 
                # we choose to perform. 1 means multiplicaiton and 
                # 0 means addition.
                if op_to_use[i] == 1
                    line_acc *= nums_rest[i]
                elseif op_to_use[i] == 2
                    concatenation = string(line_acc) * string(nums_rest[i])
                    line_acc = parse(Int, concatenation)
                else
                    line_acc += nums_rest[i]
                end
            end

            # If the result of the equation was the same as the 
            # test value, then the equation was valid.
            if line_acc == test_val
                eq_is_valid = true
                break
            end
        end

        # Add the value of the equation if it was valid.
        if eq_is_valid
            acc += test_val
        end
    end

    return acc
end

lines = read_input()
println(sum_of_valid_lines(lines))
