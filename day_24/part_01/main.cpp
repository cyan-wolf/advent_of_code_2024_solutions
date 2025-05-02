#include <iostream>
#include <sstream>
#include <string>
#include <fstream>
#include <unordered_map>

enum class operation {
    and_,
    or_,
    xor_,
};

struct computation {
    std::string left;
    operation op;
    std::string right;
};

class binary_calculator {
public:
    binary_calculator(
        std::unordered_map<std::string, int> vars,
        std::unordered_map<std::string, computation> comps
    ) {
        variables = vars;
        computations = comps;
    }

    void compute_missing_variables() {
        while (this->computations.size() > 0) {
            // Choose an uncomputed variable.
            auto varname = this->computations.begin()->first;
            this->compute_variable(varname); 
        }
    }

    // Gets the decimal value by checking all the variables that 
    // that with "z".
    // TODO: This isn't working, probably due to integer overlow.
    long long get_decimal_value() {
        long long acc = 0;
        int place = 0;

        bool bin_digits_left = true;
        while (bin_digits_left) {
            std::string varname = "z";
            if (place < 10) {
                varname += "0";
            }
            varname += std::to_string(place);

            if (this->variables.find(varname) == this->variables.end()) {
                bin_digits_left = false;
                break;
            }
            long long bit = this->variables.at(varname);
            acc += bit << place;

            place++;
        }
        return acc;
    }

private:
    // Already computed variables.
    std::unordered_map<std::string, int> variables;

    // Variables that have not been computed yet.
    // This is a map that points the uncomputed variable to 
    // instructions on how to compute it.
    std::unordered_map<std::string, computation> computations;

    // Computes the value of a given variable.
    // Recursively computes the value of sub-variables that this variable depends on.
    void compute_variable(const std::string &varname) {
        auto comp = this->computations.at(varname);

        // Compute left if it was uncomputed.
        if (this->variables.find(comp.left) == this->variables.end()) {
            this->compute_variable(comp.left);
        }
        // Compute right if it was uncomputed.
        if (this->variables.find(comp.right) == this->variables.end()) {
            this->compute_variable(comp.right);
        }
        int left = this->variables.at(comp.left);
        int right = this->variables.at(comp.right);

        int res = compute_value(left, right, comp.op);

        // Remove the variable name from the uncomputed vars map.
        this->computations.erase(varname);

        // Add the variable name to the computed vars map.
        this->variables.insert({varname, res});
    }

    static int compute_value(int left, int right, operation op) {
        if (op == operation::and_) {
            return (left == 1 && left == right) ? 1 : 0;
        }
        else if (op == operation::or_) {
            return (left == 0 && left == right) ? 0 : 1;
        }
        else {
            return (left != right) ? 1 : 0;
        }
    }
};

operation parse_operation(const std::string &op_string) {
    if (op_string == "AND") {
        return operation::and_;
    }
    else if (op_string == "OR") {
        return operation::or_;
    }
    else if (op_string == "XOR") {
        return operation::xor_;
    }
    else {
        throw std::runtime_error("unknown operation string");
    }
}

// Parses a binary calculator from the given filename.
binary_calculator get_input(const std::string &filename) {
    std::ifstream input(filename);

    std::unordered_map<std::string, int> variables;
    std::unordered_map<std::string, computation> computations;

    bool is_parsing_vars = true;

    for (std::string line; std::getline(input, line);) {
        if (line.length() == 0) {
            is_parsing_vars = false;
            continue;
        }
        
        if (is_parsing_vars) {
            int idx = line.find(":");
            auto varname = line.substr(0, idx);
            int value = std::stoi(line.substr(idx + 2, 1));

            variables.insert({varname, value});
        }
        else {
            std::stringstream ss(line);
            std::string leftvar, op_string, rightvar, dummy, resultvar;

            ss >> leftvar;
            ss >> op_string;
            ss >> rightvar;
            ss >> dummy;
            ss >> resultvar;

            operation op = parse_operation(op_string);

            computation comp = {
                leftvar,
                op,
                rightvar,
            };

            computations.insert({resultvar, comp});
        }
    }
    return binary_calculator{variables, computations};
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cout << "A path to the input file must be provided." << std::endl;
        return 1;
    }
    auto filename = argv[1];
    binary_calculator bc = get_input(filename);

    bc.compute_missing_variables();

    int decimal_value = bc.get_decimal_value();

    // TODO: Not working for the main input file (input.txt).
    std::cout << decimal_value << std::endl;
}
