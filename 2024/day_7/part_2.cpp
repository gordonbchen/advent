#include <unordered_map>
#include <vector>
#include <iostream>
#include <fstream>
#include <regex>
#include <string>
#include <cmath>

long long concat(long long a, long long b) {
    std::string str_a = std::to_string(a);
    std::string str_b = std::to_string(b);
    std::string ab = str_a + str_b;
    return std::stoll(ab);
}

bool possible(const std::vector<long long>& equation, long long right_val, int i) {
    long long left = equation[0];
    if ((right_val == left) && (i == equation.size())) { 
        return true;
    }
    else if (i >= equation.size()) {
        return false;
    }
    else if (right_val > left) {
        return false;
    }

    long long add_val = right_val + equation[i];
    long long mul_val = right_val * equation[i];
    long long concat_val = concat(right_val, equation[i]);
    long long poss_vals[] = {add_val, mul_val, concat_val};

    for (long long poss_val : poss_vals) {
        if (possible(equation, poss_val, i + 1)) {
            return true;
        }
    }
    return false;
}

int main(int argc, char* argv[]) {
    // Parse data.
    std::ifstream file(argv[1]);
    if (!file) {
        std::cerr << "Failed to open file." << std::endl;
        return EXIT_FAILURE;
    }

    std::vector<std::vector<long long>> equations;
    std::regex pattern("\\d+");

    std::string line;
    while (std::getline(file, line)) {
        std::sregex_iterator begin_it(line.begin(), line.end(), pattern);
        std::sregex_iterator end_it;

        std::vector<long long> equation;
        std::smatch match;
        for (std::sregex_iterator i = begin_it; i != end_it; ++i) {
            match = *i;
            equation.push_back(std::stoll(match.str()));
        }

        equations.push_back(equation);
    }

    for (const auto& equation : equations) {
        for (long long i : equation) {
            std::cout << i << " ";
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;

    // Check if equation can be true.
    long long sum = 0;
    for (const auto& equation : equations) {
        if (possible(equation, equation[1], 2)) {
            std::cout << equation[0] << std::endl;
            sum += equation[0];
        }
    }

    std::cout << std::endl << "Sum: " << sum << std::endl;
}