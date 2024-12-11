#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <unordered_map>

struct pair_hash {
    std::size_t operator() (const std::pair<long long, int>& p) const {
        auto h1 = std::hash<long long>{}(p.first);
        auto h2 = std::hash<int>{}(p.second);
        return h1 ^ (h2 << 1);
    }
};

long long count(long long value, int blinks, std::unordered_map<std::pair<long long, int>, long long, pair_hash>& cache) {
    std::pair<long long, int> value_blinks(value, blinks);
    if (cache.find(value_blinks) != cache.end()) {
        return cache[value_blinks];
    }

    if (blinks == 0) {
        return 1;
    }

    long long n_stones;
    if (value == 0) {
        value = 1;
        n_stones = count(value, blinks - 1, cache);
    }
    else {
        std::string string_val = std::to_string(value);
        if (string_val.size() % 2 == 0) {
            int half_size = string_val.size() / 2;
            long long left = std::stoll(string_val.substr(0, half_size), nullptr, 10);
            long long right = std::stoll(string_val.substr(half_size), nullptr, 10);

            n_stones = count(left, blinks - 1, cache) + count(right, blinks - 1, cache);
        }
        else {
            value *= 2024;
            n_stones = count(value, blinks - 1, cache);
        }
    }

    cache[value_blinks] = n_stones;
    return n_stones;
}


int main(int argc, char* argv[]) {
    // Parse data.
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cout << "Failed to open file: " << argv[1] << std::endl;
        exit(EXIT_FAILURE);
    }
    
    std::string line;
    std::getline(file, line);

    std::vector<int> nums;

    std::stringstream ss(line);
    int num;
    while (ss >> num) {
        nums.push_back(num);
    }

    for (const auto& i : nums) {
        std::cout << i << " ";
    }
    std::cout << std::endl;

    // Do stuff.
    long long total_count = 0;
    std::unordered_map<std::pair<long long, int>, long long, pair_hash> cache;
    for (const auto& i : nums) {
        total_count += count(i, atoi(argv[2]), cache);
    }
    
    std::cout << "Total count: " << total_count << std::endl;
}
