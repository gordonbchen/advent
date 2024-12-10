#include <iostream>
#include <fstream>
#include <vector>
#include <set>

bool between(int x, int low, int high) {
    return (x >= low) && (x < high);
}

int get_value(std::pair<int, int>& coord, std::vector<std::vector<int>>& nums) {
    return nums[coord.first][coord.second];
}

bool is_valid(std::pair<int, int>& coord, int n_rows, int n_cols) {
    return between(coord.first, 0, n_rows) && between(coord.second, 0, n_cols);
}

std::vector<std::pair<int, int>> get_neighbors(std::pair<int, int>& coord, std::vector<std::vector<int>>& nums) {
    std::vector<std::pair<int, int>> neighbors;
    for (int i = -1; i <= 1; ++i) {
        for (int j = -1; j <= 1; ++j) {
            if ((i == 0) == (j == 0)) {
                continue;
            }

            std::pair<int, int> neighbor(coord.first + i, coord.second + j);
            if (
                is_valid(neighbor, nums.size(), nums[0].size()) &&
                (get_value(neighbor, nums) - get_value(coord, nums) == 1)
            ) {
                neighbors.push_back(neighbor);
            }
        }
    }

    return neighbors;
}

int get_trailhead_score(std::vector<std::vector<int>>& nums, int i, int j) {
    int n_trails = 0;

    std::vector<std::pair<int, int>> to_visit;
    to_visit.push_back(std::pair<int, int>(i, j));

    while (to_visit.size() >= 1) {
        std::pair<int, int> coord = to_visit.back();
        to_visit.pop_back();

        if (get_value(coord, nums) == 9) {
            n_trails += 1;
            continue;
        }

        std::vector<std::pair<int, int>> neighbors = get_neighbors(coord, nums);
        for (const auto& neighbor : neighbors) {
            to_visit.push_back(neighbor);
        }
    }

    return n_trails;
}

int main(int argc, char* argv[]) {
    // Parse data.
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cout << "Failed to open file " << argv[1] << std::endl;
        exit(EXIT_SUCCESS);
    }

    std::vector<std::vector<int>> nums;
    char c;
    std::vector<int> row;
    while (true) {
        if (!file.get(c)) {
            nums.push_back(row);
            break;            
        }
        else if (c == '\n') {
            nums.push_back(row);
            row.clear();
        }
        else {
            row.push_back(c - '0');
        }
    }

    for (const auto& row : nums) {
        for (int i : row) {
            std::cout << i;
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;

    std::cout << "Shape: " << nums.size() << ", " << nums[0].size() << std::endl;

    // Computations.
    int total_score = 0;
    for (int i = 0; i < nums.size(); ++i) {
        for (int j = 0; j < nums[0].size(); ++j) {
            if (nums[i][j] == 0) {
                int score = get_trailhead_score(nums, i, j);
                total_score += score;
                std::cout << score;
            }
            else {
                std::cout << ".";
            }
        }
        std::cout << std::endl;
    }
    std::cout << std::endl << "Total score: " << total_score << std::endl;
}
