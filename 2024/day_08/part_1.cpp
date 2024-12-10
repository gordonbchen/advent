#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <set>

bool within(int x, int low, int high) {
    return (x >= low) && (x < high);
}

int main(int argc, char* argv[]) {
    // Parse data.
    std::ifstream file(argv[1]);
    if (!file) {
        std::cout << "Failed to open file " << argv[1] << std::endl;
    }

    std::unordered_map<char, std::vector<std::pair<int, int>>> antennas;

    std::string line;
    int i = 0;
    char c;
    while (std::getline(file, line)) {
        for (int j = 0; j < line.length(); ++j) {
            c = line[j];
            if ((c == '.') || (c =='\n')){
                continue;
            }

            if (antennas.find(c) == antennas.end()) {
                std::vector<std::pair<int, int>> coords;
                antennas[c] = coords;
            }
            antennas[c].push_back(std::make_pair(i, j));
        }
        ++i;
    }

    int n_rows = i;
    int n_cols = line.length();
    std::cout << "Shape: " << n_rows << ", " << n_cols << std::endl;

    for (const auto& char_coords_pair : antennas) {
        const auto& c = char_coords_pair.first;
        const auto& coords = char_coords_pair.second;
        std::cout << c << ": ";
        for (const auto& coord : coords) {
            std::cout << "(" << coord.first << ", " << coord.second << ") ";
        }
        std::cout << std::endl;
    }

    // Find antinodes.
    std::set<std::pair<int, int>> antinodes;
    for (const auto& char_coords_pair : antennas) {
        const auto& coords = char_coords_pair.second;
        for (int i = 0; i < coords.size(); ++i) {
            for (int j = i + 1; j < coords.size(); ++j) {
                int dy = coords[i].first - coords[j].first;
                int dx = coords[i].second - coords[j].second;

                std::pair<int, int> antinode1(coords[i].first + dy, coords[i].second + dx);
                std::pair<int, int> antinode2(coords[j].first - dy, coords[j].second - dx);

                if (within(antinode1.first, 0, n_rows) && within(antinode1.second, 0, n_cols)) {
                    antinodes.insert(antinode1);
                }
                if (within(antinode2.first, 0, n_rows) && within(antinode2.second, 0, n_cols)) {
                    antinodes.insert(antinode2);
                }
            }
        }
    }

    std::cout << std::endl << "Antinodes: " << antinodes.size() << std::endl;
}