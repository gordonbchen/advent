#include <iostream>
#include <fstream>
#include <vector>

int main(int argc, char* argv[]) {
    // Parse data.
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cout << "Failed to open file " << argv[1] << std::endl;
        exit(EXIT_FAILURE);
    }

    std::vector<int> disk;
    char c;
    while (file.get(c)) {
        disk.push_back(c - '0');
    }

    for (int i : disk) {
        std::cout << i;
    }
    std::cout << std::endl;

    std::cout << "Disk size: " << disk.size() << std::endl;

    // Computations.
    long long checksum = 0;
    int expanded_idx = 0;
    for (int i = 0; i < disk.size(); ++i) {
        int block_size = disk[i];

        if ((i % 2) == 0) {
            int id = i / 2;
            for (int j = 0; j < block_size; ++j) {
                checksum += id * expanded_idx;
                ++expanded_idx;
                // std::cout << id;
            }
        }
        else {
            int j = 0;
            while (j < block_size) {
                int end_idx = disk.size() - 1;
                if ((end_idx % 2) == 1) {
                    disk.pop_back();
                    continue;
                }
                if (disk[end_idx] <= 0) {
                    disk.pop_back();
                    continue;
                }
                if (end_idx <= i) {
                    break;
                }

                int id = end_idx / 2;
                checksum += id * expanded_idx;
                ++expanded_idx;
                // std::cout << id;

                --disk[end_idx];
                ++j;
            }
        }
    }

    std::cout << std::endl << "Checksum: " << checksum << std::endl;
}
