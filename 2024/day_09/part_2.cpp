#include <iostream>
#include <fstream>
#include <vector>

class Block {
    public:
        int block_size;
        int empty_size;
        int idx;
        long long checksum = 0;

        std::vector<std::pair<int, int>> files;

        void add_file(int file_id, int file_size) {
            for (int i = 0; i < file_size; ++i) {
                checksum += file_id * idx;
                ++idx;
            }
            empty_size -= file_size;

            std::pair<int, int> file(file_id, file_size);
            files.push_back(file);
        }

        void clear() {
            checksum = 0;
            idx -= block_size - empty_size;
            empty_size = block_size;
            files.clear();
        }

        void show_block() const {
            for (const auto& file : files) {
                int file_id = file.first;
                int file_size = file.second;
                for (int i = 0; i < file_size; ++i) {
                    std::cout << file_id;
                }
            }
            for (int i = 0; i < empty_size; ++i) {
                std::cout << ".";
            }
        }
};


int main(int argc, char* argv[]) {
    // Parse data.
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cout << "Failed to open file " << argv[1] << std::endl;
        exit(EXIT_FAILURE);
    }

    std::vector<Block> disk;
    char c;
    int i = 0;
    int idx = 0;
    while (file.get(c)) {
        Block block;
        block.idx = idx;
        int block_size = c - '0';

        if (i % 2 == 0) {
            block.empty_size = 0;
            block.block_size = block_size;
            block.add_file(i / 2, block_size);
        }
        else {
            block.empty_size = block_size;
            block.block_size = block_size;
        }

        disk.push_back(block);
        ++i;
        idx += block_size;
    }

    for (const auto& block : disk) {
        block.show_block();
    }
    std::cout << std::endl;

    // Computations.
    for (int i = disk.size() - 1; i >= 2; i -= 2) {
        Block& file = disk[i];
        for (int j = 1; j < i; j += 2) {
            Block& space = disk[j];
            if (space.empty_size >= file.block_size) {
                int file_id = i / 2;
                space.add_file(file_id, file.block_size);
                file.clear();
                break;
            }
        }
    }

    for (const auto& block : disk) {
        block.show_block();
    }
    std::cout << std::endl;


    long long checksum = 0;
    for (const auto& block : disk) {
        checksum += block.checksum;
    }

    std::cout << std::endl << "Checksum: " << checksum << std::endl;
}
