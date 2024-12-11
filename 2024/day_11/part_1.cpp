#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

struct Stone {
    long long value;
    Stone* next;
};

class Stones {
    public:
        int len;
        Stone* head;
        Stone* end;

        Stones() : len(0), head(NULL) {}

        void append(int value) {
            Stone* stone = new Stone;
            stone->value = value;
            stone->next = NULL;

            if (head == NULL) {
                head = stone;
                end = stone;
            }
            else {
                Stone* old_end = end;
                old_end->next = stone;
                end = stone;
            }
            ++len;
        }

        void blink() {
            Stone* curr = head;
            while (curr != NULL) {
                if (curr->value == 0) {
                    curr->value = 1;
                }
                else {
                    std::string string_val = std::to_string(curr->value);
                    if (string_val.size() % 2 == 0) {
                        int half_size = string_val.size() / 2;
                        long long left = std::stoll(string_val.substr(0, half_size), nullptr, 10);
                        long long right = std::stoll(string_val.substr(half_size), nullptr, 10);

                        curr->value = left;
                        Stone* right_stone = new Stone();
                        right_stone->value = right;
                        right_stone->next = curr->next;
                        curr->next = right_stone;

                        ++len;

                        curr = curr->next;
                    }
                    else {
                        curr->value *= 2024;
                    }
                }

                if (curr != NULL) {
                    curr = curr->next;
                }
            }
        }

        void print() {
            Stone* curr = head;
            while (curr != NULL) {
                std::cout << curr->value << " ";
                curr = curr->next;
            }
            std::cout << std::endl;
        }

        ~Stones() {
            Stone* curr = head;
            while (curr != NULL) {
                Stone* temp = curr;
                curr = temp->next;
                delete temp;
            }
        }
};

int main(int argc, char* argv[]) {
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cout << "Failed to open file: " << argv[1] << std::endl;
        exit(EXIT_FAILURE);
    }

    Stones stones;
    
    std::string line;
    std::getline(file, line);

    std::stringstream ss(line);

    int num;
    while (ss >> num) {
        stones.append(num);
    }

    stones.print();
    for (int i = 0; i < 75; ++i) {
        stones.blink();
        // stones.print();
    }
    std::cout << "Len: " << stones.len << std::endl;
}
