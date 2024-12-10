#include <stdio.h>
#include <stdlib.h>

int* read_nums(char *line, int *length_ptr) {
    // Malloc for list of nums.
    int len = 4;
    int *nums = (int*) malloc(len * sizeof(int));
    if (nums == NULL) {
        printf("Failed to malloc nums.");
        exit(EXIT_FAILURE);
    }

    // Read nums into list.
    char *end_ptr = line;
    int n_nums = 0;

    while (1) {
        int num = strtol(end_ptr, &end_ptr, 10);
        if ((num == 0) && (*end_ptr != '0')) {
            break;
        }

        // Realloc if need more space for nums.
        if (n_nums >= len) {
            len *= 2;
            nums = (int*) realloc(nums, len * sizeof(int));
            if (nums == NULL) {
                printf("Failed to realloc nums. n_nums=%d, len=%d", n_nums, len);
                exit(EXIT_FAILURE);
            }
        }

        nums[n_nums] = num;
        ++n_nums;
    }

    // Set length and return ptr to nums.
    *length_ptr = n_nums;
    return nums;
}

void print_nums(int* nums, int length) {
    for (int i = 0; i < length; ++i) {
        printf("%d ", nums[i]);
    }
    printf("\n");
}

int main() {
    FILE *file_ptr = fopen("data.txt", "r");
    if (file_ptr == NULL) {
        printf("Failed to open file.");
        return 0;
    }

    int safe_reports = 0;

    char line[128];  // Ugly hard-coded.
    while (fgets(line, 128, file_ptr)) {
        int n_nums = 0;
        int *nums = read_nums(line, &n_nums);
        print_nums(nums, n_nums);
        
        int safe = 1;
        int prev_diff = 0;
        for (int i = 1; i < n_nums; ++i) {
            int diff = nums[i] - nums[i - 1];

            // Check diff value.
            if ((diff == 0) || (abs(diff) > 3)) {
                safe = 0;
                break;
            }

            // Check same inc/dec as prev.
            if ((i > 1) && ((prev_diff > 0) != (diff > 0))) {
                safe = 0;
                break;
            }

            prev_diff = diff;
        }
        if (safe == 1) {
            ++safe_reports;
        }

        free(nums);
        printf("\n");
    }

    printf("Safe reports: %d", safe_reports);
}
