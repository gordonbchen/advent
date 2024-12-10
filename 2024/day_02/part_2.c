#include <stdio.h>
#include <stdlib.h>

// Edge cases: https://www.reddit.com/r/adventofcode/comments/1h4shdu/2024_day_2_part2_edge_case_finder/.

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

int is_safe(int* nums, int n_nums, int skip_ind) {
    int safe = 1;
    int prev_diff = 0;
    int prev_diff_set = 0;
    
    for (int i = 0; i < (n_nums - 1); ++i) {
        if (i == skip_ind) {
            continue;
        }

        int next_ind = i + 1;
        if (next_ind == skip_ind) {
            if (next_ind >= (n_nums - 1)) {
                continue;
            }
            next_ind += 1;
        }

        int diff = nums[next_ind] - nums[i];

        if (
            (diff == 0) ||
            (abs(diff) > 3) ||
            (prev_diff_set && ((prev_diff > 0) != (diff > 0)))
        ) {
            if (skip_ind == -1) {
                int safe_skip_prev = (i > 0) && is_safe(nums, n_nums, i - 1);
                int safe_skip_curr = is_safe(nums, n_nums, i);
                int safe_skip_next = is_safe(nums, n_nums, i + 1);
                return (safe_skip_prev || safe_skip_curr || safe_skip_next); 
            }

            return 0;
        }

        prev_diff = diff;
        prev_diff_set = 1;
    }
    return 1;
}

int main(int argc, char *argv[]) {
    FILE *file_ptr = fopen(argv[1], "r");
    if (file_ptr == NULL) {
        printf("Failed to open file.");
        return 0;
    }

    int safe_reports = 0;

    char line[128];  // Ugly hard-coded.
    while (fgets(line, 128, file_ptr)) {
        int n_nums = 0;
        int *nums = read_nums(line, &n_nums);

        if (is_safe(nums, n_nums, -1)) {
            ++safe_reports;
        }
        else {
            printf("\n");
            print_nums(nums, n_nums);
        }

        free(nums);
    }

    printf("Safe reports: %d", safe_reports);
}
