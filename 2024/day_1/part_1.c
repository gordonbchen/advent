#include <stdio.h>
#include <stdlib.h>

int compare(const void *a, const void *b) {
    return (*(int*) a - *(int*)b);
}

int main() {
    // Open file for reading.
    FILE *file_ptr = fopen("data.txt", "r");
    if (file_ptr == NULL) {
        printf("File could not be opened.");
        return 0;
    }

    // Allocate memory for left and right nums.
    int len = 4;
    int* left_nums = (int*) malloc(len * sizeof(int));
    int* right_nums = (int*) malloc(len * sizeof(int));
    if ((left_nums == NULL) | (right_nums == NULL)) {
        printf("Failed to allocate memory for left and right nums.");
        return 0;
    }

    int n_nums = 0;
    int left, right;
    while (fscanf(file_ptr, "%d %d", &left, &right) == 2) {
        // Double len and realloc.
        if (n_nums + 1 >= len) {
            len *= 2;
            left_nums = (int*) realloc(left_nums, len * sizeof(int));
            right_nums = (int*) realloc(right_nums, len * sizeof(int));
            if ((left_nums == NULL) | (right_nums == NULL)) {
                printf("Failed to allocate memory for left and right nums.");
                return 0;
            }
            printf("Realloc to len %d\n", len);
        }
        
        // Store left and right nums.
        left_nums[n_nums] = left;
        right_nums[n_nums] = right;
        n_nums++;
    }

    // Sort arrays.
    qsort(left_nums, n_nums, sizeof(int), compare);
    qsort(right_nums, n_nums, sizeof(int), compare);

    // Calculate abs differences.
    int dist = 0;
    for (int i = 0; i < n_nums; i++) {
        printf("%d\t%d\n", left_nums[i], right_nums[i]);
        dist += abs(left_nums[i] - right_nums[i]);
    }

    printf("Total distance: %d", dist);

    // Free left and right nums.
    free(left_nums);
    free(right_nums);

    return 0;
}
