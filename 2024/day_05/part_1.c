#include <stdio.h>
#include <stdlib.h>
#include "array_list.h"

int main(int argc, char* argv[]) {
    FILE* file = fopen(argv[1], "r");
    if (file == NULL) {
        printf("Failed to open file %s.\n", argv[1]);
        return 0;
    }
    
    const int max_page_num = 100;  // Ugly hard code or using my eyes?
    ArrayList pages_after[max_page_num];
    for (int i = 0; i < max_page_num; ++i) {
        init(&pages_after[i]);
    }

    int page, page_after;
    while (fscanf(file, "%d|%d", &page, &page_after) == 2) {
        if (page >= max_page_num) {
            printf("page %d > max_page_num %d", page, max_page_num);
            return 0;
        }

        append(&pages_after[page], page_after);
        printf("%d|%d\n", page, page_after);
    }
    printf("\n");

    for (int i = 0; i < max_page_num; ++i) {
        if (pages_after[i].len > 0) {
            printf("%d: ", i);
            print(&pages_after[i]);
        }
    }
    printf("\n");

    // HACK: First page number is read into page. Yes this sucks.
    // So read 1 char more, (comma).
    fgetc(file);

    // Read pages.
    int fix = 0;
    int cont = 1;
    int sum = 0;
    while (cont) {
        
        ArrayList pages;
        init(&pages);

        if (!fix) {
            append(&pages, page);
            fix = 1;
        }

        int num;
        int next_char;
        while (1) {
            fscanf(file, "%d", &num);
            append(&pages, num);

            next_char = fgetc(file);
            if (next_char == 10) { // '\n'. 
                break;
            }
            else if (next_char == -1) {
                cont = 0;
                break;
            }
        }
        print(&pages);

        // Check pages.
        int good_order = 1;
        for (int i = 1; i < pages.len; ++i) {
            if (!good_order) {break;}
            for (int j = 0; j < i; ++j) {
                if (get_idx(&pages_after[pages.ptr[i]], pages.ptr[j]) != -1) {
                    good_order = 0;
                    break;
                }
            }
        }

        if (good_order) {
            int middle = pages.ptr[pages.len / 2];
            printf("middle=%d\n\n", middle);
            sum += middle;
        }

        destroy(&pages);
    }

    printf("Sum: %d", sum);

    fclose(file);
    for (int i = 0; i < max_page_num; ++i) {
        destroy(&pages_after[i]);
    }
}