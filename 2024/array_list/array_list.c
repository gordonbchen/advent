#include <stdio.h>
#include <stdlib.h>
#include "array_list.h"

void init(ArrayList* list) {
    list->len = 0;
    list->max_len = 4;

    list->ptr = (int*) malloc(list->max_len * sizeof(int));
    if (list->ptr == NULL) {
        printf("Failed to malloc %d bytes.\n", list->max_len * sizeof(int));
        exit(EXIT_FAILURE);
    }
}

void print(ArrayList* list) {
    for (int i = 0; i < list->len; ++i) {
        printf("%d ", list->ptr[i]);
    }
}

void append(ArrayList* list, int item) {
    if (list->len >= list->max_len) {
        list->max_len *= 2;
        list->ptr = (int*) realloc(list->ptr, list->max_len * sizeof(int));
        if (list->ptr == NULL) {
            printf("Failed to realloc %d bytes.\n", list->max_len * sizeof(int));
            exit(EXIT_FAILURE);
        }
    }

    list->ptr[list->len] = item;
    list->len += 1;
}

void destroy(ArrayList* list) {
    free(list->ptr);
    list->ptr = NULL;
}
