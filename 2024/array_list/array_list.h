#ifndef ARRAY_LIST_H
#define ARRAY_LIST_H

typedef struct {
    int* ptr;
    int len;
    int max_len;
} ArrayList;

void init(ArrayList* list);
void print(ArrayList* list);
void append(ArrayList* list, int item);

// Get the idx of the item in the list.
// Returns -1 if the item is not in the list.
int get_idx(ArrayList* list, int item);

void destroy(ArrayList* list);

#endif