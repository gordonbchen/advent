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
void destroy(ArrayList* list);

#endif