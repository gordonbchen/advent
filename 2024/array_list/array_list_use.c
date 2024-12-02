#include "array_list.h"

int main() {
    ArrayList list;
    init(&list);
    
    for (int i = 0; i < 64; ++i) { 
        append(&list, i);
    }
    print(&list);

    destroy(&list);
}
