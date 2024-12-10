#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    FILE *file = fopen(argv[1], "r");
    if (file == NULL) {
        printf("Failed to open file.");
        return 0;
    }

    int x, y;
    char paren;

    int sum = 0;

    while (1) {
        int result = fscanf(file, "mul(%d,%d", &x, &y);
        fscanf(file, "%c", &paren);
        if ((result == 2) && (paren == ')')) {
            printf("%d, %d\n", x, y);
            sum += x * y;
        }
        else if (result == EOF) {
            break;
        }
        else {
            fscanf(file, "%*[^m]");
        }
    }

    printf("Sum: %d", sum);

    fclose(file);
}
