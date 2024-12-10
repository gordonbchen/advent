#include <stdio.h>
#include <stdlib.h>

char* read_file(char* file_name, int* line_len, int* n_lines) {
    FILE* file = fopen(file_name, "r");
    if (file == NULL) {
        printf("Failed to open file %s.", file_name);
        exit(EXIT_FAILURE);
    }

    int max_len = 64;
    char* text = (char*) malloc(sizeof(char) * max_len);
    if (text == NULL) {
        printf("Failed to malloc %d bytes.\n", max_len);
        exit(EXIT_FAILURE);
    }

    int c;
    int len = 0;
    int line1_done = 0;
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            if (!line1_done) {
                line1_done = 1;
                *line_len = len;
            }
            continue;
        }

        if (len >= max_len) {
            max_len *= 2;
            text = (char*) realloc(text, sizeof(char) * max_len);
            if (text == NULL) {
                printf("Failed to realloc %d bytes.\n", max_len);
                exit(EXIT_FAILURE);
            }
        }
        text[len] = (char) c;
        len += 1;
    }

    *n_lines = len / *line_len;
    printf("len=%d, ", len);

    fclose(file);
    return text;
}

int between(int x, int low, int high) {
    return (x >= low) && (x < high);
}

int get_xmas_count(char* text, int n_lines, int line_len, int i, int j, int i_inc, int j_inc) {
    const char* xmas = "XMAS";

    for (int a = 0; a < 4; ++a) {
        if (a != 0) {
            i += i_inc;
            j += j_inc;
        }
        
        if (!(between(i, 0, n_lines) && between(j, 0, line_len))){
            return 0;
        }

        char letter = text[(i * line_len) + j];
        if (letter != xmas[a]) {
            return 0;
        }
    }
    return 1;
}

int main(int argc, char* argv[]) {
    int line_len, n_lines;
    char* text = read_file(argv[1], &line_len, &n_lines);
    printf("line_len=%d, n_lines=%d\n\n", line_len, n_lines);

    for (int i = 0; i < n_lines; ++i) {
        for (int j = 0; j < line_len; ++j) {
            printf("%c", text[(i * line_len) + j]);
        }
        printf("\n");
    }
    printf("\n");

    int count = 0;
    for (int i = 0; i < n_lines; ++i) {
        for (int j = 0; j < line_len; ++j) {
            
            int x = 0;
            for (int i_inc = -1; i_inc <= 1; ++i_inc) {
                for (int j_inc = -1; j_inc <= 1; ++j_inc) {
                    if ((i_inc == 0) && (j_inc == 0)) {continue;}
                    x += get_xmas_count(text, n_lines, line_len, i, j, i_inc, j_inc);
                }
            }
            count += x;

            if (x >= 1) {
                printf("%c", text[(i * line_len) + j]);
            }
            else {
                printf(".");
            }
        }
        printf("\n");
    }

    printf("\nXMAS count: %d", count);

    free(text);
}
