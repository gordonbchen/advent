#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char* read_text(char* file_name, int* length_ptr) {
    FILE *file = fopen(file_name, "r");
    if (file == NULL) {
        printf("Failed to open file.");
        return 0;
    }

    int max_len = 256;
    int len = 0;
    char* text = (char*) malloc(max_len * sizeof(char));
    if (text == NULL) {
        printf("Failed to malloc for %d chars.\n", max_len);
        return 0;
    }

    char c;
    while ((c = fgetc(file)) != EOF) {
        if (len >= max_len) {
            max_len *= 2;
            text = (char*) realloc(text, max_len * sizeof(char));
            if (text == NULL) {
                printf("Failed to realloc for %d chars.\n", max_len);
                return 0;
            }
        }
        text[len] = c;
        len += 1;
    }

    fclose(file);

    *length_ptr = len; 
    return text;
}

int compare_str(const char* str, int start, char* text, int len) {
    for (int i = 0; i < strlen(str); ++i) {
        if ((i >= len) || (str[i] != text[start + i])) {
            return 0;
        }
    }
    return 1;
}

int read_int(const char* str, int start, int len, int* n_chars_read) {
    int negative = 0;
    if (str[start] == '-') {
        negative = 1;
        start += 1;
    }

    int num = 0;
    for (int i = start; i < len; ++i) {
        if (!isdigit(str[i])) {
            *n_chars_read = i - start;
            break;
        }
        num = (num * 10) + (str[i] - '0');
    }

    if (negative && (*n_chars_read > 0)) {
        num *= -1;
        *n_chars_read += 1;
    }

    return num;
}

int main(int argc, char *argv[]) {
    int len;
    char* text = read_text(argv[1], &len);
    
    if (argv[2][0] == '1') {
        printf("Chars read: %d\n", len);
        for (int i = 0; i < len; ++i) {
            printf("%c", text[i]);
        }
        printf("\n");
    }

    const char* mul_str = "mul(";
    const char* do_str = "do()";
    const char* dont_str = "don't()";

    int n_chars_read = 0;
    int sum = 0;
    int do_mul = 1;

    for (int i = 0; i < len; ++i) {
        if (text[i] == 'd') {
            if (compare_str(do_str, i, text, len)) {
                printf("do\n");
                i += strlen(do_str);
                do_mul = 1;
            }
            if (compare_str(dont_str, i, text, len)) {
                printf("don't\n");
                i += strlen(dont_str);
                do_mul = 0;
            }
        }
        
        if (do_mul && (text[i] == 'm') && compare_str(mul_str, i, text, len)) {
            i += strlen(mul_str);
            int num1 = read_int(text, i, len, &n_chars_read);
            if ((n_chars_read != 0) && (text[i + n_chars_read] == ',')) {
                i += n_chars_read + 1;
                int num2 = read_int(text, i, len, &n_chars_read);
                if ((n_chars_read != 0) && (text[i + n_chars_read] == ')')) {
                    printf("%d %d\n", num1, num2);
                    sum += num1 * num2;
                }
            }
        }
    }

    printf("\nSum: %d", sum);

    free(text);
}
