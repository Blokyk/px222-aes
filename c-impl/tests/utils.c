#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "utils.h"

void ok() {
     printf("\x1b[1;32mOK!\x1b[0m\n");
}

void copy_block(byte src[4][4], byte dest[4][4]) {
    for (int i = 0; i < 4; i++) {
        memcpy(dest[i], src[i], 4);
    }
}

bool eq_array(byte res[], byte expected[], size_t n) {
    for (size_t i = 0; i < n; i++) {
        if (res[i] != expected[i]) {
            printf("%ldth element was different! Expected 0x%x, but got 0x%x\n", i, expected[i], res[i]);
            return false;
        }
    }

    return true;
}

bool eq_block(byte res[4][4], byte expected[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (res[i][j] != expected[i][j]) {
                printf("Element @ (%d, %d) was different! Expected 0x%x, but got 0x%x\n", i+1, j+1, res[i][j], expected[i][j]);
                return false;
            }
        }
    }

    return true;
}

bool verif_vs_res_block(byte expected[4][4], byte actual[4][4]) {
    printf("\n--- \x1b[1;31mERROR\x1b[0m ---\n");
    printf("Expected:\n");
    print_block(expected);
    printf("Actual:\n");
    print_block(actual);
    return false;
}