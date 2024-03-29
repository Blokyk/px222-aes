#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include "utils.h"

void print_block(const byte blk[4][4]) {
    for (int i = 0; i < 4; i++) {
        printf("| ");

        for (int j = 0; j < 4; j++) {
            printf("%02x ", blk[i][j]);
        }

        printf("|\n");
    }
}

void print_array(const byte a[], size_t n) {
    printf("[");
    for (size_t i = 0; i < n; i++) {
        printf("0x%x, ", a[i]);
    }
    printf("]\n");
}

void print_hex_array(const byte a[], size_t n) {
    printf("0x");
    for (size_t i = 0; i < n; i++) {
        printf("%02x", a[i]);
    }
}

// { 0, 1, 2, 3, ... } = { { 0, ... }, { 1, ... }, { 2, ... }, { 3, ... }}
void linear_to_column_first_block(const byte line[16], byte blk[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            blk[j][i] = line[i*4 + j];
        }
    }
}

void column_first_block_to_linear(const byte blk[4][4], byte line[16]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            line[i*4 + j] = blk[j][i];
        }
    }
}