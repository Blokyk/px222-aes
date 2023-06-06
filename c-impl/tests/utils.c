#pragma once

#include <assert.h>
#include <stdio.h>

#include "utils.h"

bool eq_block(byte blk1[4][4], byte blk2[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (blk1[i][j] != blk2[i][j])
                return false;
        }
    }

    return true;
}

void print_block(byte blk[4][4]) {
    for (int i = 0; i < 4; i++) {
        printf("| ");

        for (int j = 0; j < 4; j++) {
            printf("%02x ", blk[i][j]);
        }

        printf("|\n");
    }
}

bool verif_vs_res_block(byte expected[4][4], byte actual[4][4]) {
    printf("Expected:\n");
    print_block(expected);
    printf("Actual:\n");
    print_block(actual);
    return false;
}