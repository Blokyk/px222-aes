#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include "utils.h"

void print_block(byte blk[4][4]) {
    for (int i = 0; i < 4; i++) {
        printf("| ");

        for (int j = 0; j < 4; j++) {
            printf("%02x ", blk[i][j]);
        }

        printf("|\n");
    }
}