#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include "../src/cipher.h"
#include "../src/utils.h"

int main(void) {
    srand(time(NULL));

    const int blocks = 10;

    uint8_t *data = malloc(blocks*16);
    uint8_t *dest = malloc(blocks*16);

    uint8_t key[16];

    for(int i = 0; i < blocks*16; i++)
        data[i] = rand();

    for(int i = 0; i < 16; i++)
        key[i] = rand();

    printf("Key:\n");

    uint8_t keyBlock[4][4];
    linear_to_column_first_block(key, keyBlock);
    print_block(keyBlock);

    printf("\nBlocks:\n");

    uint8_t blk[4][4];
    for (int i = 0; i < blocks; i++) {
        linear_to_column_first_block(data + i*16, blk);
        print_block(blk);
        printf("--------------\n");
    }

    encrypt_ecb(data, dest, blocks*16, key, 16);

    printf("\nResult:\n");
    for (int i = 0; i < blocks; i++) {
        linear_to_column_first_block(dest + i*16, blk);
        print_block(blk);
        printf("--------------\n");
    }

    free(data);
    free(dest);
}