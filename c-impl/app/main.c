#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include "../src/cipher.h"
#include "../src/utils.h"

#include "../tests/utils.h"
#include "file.h"

void testEncryptFile(){
    byte key[16] = {
        0x2b, 0x7e, 0x15, 0X16,
        0x28, 0xae, 0xd2, 0Xa6,
        0xab, 0xf7, 0x15, 0X88,
        0x09, 0xcf, 0x4f, 0X3c
    };
    printf("Encrypting & Decrypting a bitmap : \n");
    modify_bitmap("app/transporteur.bmp","app/image_finale.bmp",key,16,'A');
    modify_bitmap("app/image_finale.bmp","app/image_finale2.bmp",key,16,'B');
}

int main(void) {
    srand(time(NULL));

    const int blocks = 2;

    uint8_t dest[32];
    uint8_t data[32] = {
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,

        0xc6, 0xa1, 0x3b, 0x37,
        0x87, 0x8f, 0x5b, 0x82,
        0x6f, 0x4f, 0x81, 0x62,
        0xa1, 0xc8, 0xd8, 0x79
    };

    uint8_t key[24];

    for(int i = 0; i < 24; i++)
        key[i] = i; // rand();

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

    encrypt_ecb(data, dest, blocks*16, key, 24);

    printf("\nResult:\n");
    for (int i = 0; i < blocks; i++) {
        linear_to_column_first_block(dest + i*16, blk);
        print_block(blk);
        printf("--------------\n");
    }
    testEncryptFile();
}