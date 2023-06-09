#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "file.h"

#define blocks 3

__attribute_warn_unused_result__
char *getDestinationFilename(const char *srcFilename) {
    size_t filenameLength = strlen(srcFilename);
    if (filenameLength < 5 && strcmp(srcFilename + filenameLength - 4, ".bmp") != 0) {
        printf("File must be a .bmp image!\n");
        exit(1);
    }

    char *destFilename = malloc(filenameLength + 5); // "-enc" + \0
    strncpy(destFilename, srcFilename, filenameLength - 4);
    strncat(destFilename, "-enc.bmp", 5);

    return destFilename;
}

int main(void) {
    srand(time(NULL));
    uint8_t key[32];

    for(int i = 0; i < 32; i++)
        key[i] = rand();

    printf("Encrypting tux-clear.bmp...\n");
    encrypt_bitmap("tux-clear.bmp", "tux-enc.bmp", CBC_MODE, key, 32);
    printf("Done!\n");

    return 0;

    /*
    srand(time(NULL));

    uint8_t *dest;
    uint8_t data[blocks*16] = {
        0x00, 0x11, 0x22, 0x33,
        0x44, 0x55, 0x66, 0x77,
        0x88, 0x99, 0xaa, 0xbb,
        0xcc, 0xdd, 0xee, 0xff,

        0x8e, 0xa2, 0xb7, 0xca,
        0x51, 0x67, 0x45, 0xbf,
        0xea, 0xfc, 0x49, 0x90,
        0x4b, 0x49, 0x60, 0x89,

        0x01, 0x23, 0x45, 0x67,
        0x89, 0xab, 0xcd, 0xef,
        0xfe, 0xdc, 0xba, 0x98,
        0x76,
    };

    uint8_t key[32];

    for(int i = 0; i < 32; i++)
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

    size_t encryptedSize = encrypt(CBC_MODE, data, &dest, 45, key, 32);

    printf("\nResult:\n");
    for (int i = 0; i < blocks; i++) {
        linear_to_column_first_block(dest + i*16, blk);
        print_block(blk);
        printf("--------------\n");
    }

    byte *decrypted;

    size_t decryptedSize = decrypt(CBC_MODE, dest, &decrypted, encryptedSize, key, 32);

    printf("\nDecrypted:\n");
    print_array(decrypted, decryptedSize);
    // for (int i = 0; i < blocks; i++) {
    //     linear_to_column_first_block(decrypted + i*16, blk);
    //     print_block(blk);
    //     printf("--------------\n");
    // }

    free(dest);
    free(decrypted);*/
}