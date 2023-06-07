#include "keyExpansionTest.h"

#include "../src/byte.h"
#include "../src/cipher.h"

void testExpandKey16() {
    printf("TEST: ExpandKey16... \t");

    byte k1[16] = {
        0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c
    };

    byte o1[KEY16_FULL_SIZE];

    ExpandKey16(k1, o1);

    // for (int i = 0; i < KEY16_FULL_SIZE; i++) {
    //     if (i % 4 == 0)
    //         printf("\n");
    //     if (i % 16 == 0)
    //         printf("--------\n");
    //     printf("%02x", o1[i]);
    // }

    assert(
           o1[KEY16_FULL_SIZE-4] == 0xb6
        && o1[KEY16_FULL_SIZE-3] == 0x63
        && o1[KEY16_FULL_SIZE-2] == 0x0c
        && o1[KEY16_FULL_SIZE-1] == 0xa6
    );

    ok();
}

void testExpandKey24() {
    printf("TEST: ExpandKey24... \t");
    byte k1[24] = {
        0x8e,  0x73, 0xb0, 0xf7,
        0xda,  0x0e, 0x64, 0x52,
        0xc8,  0x10, 0xf3, 0x2b,
        0x80,  0x90, 0x79, 0xe5,
        0x62,  0xf8, 0xea, 0xd2,
        0x52,  0x2c, 0x6b, 0x7b
    };
    byte o1[KEY24_FULL_SIZE];

    ExpandKey24(k1,o1);
    assert(
           o1[KEY24_FULL_SIZE-4] == 0x01
        && o1[KEY24_FULL_SIZE-3] == 0x00
        && o1[KEY24_FULL_SIZE-2] == 0x22
        && o1[KEY24_FULL_SIZE-1] == 0x02
    );

    ok();
}

void testExpandKey32() { }
