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

void testExpandKey24() { }

void testExpandKey32() { }
