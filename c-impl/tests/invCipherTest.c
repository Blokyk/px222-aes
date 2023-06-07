#include "../src/cipher.h"

#include "invCipherTest.h"
#include "utils.h"

void testInvSubBytes() {
    printf("TEST: InvSubBytes... \t");

    byte t1[4][4] = {
        {0x0, 0x1, 0x2, 0x3},
        {0x4, 0x5, 0x6, 0x7},
        {0x8, 0x9, 0xa, 0xb},
        {0xc, 0xd, 0xe, 0xf}
    };

    byte v1[4][4];
    copy_block(t1, v1);

    SubBytes(t1);
    InvSubBytes(t1);

    check_block(t1, v1);

    ok();
}

void testInvShiftRows() {
    printf("TEST: InvShiftRows... \t");
    byte t1[4][4] = {
        {0x0, 0x1, 0x2, 0x3},
        {0x4, 0x5, 0x6, 0x7},
        {0x8, 0x9, 0xa, 0xb},
        {0xc, 0xd, 0xe, 0xf}
    };

    byte v1[4][4];
    copy_block(t1, v1);

    ShiftRows(t1);
    InvShiftRows(t1);

    check_block(t1, v1);

    ok();
}

void testInvMixColumns() {
    printf("TEST: InvMixColumns... \t");

    byte t1[4][4] = {
        {0x0, 0x1, 0x2, 0x3},
        {0x4, 0x5, 0x6, 0x7},
        {0x8, 0x9, 0xa, 0xb},
        {0xc, 0xd, 0xe, 0xf}
    };

    byte v1[4][4];
    copy_block(t1, v1);

    MixColumns(t1);
    InvMixColumns(t1);

    check_block(t1, v1);

    ok();
}