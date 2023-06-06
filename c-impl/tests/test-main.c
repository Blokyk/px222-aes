#include <assert.h>
#include <stdio.h>
#include <stdint.h>

#include "../src/byte.h"
#include "../src/cipher.h"

#include "utils.h"

#define check_test(res, exp) assert(eq_block(res, exp) || verif_vs_res_block(exp, res))

#define ok() printf("\x1b[1;32mOK!\x1b[0m\n")

void testUtils() {
    printf("TEST: linear2block... ");

    byte l1[16] = { 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf };
    byte b1[4][4];

    byte v1[4][4] = {
        { 0x0, 0x4, 0x8, 0xc},
        { 0x1, 0x5, 0x9, 0xd},
        { 0x2, 0x6, 0xa, 0xe},
        { 0x3, 0x7, 0xb, 0xf}
    };

    linear_to_column_first_block(l1, b1);
    check_test(b1, v1);

    ok();


    printf("TEST: block2linear... ");

    byte b2[4][4] = {
        { 0x0, 0x4, 0x8, 0xc },
        { 0x1, 0x5, 0x9, 0xd },
        { 0x2, 0x6, 0xa, 0xe },
        { 0x3, 0x7, 0xb, 0xf }
    };

    byte l2[16];

    byte v2[16] = { 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf };

    column_first_block_to_linear(b2, l2);

    for (int i = 0; i < 16; i++) {
        if (l2[i] != v2[i]) {
            printf("%dth element was different! Expected 0x%x, but got 0x%x\n", i, v2[i], l2[i]);
            assert (l2 == v2); // will definitely fail
        }
    }

    ok();
}

// test de SubBytes
void testSubBytes() {
    printf("TEST: SubBytes... ");

    byte t1[4][4] = {
        {0x19, 0xa0, 0x9a, 0xe9},
        {0x3d, 0xf4, 0xc6, 0xf8},
        {0xe3, 0xe2, 0x8d, 0x48},
        {0xbe, 0x2b, 0x2a, 0x08}
    };

    byte v1[4][4] = {
        {0xd4, 0xe0, 0xb8, 0x1e},
        {0x27, 0xbf, 0xb4, 0x41},
        {0x11, 0x98, 0x5d, 0x52},
        {0xae, 0xf1, 0xe5, 0x30}
    };

    SubBytes(t1);
    check_test(t1, v1);

    byte t2[4][4] = {
        {0xa4, 0x68, 0x6b, 0x02},
        {0x9c, 0x9f, 0x5b, 0x6a},
        {0x7f, 0x35, 0xea, 0x50},
        {0xf2, 0x2b, 0x43, 0x49}
    };

    byte v2[4][4] = {
        {0x49, 0x45, 0x7f, 0x77},
        {0xde, 0xdb, 0x39, 0x02},
        {0xd2, 0x96, 0x87, 0x53},
        {0x89, 0xf1, 0x1a, 0x3b}
    };

    SubBytes(t2);
    check_test(t2, v2);

    ok();
}

// test de ShiftRows
void testShiftRows(){
    printf("TEST: ShiftRows... ");

    byte t1[4][4] = {
        {0x00, 0x01, 0x02, 0x03},
        {0x04, 0x05, 0x06, 0x07},
        {0x08, 0x09, 0x0a, 0x0b},
        {0x0c, 0x0d, 0x0e, 0x0f}
    };

    byte v1[4][4] = {
        {0x00, 0x01, 0x02, 0x03},
        {0x05, 0x06, 0x07, 0x04},
        {0x0a, 0x0b, 0x08, 0x09},
        {0x0f, 0x0c, 0x0d, 0x0e}
    };

    ShiftRows(t1);
    check_test(t1, v1);


    byte t2[4][4] = {
        {0xd4, 0x27, 0x11, 0xae},
        {0xe0, 0xbf, 0x98, 0xf1},
        {0xb8, 0xb4, 0x5d, 0xe5},
        {0x1e, 0x41, 0x52, 0x30}
    };

    byte v2[4][4] = {
        {0xd4, 0x27, 0x11, 0xae},
        {0xbf, 0x98, 0xf1, 0xe0},
        {0x5d, 0xe5, 0xb8, 0xb4},
        {0x30, 0x1e, 0x41, 0x52}
    };

    ShiftRows(t2);
    check_test(t2, v2);

    ok();
}

void testMixColumns() {
    printf("TEST: MixColumns... ");

    byte t1[4][4] = {
        {0xd4, 0xe0, 0xb8, 0x1e},
        {0xbf, 0xb4, 0x41, 0x27},
        {0x5d, 0x52, 0x11, 0x98},
        {0x30, 0xae, 0xf1, 0xe5}
    };

    byte v1[4][4] = {
        {0x04, 0xe0, 0x48, 0x28},
        {0x66, 0xcb, 0xf8, 0x06},
        {0x81, 0x19, 0xd3, 0x26},
        {0xe5, 0x9a, 0x7a, 0x4c}
    };

    MixColumns(t1);
    check_test(t1, v1);

    byte t2[4][4] = {
        {0x49, 0x45, 0x7f, 0x77},
        {0xdb, 0x39, 0x02, 0xde},
        {0x87, 0x53, 0xd2, 0x96},
        {0x3b, 0x89, 0xf1, 0x1a}
    };

    byte v2[4][4] = {
        {0x58, 0x1b, 0xdb, 0x1b},
        {0x4d, 0x4b, 0xe7, 0x6b},
        {0xca, 0x5a, 0xca, 0xb0},
        {0xf1, 0xac, 0xa8, 0xe5}
    };

    MixColumns(t2),
    check_test(t2, v2);

    ok();
}

void testAddRoundKey() {
    printf("TEST: AddRoundKey... ");

    byte k1[16] = {
        0x2b, 0x28, 0xab, 0x09,
        0x7e, 0xae, 0xf7, 0xcf,
        0x15, 0xd2, 0x15, 0x4f,
        0x16, 0xa6, 0x88, 0x3c
    };

    byte t1[4][4] = {
        {0x32, 0x88, 0x31, 0xe0},
        {0x43, 0x5a, 0x31, 0x37},
        {0xf6, 0x30, 0x98, 0x07},
        {0xa8, 0x8d, 0xa2, 0x34}
    };

    byte v1[4][4] = {
        {0x19, 0xa0, 0x9a, 0xe9},
        {0x3d, 0xf4, 0xc6, 0xf8},
        {0xe3, 0xe2, 0x8d, 0x48},
        {0xbe, 0x2b, 0x2a, 0x08}
    };

    AddRoundKey(t1, k1);
    check_test(t1, v1);

    byte k2[16] = {
        0xa0, 0x88, 0x23, 0x2a,
        0xfa, 0x54, 0xa3, 0x6c,
        0xfe, 0x2c, 0x39, 0x76,
        0x17, 0xb1, 0x39, 0x05
    };

    byte t2[4][4] = {
        {0x04, 0xe0, 0x48, 0x28},
        {0x66, 0xcb, 0xf8, 0x06},
        {0x81, 0x19, 0xd3, 0x26},
        {0xe5, 0x9a, 0x7a, 0x4c}
    };

    byte v2[4][4] = {
        {0xa4, 0x68, 0x6b, 0x02},
        {0x9c, 0x9f, 0x5b, 0x6a},
        {0x7f, 0x35, 0xea, 0x50},
        {0xf2, 0x2b, 0x43, 0x49}
    };

    AddRoundKey(t2, k2);
    check_test(t2, v2);

    ok();
}

int main (void){
    testUtils();

    testSubBytes();
    testShiftRows();
    testMixColumns();
    testAddRoundKey();

    printf("\x1b[1;32mEvery tests passed!\x1b[0m\n");
}
