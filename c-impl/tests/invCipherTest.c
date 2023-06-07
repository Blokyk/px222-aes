#include "../src/cipher.h"

#include "invCipherTest.h"
#include "utils.h"

void testDecryptECB() {
    printf("TEST: Decrypt (ECB-128) ");


    byte d1[16] = { 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff};

    byte k1[16] = {
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f
    };

    byte ciphertext[16];
    byte plaintext[16];

    encrypt_ecb(d1, ciphertext, 16, k1, 16);
    decrypt_ecb(ciphertext, plaintext, 16, k1, 16);

    if (!eq_array(plaintext, d1, 16)) {
        printf("Expected:\n");
        print_array(d1, 16);
        printf("But got:\n");
        print_array(plaintext, 16);
        assert(eq_array(plaintext, d1, 16));
    }

    ok();
}

void testInverseCipher() {
    printf("TEST: InverseCipher... \t");

    byte d1[16] = {
        0x32, 0x43, 0xf6, 0xa8,
        0x88, 0x5a, 0x30, 0x8d,
        0x31, 0x31, 0x98, 0xa2,
        0xe0, 0x37, 0x07, 0x34
    };

    byte originalBlock[4][4];
    linear_to_column_first_block(d1, originalBlock);

    byte s1[4][4] = {};
    linear_to_column_first_block(d1, s1);

    byte k1[16] = {
        0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c
    };

    byte fk1[KEY16_FULL_SIZE];

    ExpandKey16(k1, fk1);

    Cipher(s1, fk1, 10);
    InverseCipher(s1, fk1, 10);

    byte tmp1[16];

    check_block(s1, originalBlock);

    ok();
}

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