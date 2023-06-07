#include <assert.h>
#include <stdio.h>
#include <stdint.h>

#include "../src/byte.h"
#include "../src/cipher.h"

#include "utils.h"

#include "keyExpansionTest.h"
#include "cipherTest.h"
#include "invCipherTest.h"
#include "utilsTest.h"

int main (void){
    printf("\n");

    testUtils();

    printf("\n");

    testExpandKey16();
    testExpandKey24();
    testExpandKey32();

    printf("\n");

    testSubBytes();
    testShiftRows();
    testMixColumns();
    testAddRoundKey();

    testCipher();
    testEncryptECB();

    printf("\n");

    testInvSubBytes();
    testInvShiftRows();
    testInvMixColumns();

    testInverseCipher();
    testDecryptECB();

    printf("\n");

    printf("\x1b[1;32mEvery tests passed!\x1b[0m\n");
}
