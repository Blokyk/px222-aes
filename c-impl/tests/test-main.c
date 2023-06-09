#include <assert.h>
#include <stdio.h>
#include <stdint.h>

#include "../src/byte.h"
#include "../src/cipher.h"

#include "utils.h"

#include "utilsTest.h"
#include "keyExpansionTest.h"
#include "cipherTest.h"
#include "encryptionTest.h"
#include "invCipherTest.h"
#include "decryptionTest.h"

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

    printf("\n");

    testEncryptECB128();
    testEncryptECB192();
    testEncryptECB256();

    testEncryptCBC128();
    testEncryptCBC192();
    testEncryptCBC256();

    testEncryptUnaligned();

    printf("\n");

    testInvSubBytes();
    testInvShiftRows();
    testInvMixColumns();
    testInverseCipher();

    printf("\n");

    testDecryptECB128();
    testDecryptECB192();
    testDecryptECB256();

    testDecryptCBC128();
    testDecryptCBC192();
    testDecryptCBC256();

    testDecryptUnaligned();

    printf("\n");

    printf("\x1b[1;32mEvery tests passed!\x1b[0m\n");
}
