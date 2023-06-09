#include <assert.h>
#include <byteswap.h>
#include <errno.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>

#include "byte.h"
#include "cipher.h"
#include "lookups.h"
#include "utils.h"

#define mk_word(b3, b2, b1, b0) ((uint32_t)(b3<<(8*3)) + (uint32_t)(b2<<(8*2)) + (uint32_t)(b1<<8) + (uint32_t)b0)

void Cipher(byte data[4][4], const byte key[], int nr) {
    log("Initial state:\n");
    do_debug(print_block(data));

    log("Encrypting with key:\n");
    for (int i = 0; i < KEY16_FULL_SIZE; i++) {
        if (i % 4 == 0)
            log("\n");
        if (i % 16 == 0)
            log("--------\n");
        log("%02x", key[i]);
    }

    AddRoundKey(data, key);
    key += 16; // on offset key par le nombre de byte consommés dans AddRoundKey

    log("After AddRoundKey(i=0): \n");
    do_debug(print_block(data));

    for (int i=0; i < nr - 1; i++) {
        SubBytes(data);
        log("[i=%d] after SubBytes: \n", i);
        do_debug(print_block(data));

        ShiftRows(data);
        log("[i=%d] after ShiftRows: \n", i);
        do_debug(print_block(data));

        MixColumns(data);
        log("[i=%d] after MixColumns: \n", i);
        do_debug(print_block(data));

        AddRoundKey(data, key);
        key += 16; // on offset key par le nombre de byte consommés dans AddRoundKey
        log("[i=%d] after AddRoundKey: \n", i);
        do_debug(print_block(data));
    }

    SubBytes(data);
    log("After final SubBytes: \n");
    do_debug(print_block(data));

    ShiftRows(data);
    log("After final ShiftRows: \n");
    do_debug(print_block(data));

    AddRoundKey(data, key);
    log("After final AddRoundKey: \n");
    do_debug(print_block(data));

    log("Result is: \n");
    do_debug(print_block(data));
}

// for some reason, GCC generates better code for a "dumb"
// version like this rather one that uses casting or w/ever
void AddRoundKey(byte data[4][4], const byte key[16]) {
    for (int i = 0; i < 4; i++) {
        data[i][0] ^= key[i];
        data[i][1] ^= key[i+4];
        data[i][2] ^= key[i+8];
        data[i][3] ^= key[i+12];
    }
}

//SubBytes transformation is just taking a byte and applicate it to the Sbox; as the Sbox is a linear array, the byte contained in state[i][j] is
//the index in hexadecimal of the substituing byte.
void SubBytes(byte state[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            state[i][j] = Sbox[state[i][j]];
        }
    }
}

// note: this gets compiled to a bunch of rol/ror instructions with -O1/-O2 anyway :P
void ShiftRows(byte state[4][4]) {
    // note: i = 1 because first row is unchanged
    for (int i = 1; i < 4; i++) {
        // we need to invert the shift directions because we're using little-endian
        //
        // (e.g., when converting byte[] {00, 01, 02, 03} to a uint32_t, we'll get 0x03020100)

        uint32_t asWord = *((uint32_t*)state[i]);
        *((uint32_t*)state[i]) = ROR(asWord, 8*i);
    }
}
// we recognized a pattern alternating for mixcolumns : it's always multiply one term by 2, one by 3, and two by 1, then xor everything.
// Needed to make an intermediate variable to help modify the columns, then copy it.
void MixColumns(byte data[4][4]){
    static byte tmp[4][4];
    for (int i = 0 ; i<4 ; i++) {
        for (int x = 0 ; x<4 ; x++) {
            tmp[i][x] = mult2[data[i][x]] ^ mult3[data[(i+1)%4][x]] ^ data[(i+2)%4][x] ^ data[(i+3)%4][x] ;
        }
    }

    // no need to memcpy, cause compiler will just rewrite it to of moves
    for (int i = 0 ; i<4 ; i++) {
        for (int x = 0 ; x<4 ; x++) {
            data[i][x] = tmp[i][x];
        }
    }
}

void InverseCipher(byte data[4][4], const byte key[], int nr) {
    log("Initial state:\n");
    do_debug(print_block(data));

    log("Decrypting with key:\n");
    for (int i = 0; i < KEY16_FULL_SIZE; i++) {
        if (i % 4 == 0)
            log("\n");
        if (i % 16 == 0)
            log("--------\n");
        log("%02x", key[i]);
    }

    int endOfKey = Nb*(nr + 1)*4 - 16; // offset of the last block of the key

    key += endOfKey;

    AddRoundKey(data, key);
    key -= 16; // offset after use of first key

    log("After AddRoundKey(i=0): \n");
    do_debug(print_block(data));

    for (int i = 0; i < nr - 1; i++) {
        InvShiftRows(data);
        log("[i=%d] after InvShiftRows: \n", i);
        do_debug(print_block(data));

        InvSubBytes(data);
        log("[i=%d] after InvSubBytes: \n", i);
        do_debug(print_block(data));

        AddRoundKey(data, key);
        log("[i=%d] after AddRoundKey: \n", i);
        do_debug(print_block(data));
        key -= 16; // on offset key par le nombre de byte consommés dans AddRoundKey

        InvMixColumns(data);
        log("[i=%d] after InvMixColumns: \n", i);
        do_debug(print_block(data));
    }

    InvShiftRows(data);
    log("After final InvShiftRows: \n");
    do_debug(print_block(data));

    InvSubBytes(data);
    log("After final InvSubBytes: \n");
    do_debug(print_block(data));

    AddRoundKey(data, key);
    log("After final AddRoundKey: \n");
    do_debug(print_block(data));

    log("Result is: \n");
    do_debug(print_block(data));
}

// as SubBytes, with the Inverse table
void InvSubBytes(byte data[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            data[i][j] = InvSbox[data[i][j]];
        }
    }
}

// cf ShiftRows
void InvShiftRows(byte data[4][4]) {
    for (int i = 1; i < 4; i++) {
        uint32_t asWord = *((uint32_t*)data[i]);
        *((uint32_t*)data[i]) = ROL(asWord, 8*i);
    }
}

void InvMixColumns(byte data[4][4]) {
    static byte tmp[4][4];

    for (int i = 0 ; i<4 ; i++){
        for (int x = 0 ; x<4 ; x++){
            tmp[i][x] = multe[data[i][x]] ^ multb[data[(i+1)%4][x]] ^ multd[data[(i+2)%4][x]] ^ mult9[data[(i+3)%4][x]];
        }
    }

    // memcpy? cf MixColumns
    for (int i = 0 ; i < 4 ; i++){
        for (int x = 0 ; x < 4; x++){
            data[i][x] = tmp[i][x];
        }
    }
}
// as SubBytes
void SubWord(byte w[4]){
    for (int i = 0; i < 4; i++) {
        w[i] = Sbox[w[i]];
    }
}

// A note on ExpandKeyXX funcs: yes, we could have simply made *one*
// function that took a key size and be done with it, but doing
// micro- and macro-benchmarks revealed that manually "unrolling"
// made them a lot more performant, and had better codegen, since
// the compiler has a lot more knowledge about the situation than
// in the generic case

void ExpandKey16(const byte key[16], byte output[KEY16_FULL_SIZE]) {
    const int nk = 4;

    const uint32_t rcon[] = {
        0x00,
        0x01,
        0x02,
        0x04,
        0x08,
        0x10,
        0x20,
        0x40,
        0x80,
        0x1b,
        0x36
    };

    // rotword(uint32_t w) = w >> 1 (reversed cause endianess)

    memmove(output, key, 16);

    for (int i = nk; i < Nb*(KEY16_NR+1); i++) {
        uint32_t w = *(uint32_t*)(output+4*(i-1));
        // log("w[i-1] = %08x\n", bswap_32(w));

        if (i % nk == 0) {
            w = ROR(w, 8);
            // log("i %% nk == 0 -> %08x", bswap_32(w));
            SubWord((byte*)&w); // a word is just a compact array of bytes after all, right? ;)
            // log(" -> %08x", bswap_32(w));
            w ^= rcon[i/nk];
            // log(" -> (with rcon[i/nk]=%08x) %08x\n", bswap_32(rcon[i/nk]), bswap_32(w));
        }

        // log("w[i-nk] = %08x\n", bswap_32(*(uint32_t*)(output+4*(i-nk))));

        *(uint32_t*)(output+4*i) = *(uint32_t*)(output+4*(i-nk)) ^ w;
    }
}

void ExpandKey24(const byte key[24], byte output[KEY24_FULL_SIZE]) {
    const int nk = 6;
    const uint32_t rcon[] = {
        0x00,
        0x01,
        0x02,
        0x04,
        0x08,
        0x10,
        0x20,
        0x40,
        0x80,
        0x1b,
        0x36
    };

    memmove(output, key, 24);

    for (int i = nk; i < Nb*(KEY24_NR+1); i++) {
        uint32_t w = *(uint32_t*)(output+4*(i-1));

        if (i % nk == 0) {
            w = ROR(w, 8);
            // log("i %% nk == 0 -> %08x", bswap_32(w));
            SubWord((byte*)&w); // a word is just a compact array of bytes after all, right? ;)
            // log(" -> %08x", bswap_32(w));
            w ^= rcon[i/nk];
            // log(" -> (with rcon[i/nk]=%08x) %08x\n", bswap_32(rcon[i/nk]), bswap_32(w));
        }

        // log("w[i-nk] = %08x\n", bswap_32(*(uint32_t*)(output+4*(i-nk))));

        *(uint32_t*)(output+4*i) = *(uint32_t*)(output+4*(i-nk)) ^ w;
    }
}

void ExpandKey32(const byte key[32], byte output[KEY32_FULL_SIZE]) {
    const int nk = 8;
    const uint32_t rcon[] = {
        0x00,
        0x01,
        0x02,
        0x04,
        0x08,
        0x10,
        0x20,
        0x40,
        0x80,
        0x1b,
        0x36
    };

    memmove(output, key, 32);

    for (int i = nk; i < Nb*(KEY32_NR+1); i++) {
        uint32_t w = *(uint32_t*)(output+4*(i-1));

        if (i % nk == 0) {
            w = ROR(w, 8);
            // log("i %% nk == 0 -> %08x", bswap_32(w));
            SubWord((byte*)&w); // a word is just a compact array of bytes after all, right? ;)
            // log(" -> %08x", bswap_32(w));
            w ^= rcon[i/nk];
            // log(" -> (with rcon[i/nk]=%08x) %08x\n", bswap_32(rcon[i/nk]), bswap_32(w));
        }
        else if ( i%nk == 4){
            SubWord((byte*) &w) ;
        }

        // log("w[i-nk] = %08x\n", bswap_32(*(uint32_t*)(output+4*(i-nk))));

        *(uint32_t*)(output+4*i) = *(uint32_t*)(output+4*(i-nk)) ^ w;
    }
}

int expandKeyAndGetRoundNumber(const byte key[], byte fullKey[], short keySize) {
    switch (keySize) {
        case 16:
            ExpandKey16(key, fullKey);
            return 10;
        case 24:
            ExpandKey24(key, fullKey);
            return 12;
        case 32:
            ExpandKey32(key, fullKey);
            return 14;
        default:
            printf("Key must be 16, 24 or 32 bytes long!\n");
            exit(1);
            return -1;
    }
}

void encrypt_ecb(const byte plaintext[], byte ciphertext[], size_t dataSize, const byte key[], short keySize) {
    assert (dataSize % 16 == 0);

    // beware! static variables aren't an automatic speed-up!
    // while they can speed-up some common functions by avoiding
    // useless allocations, they might also impede data locality,
    // thus leading to an overall slow-down because of cache misses
    static byte tmp[4][4];
    static byte fullKey[KEY32_FULL_SIZE]; // worst case, doesn't take up too many extra bytes anyway

    int nr = expandKeyAndGetRoundNumber(key, fullKey, keySize);

    for (size_t i = 0; i < dataSize/16; i++) {
        log("Encrypting block #%02ld", i+1);
        linear_to_column_first_block(plaintext + i*16, tmp);
        Cipher(tmp, fullKey, nr);
        column_first_block_to_linear(tmp, ciphertext + i*16);
    }
}

void decrypt_ecb(const byte ciphertext[], byte plaintext[], size_t dataSize, const byte key[], short keySize) {
    assert(dataSize % 16 == 0);

    static byte tmp[4][4];
    static byte fullKey[KEY32_FULL_SIZE]; // worst case, doesn't take up too many extra bytes anyway

    int nr = expandKeyAndGetRoundNumber(key, fullKey, keySize);

    for (size_t i = 0; i < dataSize/16; i++) {
        log("Decrypting block #%02ld", i+1);
        linear_to_column_first_block(ciphertext + i*16, tmp);
        InverseCipher(tmp, fullKey, nr);
        column_first_block_to_linear(tmp, plaintext + i*16);
    }
}

void encrypt_cbc(const byte plaintext[], byte ciphertext[], size_t dataSize, const byte key[], short keySize) {
    assert(dataSize % 16 == 0);

    const byte emptyBlock[16] = {0};
    static byte tmp[4][4];
    static byte fullKey[KEY32_FULL_SIZE]; // worst case, doesn't take up too many extra bytes anyway

    int nr = expandKeyAndGetRoundNumber(key, fullKey, keySize);

    const byte *lastBlock = emptyBlock;

    for (size_t i = 0; i < dataSize/16; i++) {
        log("Encrypting block #%02ld", i+1);

        linear_to_column_first_block(plaintext + i*16, tmp);

        // we can just reuse AddRoundKey cause it's just an XOR
        AddRoundKey(tmp, lastBlock);

        Cipher(tmp, fullKey, nr);

        column_first_block_to_linear(tmp, ciphertext + i*16);

        lastBlock = ciphertext + i*16;
    }
}

void decrypt_cbc(const byte ciphertext[], byte plaintext[], size_t dataSize, const byte key[], short keySize) {
    assert(dataSize % 16 == 0);

    const byte emptyBlock[16] = {0};
    static byte tmp[4][4];
    static byte fullKey[KEY32_FULL_SIZE]; // worst case, doesn't take up too many extra bytes anyway

    int nr = expandKeyAndGetRoundNumber(key, fullKey, keySize);

    const byte *lastBlock = emptyBlock;

    for (size_t i = 0; i < dataSize/16; i++) {
        log("Decrypting block #%02ld", i+1);

        linear_to_column_first_block(ciphertext + i*16, tmp);

        InverseCipher(tmp, fullKey, nr);

        AddRoundKey(tmp, lastBlock); // equivalent to an XOR

        column_first_block_to_linear(tmp, plaintext + i*16);

        lastBlock = ciphertext + i*16;
    }
}


void encrypt_aligned(
    enum block_mode mode,
    const byte plaintext[],
    byte ciphertext[],
    uint32_t dataSize,
    const byte key[],
    short keySize
) {
    switch (mode) {
        case ECB_MODE:
            encrypt_ecb(plaintext, ciphertext, dataSize, key, keySize);
            return;
        case CBC_MODE:
            encrypt_cbc(plaintext, ciphertext, dataSize, key, keySize);
            return;
        default:
            printf("encrypt() doesn't support block mode %d!\n", mode);
            exit(1);
            break;;
    }
}

void decrypt_aligned(
    enum block_mode mode,
    const byte ciphertext[],
    byte plaintext[],
    uint32_t dataSize,
    const byte key[],
    short keySize
) {
    switch (mode) {
        case ECB_MODE:
            decrypt_ecb(ciphertext, plaintext, dataSize, key, keySize);
            break;
        case CBC_MODE:
            decrypt_cbc(ciphertext, plaintext, dataSize, key, keySize);
            break;
        default:
            printf("decrypt() doesn't support block mode %d!\n", mode);
            exit(1);
            break;;
    }
}

size_t encrypt(
    enum block_mode mode,
    const byte plaintext[],
    byte *(ciphertext[]),
    uint32_t dataSize,
    const byte key[],
    short keySize
) {
    // todo: we don't actually *have* to malloc anything here, i just ran out of time+energy
    //       what we could actually do is only copy the first 12 (=16-4) bytes of data and
    //       prepend the size to that, encrypt it as the first block, and then have another
    //       wrapper/function that takes care of padding for us, this way we avoid any dynamic
    //       allocation in the cipher (which would "escape" to the client and become their
    //       responsibility despite not having requested any dynamic stuff AND having potentially
    //       passed a buffer themselves -- bad API design if you ask me!)
    //       i'm too tired to figure out if/how this would also avoid malloc's for decryption,
    //       but it's definitely a positive change in any case

    short extraBytes = dataSize % 16;

    // So... turns out we have to always add the size and pad anyway? ¯\_(ツ)_/¯
    //
    // if (extraBytes == 0) {
    //     encrypt_aligned(mode, plaintext, ciphertext, dataSize, key, keySize);
    //     return;
    // }

    // if adding the size at the start is going to roll over into another block
    // we need to allocate and pad even more
    short toPad
        = extraBytes + 4 > 16
        ? (16 - extraBytes) + 16 // original + semi-padded + extra
        : 16 - extraBytes;       // original + padded

    log("Data wasn't a multiple of 16 bytes! Need to pad by %d\n", toPad);

    uint32_t padded_size = dataSize + toPad;
    byte *padded_plaintext = malloc(padded_size);

    if (padded_plaintext == NULL) {
        printf("Couldn't allocate buffer of %u bytes for plaintext buffer!\n", padded_size);
        exit(ENOMEM);
        return -1;
    }

    // size
    ((uint32_t*)padded_plaintext)[0] = dataSize;

    // original data
    memcpy(padded_plaintext + 4, plaintext, dataSize);

    // final 0s
    bzero(padded_plaintext + 4 + dataSize, toPad - 4); // compensate the size at the start

    *ciphertext = malloc(padded_size);

    if (*ciphertext == NULL) {
        printf("Couldn't allocate buffer of %u bytes for ciphertext buffer!\n", padded_size);
        exit(ENOMEM);
        return -1;
    }

    encrypt_aligned(mode, padded_plaintext, *ciphertext, padded_size, key, keySize);

    free(padded_plaintext);

    return padded_size;
}

size_t decrypt(
    enum block_mode mode,
    const byte ciphertext[],
    byte *(plaintext[]),
    uint32_t dataSize,
    const byte key[],
    short keySize
) {
    byte *tmp_plaintext = malloc(dataSize);
    log("Alloc'd plaintext @ %p\n", tmp_plaintext);

    decrypt_aligned(mode, ciphertext, tmp_plaintext, dataSize, key, keySize);

    uint32_t size = ((uint32_t*)tmp_plaintext)[0];

    printf("Decrypted to a size of %u (0x%X) bytes\n", size, size);

    assert(size < dataSize);

    byte *old_plaintext = tmp_plaintext;
    tmp_plaintext = malloc(size);
    memcpy(tmp_plaintext, old_plaintext+sizeof(uint32_t), size);
    free(old_plaintext);

    *plaintext = tmp_plaintext;

    return size;
}