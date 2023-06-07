#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <byteswap.h>

#include "byte.h"
#include "cipher.h"
#include "lookups.h"
#include "utils.h"


void Cipher(byte data[4][4], byte key[], int nr) {
    log("Initial state: \n");
    do_debug(print_block(data));

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

    for (int i=0; i < nr - 1; i++){
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

// beware: GCC generates awful code for this AND the version with make_word
//         but Clang doesn't care and generates mostly similar code for either
void AddRoundKey(byte data[4][4], byte key[16]) {
    for (int i = 0; i < 4; i++) {
        data[i][0] ^= key[i];
        data[i][1] ^= key[i+4];
        data[i][2] ^= key[i+8];
        data[i][3] ^= key[i+12];
    }
}

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

void MixColumns(byte data[4][4]){
    byte tmp[4][4];
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

void InverseKey(byte data[4][4], byte key[], int nr) {
    AddRoundKey(data, key);
    key += 16; // offset after use of first key

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
        key += 16; // on offset key par le nombre de byte consommés dans AddRoundKey
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
    byte tmp[4][4];

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

void SubWord(byte w[4]){
     for (int i = 0; i < 4; i++) {
        w[i] = Sbox[w[i]];
    }
}

void ExpandKey16(byte key[16], byte output[KEY16_FULL_SIZE]) {
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

void ExpandKey24(byte key[24], byte output[KEY24_FULL_SIZE]) {
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

void ExpandKey32(byte key[32], byte output[KEY32_FULL_SIZE]) {
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

int expandKeyAndGetRoundNumber(byte key[], byte fullKey[], size_t keySize) {
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

void encrypt_ecb(byte plaintext[], byte ciphertext[], size_t dataSize, byte key[], size_t keySize) {
    if (dataSize % 16 == 0) {
        byte tmp[4][4];
        byte fullKey[KEY32_FULL_SIZE]; // worst case, doesn't take up too many extra bytes anyway

        int nr = expandKeyAndGetRoundNumber(key, fullKey, keySize);

        for (size_t i = 0; i < dataSize/16; i++) {
            linear_to_column_first_block(plaintext, tmp);
            Cipher(tmp, fullKey, nr);
            memcpy(ciphertext + i*16, tmp, 16);
        }
    }
    else
        printf("Taille non conventionnelle!");
}