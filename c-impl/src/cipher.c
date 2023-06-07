#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <byteswap.h>

#include "byte.h"
#include "cipher.h"
#include "lookups.h"
#include "utils.h"

#define make_word(b3, b2, b1, b0) (uint32_t)((b3<<(8*3)) + (b2<<(8*2)) + (b1<<8) + b0)

void Cipher(byte State[4][4], byte Cipher[], int nr) {
    log("Initial state: \n");
    do_debug(print_block(State));

    for (int i = 0; i < KEY16_FULL_SIZE; i++) {
        if (i % 4 == 0)
            log("\n");
        if (i % 16 == 0)
            log("--------\n");
        log("%02x", Cipher[i]);
    }

    AddRoundKey(State, Cipher);
    Cipher += 16; // on offset Cipher par le nombre de byte consommés dans AddRoundKey

    log("After AddRoundKey(i=0): \n");
    do_debug(print_block(State));

    for (int i=0; i < nr - 1; i++){
        SubBytes(State);
        log("[i=%d] after SubBytes: \n", i);
        do_debug(print_block(State));
        ShiftRows(State);
        log("[i=%d] after ShiftRows: \n", i);
        do_debug(print_block(State));
        MixColumns(State);
        log("[i=%d] after MixColumns: \n", i);
        do_debug(print_block(State));
        AddRoundKey(State, Cipher);
        Cipher += 16; // on offset Cipher par le nombre de byte consommés dans AddRoundKey
        log("[i=%d] after AddRoundKey: \n", i);
        do_debug(print_block(State));
    }

    SubBytes(State);
    log("After final SubBytes: \n");
    do_debug(print_block(State));
    ShiftRows(State);
    log("After final ShiftRows: \n");
    do_debug(print_block(State));
    AddRoundKey(State, Cipher);
    log("After final AddRoundKey: \n");
    do_debug(print_block(State));

    log("Result is: \n");
    do_debug(print_block(State));
}

// beware: GCC generates awful code for this AND the version with make_word
//         but Clang doesn't care and generates mostly similar code for either
void AddRoundKey(byte State [4][4], byte Cipher[16]) {
    for (int i = 0; i < 4; i++) {
        State[i][0] ^= Cipher[i];
        State[i][1] ^= Cipher[i+4];
        State[i][2] ^= Cipher[i+8];
        State[i][3] ^= Cipher[i+12];
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

void MixColumns(byte State[4][4]){
    byte Inter[4][4];
    for (int i = 0 ; i<4 ; i++){
        for (int x = 0 ; x<4 ; x++){
            Inter[i][x] = mult2[State[i][x]] ^ mult3[State[(i+1)%4][x]] ^ State[(i+2)%4][x] ^ State[(i+3)%4][x] ;
        }
    }
    for (int i = 0 ; i<4 ; i++){
        for (int x = 0 ; x<4 ; x++){
            State[i][x] = Inter [i][x];
        }
    }
}

void InverseCipher(byte State[4][4], byte Cipher[], int nr) {
    AddRoundKey(State, Cipher);
    Cipher += 16; // offset after use of first key

    log("After AddRoundKey(i=0): \n");
    do_debug(print_block(State));

    for (int i = 0; i < nr - 1; i++) {
        InvShiftRows(State);
        log("[i=%d] after InvShiftRows: \n", i);
        do_debug(print_block(State));
        InvSubBytes(State);
        log("[i=%d] after InvSubBytes: \n", i);
        do_debug(print_block(State));
        AddRoundKey(State, Cipher);
        log("[i=%d] after AddRoundKey: \n", i);
        do_debug(print_block(State));
        Cipher += 16; // on offset Cipher par le nombre de byte consommés dans AddRoundKey
        InvMixColumns(State);
        log("[i=%d] after InvMixColumns: \n", i);
        do_debug(print_block(State));
    }

    InvShiftRows(State);
    log("After final InvShiftRows: \n");
    do_debug(print_block(State));
    InvSubBytes(State);
    log("After final InvSubBytes: \n");
    do_debug(print_block(State));
    AddRoundKey(State, Cipher);
    log("After final AddRoundKey: \n");
    do_debug(print_block(State));

    log("Result is: \n");
    do_debug(print_block(State));
}

void InvSubBytes(byte state[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            state[i][j] = InvSbox[state[i][j]];
        }
    }
}

// cf ShiftRows
void InvShiftRows(byte state[4][4]) {
    for (int i = 1; i < 4; i++) {
        uint32_t asWord = *((uint32_t*)state[i]);
        *((uint32_t*)state[i]) = ROL(asWord, 8*i);
    }
}

void InvMixColumns(byte State[4][4]) {
    byte Inter[4][4];

    for (int i = 0 ; i<4 ; i++){
        for (int x = 0 ; x<4 ; x++){
            Inter[i][x] = multe[State[i][x]] ^ multb[State[(i+1)%4][x]] ^ multd[State[(i+2)%4][x]] ^ mult9[State[(i+3)%4][x]];
        }
    }

    for (int i = 0 ; i<4 ; i++){
        for (int x = 0 ; x<4 ; x++){
            State[i][x] = Inter [i][x];
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

void encrypt (byte State[4][4], byte Key []){
    int length = sizeof(Key);
    int Nr;
    byte Cle;
    if (length == 16) {Nr = 10; byte Cle[KEY16_FULL_SIZE]; ExpandKey16(Key,Cle);}
    else if (length== 24) {Nr = 12;}
    else if (length == 32) {Nr = 14;}
    else return ("Taille de clé non conventionelle ");
    Cipher (State,Cle,Nr);
}

/* void encrypt_ecb(byte State[], byte Key [16]){
    int length = sizeof (State) ;
    byte Res[4][4] ;
    if (length%16 == 0){
        for ( int i=0; i<(length/16); i++){
            byte Inter[16];
            for (int j=0 ; j<16; j++){
                Inter[j] = State[i*16+j];
            }
            linear_to_column_first_block(Inter, Res);
            Cipher(Res, Key);
            print_block(Res);
        }
    }
    else printf("Taille non conventionnelle!") ;
} */