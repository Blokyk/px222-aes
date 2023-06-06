#include <stdio.h>
#include <stdint.h>
#include "byte.h"
#include "cipher.h"
#include "lookups.h"
#include "utils.h"

#define Nb 4
#define Nk 4
#define Nr 10

void Cipher_4 (byte State[4][4], byte Cipher[176]) { // (Nr+1)*4*4 = 11*4*' = 176
    AddRoundKey(State, Cipher);
    for (int i=0; i<(Nr-1); i++){
        SubBytes(State);
        ShiftRows(State);
        MixColumns(State);
        AddRoundKey(State,Cipher);
        printf("State is : \n");
        print_block(State);
        Cipher = Cipher+16; // on offset Cipher par le nombre de byte consommés dans AddRoundKey
    }
    SubBytes(State);
    ShiftRows(State);
    AddRoundKey(State,Cipher);
    printf("Result is:  \n");
    print_block(State);
}


// note: this gets compiled to a bunch of rol/ror instructions with -O1/-O2 anyway :P
void ShiftRows(byte state[4][4]) {
    // note: i = 1 because first row is unchanged
    for (int i = 1; i < 4; i++) {
        // we need to invert the shift directions because we're using little-endian
        //
        // (e.g., when converting byte[] {00, 01, 02, 03} to a uint32_t, we'll get 0x03020100)

        uint32_t asWord = *((uint32_t*)state[i]);
        uint32_t head = asWord << (8*(4-i));
        uint32_t tail = asWord >> (8*i);
        uint32_t res = head | tail;
        *((uint32_t*)state[i]) = res;
    }
}
void SubBytes(byte state[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            state[i][j] = Sbox[state[i][j]];
        }
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
void AddRoundKey(byte State [4][4],byte Cipher[16]){
    *(uint32_t*)State[0] ^= *((uint32_t*)(Cipher));
    *(uint32_t*)State[1] ^= *((uint32_t*)(Cipher+4));
    *(uint32_t*)State[2] ^= *((uint32_t*)(Cipher+8));
    *(uint32_t*)State[3] ^= *((uint32_t*)(Cipher+12));
}

void Subword(byte Cipher[4][4]){
     for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            Cipher[i][j] = Sbox[Cipher[i][j]];
        }
    }
}
void Rotword(byte Cipher[4][4]){
    byte inter ;
    for (int i=0; i<4 ; i++){
        inter = Cipher[i][0];
        Cipher[i][0] = Cipher[i][1];
        Cipher[i][1] = Cipher[i][2];
        Cipher[i][2] = Cipher[i][3];
        Cipher[i][3] = inter;
    }

}
byte Rcon(){
    int a =(i+1)*2;
    byte rcon[i] = {0xa,0x00,0x00,0x00};
}


void Keyexpansion(byte Cipher[4][4]){
    int i = 0;
    while (i<Nk){
        Subword
        Rotword
        Rcon
    }

}

void encrypt (byte State[4][4], byte Cipher [4][4]){
    Keyexpansion(Cipher[4][4]);
    Cipher_4 (State, Cipher);
}