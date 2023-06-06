#include <stdio.h>
#include <stdint.h>
#include "byte.h"
#include "cipher.h"
#include "lookups.h"
#include "utils.h"

#define Nb 4
#define Nk 4

//byte State[4];

/* void Cipher (struct block *s, int CipherKey){
    AddRoundKey();
    for (int i; i=0; Nr-1){
        SubBytes();
        ShiftRows(State);
        MixColumns();
        AddRoundKey();
        printf("State is : \n");
        printf("%x",State); // faire une fonction d'affichage
    }
    SubBytes();
    hiftRows();
    AddRoundKey();
    printf("Result is:  \n");
    printf("%x",State); // faire une fonction d'affichage
}

int AddRoundKey(){
}


*/

// note: this gets compiled to a bunch of rol/ror instructions with -O1/-O2 anyway :P
void ShiftRows2(byte state[4][4]) {
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

void ShiftRows(uint32_t State[4]){
    for (int i = 1; i < 4 ; i++){
            uint32_t firstByte = (State[i]) >> (8*(4-i));
            State[i] = (State[i] << (i*8)) ^ firstByte;
        }
}

void SubBytes2(byte state[4][4]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            state[i][j] = Sbox[state[i][j]];
        }
    }
}

void SubBytes(uint32_t State[4]){
    for (int i= 0; i< 4 ; i++){
        for (int x = 0; x< 4 ; x++){
            uint32_t byte ;
            byte = take_byte(State[i],x); // c'est le byte que je veux changer dans state
            uint32_t sbyte = Sbox[byte];// je dois aller chercher le byte correspondant dans la SBOX
            State[i] = setByte(State[i], x ,sbyte);
        }
    }
}
