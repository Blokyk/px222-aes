#include <stdio.h>
#include "cipher.h"

#define Nb 4
#define Nk 4

int CipherKey[4][Nk];
int State[4][Nb];

void Cipher (struct block *s, int CipherKey){
    AddRoundKey();
    for (int i; i=0; Nr-1){
        State = SubBytes();
        State = ShiftRows(State);
        State = MixColumns();
        AddRoundKey();
        printf("State is : \n");
        printf("%x",State); // faire une fonction d'affichage
    }
    State = SubBytes();
    State = ShiftRows();
    AddRoundKey();
    printf("Result is:  \n");
    printf("%x",State); // faire une fonction d'affichage
}

int AddRoundKey(){
}

int SubBytes(){

}


int ShiftRows (int State[4][Nb]){
    for (int i = 0; i<4 ; i++){
        for (int x = 0; x<4 ; x++){
            State[i][(x-1)%4];
        }
    }
}
    
int MixColumns (){

    }
