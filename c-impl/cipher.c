#include <stdio.h>
#include "cipher.h"

#define Nb 4
#define Nr 10 //12 //14
#define Nk 4 //6  //8
//matrices types [row][columns]


int CipherKey[4][Nk];

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
int State[4][Nb];
void ShiftRows (int State[4][Nb]){

    unsigned char temp;
    // Row 2
    temp = State[2][0];
    State[2][0] = State[2][1];
    State[2][1] = State[2][2];
    State[2][2] = State[2][3];
    State[2][3] = temp;

    // Row 3
    temp = State[3][0];
    State[3][0] = State[3][2];
    State[3][2] = temp;
    temp = State[3][1];
    State[3][1] = State[3][3];
    State[3][3] = temp;

    // Row 4
    temp = State[0][0];
    State[0][0] = State[0][3];
    State[0][3] = State[0][2];
    State[0][2] = State[0][1];
    State[0][1] = temp;
}
   
    int MixColumns (){

    }
