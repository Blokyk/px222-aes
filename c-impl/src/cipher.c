#include <stdio.h>
#include "cipher.h"

#define Nb 4
#define Nk 4

int CipherKey[4];
int State[4];

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

int SubBytes(){

}

*/

int ShiftRows (int State[4]){
    for (int i = 0; i<4 ; i++){
           (State[i]<<1)&(7>> State[i]);
    }
}
/*
int MixColumns (){

    } */

/*
KeyExpansion(byte key[4*Nk], word w[Nb*(Nr+1)], Nk)
begin
word  temp
i = 0
while (i < Nk)
w[i] = word(key[4*i], key[4*i+1], key[4*i+2], key[4*i+3])
i = i+1
end while
i = Nk
while (i < Nb * (Nr+1)]
temp = w[i-1]
if (i mod Nk = 0)
temp = SubWord(RotWord(temp)) xor Rcon[i/Nk]
else if (Nk > 6 and i mod Nk = 4)
temp = SubWord(temp)
end if
w[i] = w[i-Nk] xor temp
i = i + 1
end while
end  */