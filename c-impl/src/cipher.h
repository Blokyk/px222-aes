#include <stdio.h>
#define Nb 4
void Cipher (int State, int CipherKey);
int AddRoundKey();
int SubBytes();
void ShiftRows(unsigned int State[4]);
int MixColumns ();
