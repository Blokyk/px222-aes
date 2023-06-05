#include <stdio.h>
#define Nb 4
void Cipher (int State, int CipherKey);
int AddRoundKey();
int SubBytes();
void ShiftRows(int State[4][Nb]);
int MixColumns ();
