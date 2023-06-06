#include <stdio.h>
#include <stdint.h>
#define Nb 4

void Cipher (uint32_t State, uint32_t CipherKey);

void AddRoundKey();

void SubBytes(byte State[4][4]);
void ShiftRows(byte State[4][4]);
void MixColumns(byte State[4][4]);
