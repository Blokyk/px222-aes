#include <stdio.h>
#include <stdint.h>
#define Nb 4
void Cipher (uint32_t State, uint32_t CipherKey);
void AddRoundKey();
void SubBytes(uint32_t State[4]);
void ShiftRows(uint32_t State[4]);
void MixColumns (uint32_t State[4]);
