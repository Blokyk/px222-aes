#include <stdio.h>
#include <stdint.h>
#define Nb 4

void Cipher_4 (byte State[4][4], byte Cipher[4][4]);

void AddRoundKey(byte State [4][4],byte Cipher[4][4]);
void SubBytes(byte State[4][4]);
void ShiftRows(byte State[4][4]);
void MixColumns(byte State[4][4]);
