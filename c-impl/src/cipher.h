#include <stdio.h>
#include <stdint.h>
#define Nb 4

void Cipher_4 (byte State[4][4], byte Cipher[176]);

void AddRoundKey(byte State [4][4],byte Cipher[16]);
void SubBytes(byte State[4][4]);
void ShiftRows(byte State[4][4]);
void MixColumns(byte State[4][4]);

void Subword(byte Cipher[4][4]);
void Rotword(byte Cipher[4][4]);
