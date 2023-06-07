#include <stdio.h>
#include <stdint.h>

#include "byte.h"

#define Nb 4

#define KEY16_NR 10
#define KEY24_NR 12
#define KEY32_NR 14

#define KEY16_FULL_SIZE Nb*(KEY16_NR + 1)*4
#define KEY24_FULL_SIZE Nb*(KEY24_NR + 1)*4
#define KEY32_FULL_SIZE Nb*(KEY32_NR + 1)*4

void AddRoundKey(byte State [4][4],byte Cipher[16]);

void Cipher(byte State[4][4], byte Cipher[], int nr);

void SubBytes(byte State[4][4]);
void ShiftRows(byte State[4][4]);
void MixColumns(byte State[4][4]);

void InverseCipher(byte State[4][4], byte Ciper[], int nr);

void InvSubBytes(byte State[4][4]);
void InvShiftRows(byte State[4][4]);
void InvMixColumns(byte State[4][4]);

void SubWord(byte Cipher[4]);

void ExpandKey16(byte key[16], byte fullKey[KEY16_FULL_SIZE]);
void ExpandKey24(byte key[24], byte output[KEY24_FULL_SIZE]) ;
void ExpandKey32(byte key[32], byte output[KEY32_FULL_SIZE]) ;

void encrypt (byte data[4][4], byte Key[], size_t keySize);
void encrypt_ecb(byte data[], size_t dataSize, byte Key[], size_t keySize);