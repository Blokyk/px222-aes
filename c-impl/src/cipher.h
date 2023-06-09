#pragma once

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

enum block_mode { ECB_MODE, CBC_MODE };

void AddRoundKey(byte State [4][4], const byte Cipher[16]);

void Cipher(byte State[4][4], const byte Cipher[], int nr);

void SubBytes(byte State[4][4]);
void ShiftRows(byte State[4][4]);
void MixColumns(byte State[4][4]);

void InverseCipher(byte State[4][4], const byte Ciper[], int nr);

void InvSubBytes(byte State[4][4]);
void InvShiftRows(byte State[4][4]);
void InvMixColumns(byte State[4][4]);

void SubWord(byte Cipher[4]);

void ExpandKey16(const byte key[16], byte fullKey[KEY16_FULL_SIZE]);
void ExpandKey24(const byte key[24], byte output[KEY24_FULL_SIZE]) ;
void ExpandKey32(const byte key[32], byte output[KEY32_FULL_SIZE]) ;

// void encrypt_aligned(enum block_mode mode, const byte plaintext[], byte ciphertext[], uint32_t dataSize, const byte key[], size_t keySize);
// void decrypt_aligned(enum block_mode mode, const byte ciphertext[], byte plaintext[], uint32_t dataSize, const byte key[], size_t keySize);

size_t encrypt(enum block_mode mode, const byte plaintext[], byte *(ciphertext[]), uint32_t dataSize, const byte key[], short keySize);
size_t decrypt(enum block_mode mode, const byte ciphertext[], byte *(plaintext[]), uint32_t dataSize, const byte key[], short keySize);

void encrypt_ecb(const byte plaintext[], byte ciphertext[], size_t dataSize, const byte key[], short keySize);
void decrypt_ecb(const byte ciphertext[], byte plaintext[], size_t dataSize, const byte key[], short keySize);

void encrypt_cbc(const byte plaintext[], byte ciphertext[], size_t dataSize, const byte key[], short keySize);
void decrypt_cbc(const byte ciphertext[], byte plaintext[], size_t dataSize, const byte key[], short keySize);
