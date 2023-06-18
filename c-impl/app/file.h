#pragma once

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "../src/byte.h"
#include "../src/cipher.h"

#include "bitmap.h"

#define BUFFER_SIZE 4096
#define MAX_UNBUFFERED_SIZE (UINT32_MAX - 20)

void encrypt_bitmap(const char *srcFilename, const char *dstFilename, enum block_mode mode, const byte key[], short keySize);
void decrypt_bitmap(const char *srcFilename, const char *dstFilename, enum block_mode mode, const byte key[], short keySize);

void encrypt_bitmap_buffered(const char *srcFilename, const char *dstFilename, enum block_mode mode, const byte key[], short keySize);
void decrypt_bitmap_buffered(const char *srcFilename, const char *dstFilename, enum block_mode mode, const byte key[], short keySize);