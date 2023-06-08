#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include "byte.h"

#ifdef VERBOSE
#define log printf
#define do_debug(a) a
#else
#define log
#define do_debug(a)
#endif

#define ROR(x, n) ((x >> n) | (x << (32 - n)))
#define ROL(x, n) ((x << n) | (x >> (32 - n)))

void print_block(const byte blk[4][4]);
void print_array(const byte a[], size_t n);
void linear_to_column_first_block(const byte line[16], byte blk[4][4]);
void column_first_block_to_linear(const byte blk[4][4], byte line[16]);