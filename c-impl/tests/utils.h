#pragma once

#include <assert.h>
#include <stdbool.h>

#include "../src/byte.h"
#include "../src/utils.h"

#define check_block(res, exp) assert(eq_block(res, exp) || verif_vs_res_block(exp, res))

void ok();

void copy_block(byte src[4][4], byte dest[4][4]);

bool eq_array(byte res[], byte expected[], size_t n);
bool eq_block(byte blk1[4][4], byte blk2[4][4]);
bool verif_vs_res_block(byte expected[4][4], byte actual[4][4]);