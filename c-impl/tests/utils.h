#pragma once

#include <stdbool.h>

#include "../src/byte.h"

bool eq_block(byte blk1[4][4], byte blk2[4][4]);
void print_block(byte blk[4][4]);
bool verif_vs_res_block(byte expected[4][4], byte actual[4][4]);