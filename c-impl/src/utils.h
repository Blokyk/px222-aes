#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include "byte.h"

void print_block(byte blk[4][4]);
void linear_to_column_first_block(byte line[16], byte blk[4][4]);
void column_first_block_to_linear(byte blk[4][4], byte line[16]);