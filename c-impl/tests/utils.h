#pragma once

#include <assert.h>
#include <stdbool.h>

#include "../src/byte.h"
#include "../src/utils.h"

#define check_block(res, exp) do { assert(eq_block(res, exp) || verif_vs_res_block(exp, res)); } while (0)
#define check_array(res, exp, i) do {\
    if (!eq_array(res, exp, i)) {\
        printf("Expected:\n");\
        print_array(exp, i);\
        printf("But got:\n");\
        print_array(res, i);\
        assert(eq_array(res, exp, i));\
    }\
} while (0)

void ok();

void copy_block(byte src[4][4], byte dest[4][4]);

__attribute_warn_unused_result__ bool eq_array(byte res[], byte expected[], size_t n);
__attribute_warn_unused_result__ bool eq_block(byte blk1[4][4], byte blk2[4][4]);
__attribute_warn_unused_result__ bool verif_vs_res_block(byte expected[4][4], byte actual[4][4]);