#include <assert.h>
#include <stdio.h>
#include <stdint.h>

#include "../src/byte.h"
#include "../src/cipher.h"

#include "utils.h"

// test de ShiftRows
void testShiftRows(byte tab[4][4], byte verif[4][4]){
    ShiftRows(tab);
    assert(eq_block (tab,verif) || verif_vs_res_block(verif, tab));
}

// test de SubBytes
void testSubBytes(byte tab[4][4], byte verif[4][4]){
    SubBytes(tab);
    assert(eq_block (tab,verif) || verif_vs_res_block(verif, tab));
}

uint32_t test1 [4] = {0x11,0x0e,0x2b,0x20};
uint32_t verif1[4] = {0x11,0x00000e00,0x002b0000,0x20000000};
uint32_t test2 [4] = {0xd42711ae, 0xe0bf98f1, 0xb8b45de5, 0x1e415230};
uint32_t verif2[4] = {0xd42711ae, 0xbf784e98, 0x5de5b8b4, 0x3000001e};
uint32_t test3 [4] = {0xd42711ae, 0xbf784e98, 0x5de5b8b4, 0x3000001e};
uint32_t verif3[4] = {0x48cc82e4,0x08bc2f46,0x4cd96c8d,0x04636372};

byte tb1[4][4] = {
    {0x00, 0x01, 0x02, 0x03},
    {0x04, 0x05, 0x06, 0x07},
    {0x08, 0x09, 0x0a, 0x0b},
    {0x0c, 0x0d, 0x0e, 0x0f}
};

byte vb1[4][4] = {
    {0x00, 0x01, 0x02, 0x03},
    {0x05, 0x06, 0x07, 0x04},
    {0x0a, 0x0b, 0x08, 0x09},
    {0x0f, 0x0c, 0x0d, 0x0e}
};


int main (void){
    uint32_t test1 [4] = {0x11,0x0e,0x2b,0x20};
    uint32_t verif1[4] = {0x11,0x00000e00,0x002b0000,0x20000000};
    uint32_t test2 [4] = {0xd42711ae, 0xe0bf98f1, 0xb8b45de5, 0x1e415230};
    uint32_t verif2[4] = {0xd42711ae, 0xbf784e98, 0x5de5b8b4, 0x3000001e};
    uint32_t test3 [4] = {0xd42711ae, 0xbf784e98, 0x5de5b8b4, 0x3000001e};
    uint32_t verif3[4] = {0x48cc82e4,0x08bc2f46,0x4cd96c8d,0x04636372};


    testShiftRows(tb1, vb1);

    printf("Every tests passed!\n");
}
