#include <stdio.h>
#include <stdint.h>
#include "../src/cipher.h"
#include "../src/utils.h"

// test de ShiftRows
void testShiftRows(uint32_t tab[4], int verif[4]){
    ShiftRows(tab);
    renvoi (eq_tableau (tab,4,verif,4));
}

int main (void){
    uint32_t test  [4] = {0x11,0x0e,0x2b,0x20};
    uint32_t verif [4] ={0x11,0x00000e00,0x002b0000,0x20000000};
    uint32_t test2 [4] = {0xd42711ae, 0xe0bf98f1, 0xb8b45de5, 0x1e415230};
    uint32_t verif2[4] ={0xd42711ae, 0xbf784e98, 0x5de5b8b4, 0x3000001e};
    testShiftRows(test,verif);
    testShiftRows(test2,verif2);
}
