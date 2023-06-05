#include <stdio.h>
#include "../src/cipher.h"
#include "../src/utils.h"

// test de ShiftRows
int testShiftRows(unsigned int tab[4], int verif[4]){
    ShiftRows(tab);
    renvoi (eq_tableau (tab,4,verif,5));
}

int main (void){
    unsigned int tab[4] = {0x11,0x0e,0x2b,0x20};
    unsigned int verif[4] ={0x11,0x00000e00,0x002b0000,0x20000000};
    testShiftRows(tab,verif);
}
