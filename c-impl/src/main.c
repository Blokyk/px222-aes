#include <stdio.h>
#include "cipher.h"
#include "utils.h"

int main (void){
    int tab[4] = {17,14,43,32};
    int new[4];
    new[4] = ShiftRows(tab);
    afficher_tab(new);
    afficher_tab(tab);
}