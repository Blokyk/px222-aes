#include <stdio.h>
#include "utils.h"

int mask (int a, int n){
    return(n & a);
}
void afficher_tab(int tab[4]){
    int n = 0b11111111000000000000000000000000 ;
    int x = 0b00000000111111110000000000000000 ;
    int y = 0b00000000000000001111111100000000 ;
    int z = 0b00000000000000000000000011111111 ;
    for (int i=  0; i<4; i++){
        int a = tab[i];
        printf("|%02x ",mask (a,n)>>24);
        printf("%02x ",mask (a,x)>> 16);
        printf("%02x ",mask (a,y)>> 8);
        printf("%02x|\n",mask (a,z));
    }
}
