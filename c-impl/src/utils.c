#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include "utils.h"

unsigned int mask (unsigned int a,unsigned int n){
    return(n & a);
}
void afficher_tab(int tab[4]){
    unsigned int n = 0b11111111000000000000000000000000 ;
    unsigned int x = 0b00000000111111110000000000000000 ;
    unsigned int y = 0b00000000000000001111111100000000 ;
    unsigned int z = 0b00000000000000000000000011111111 ;
    for (int i=  0; i<4; i++){
        int a = tab[i];
        printf("|%02x ",mask (a,n)>>24);
        printf("%02x ",mask (a,x)>> 16);
        printf("%02x ",mask (a,y)>> 8);
        printf("%02x|\n",mask (a,z));
    }
}

bool eq_tableau ( int tab[],int b, int verif[],int a){
    if( a != b)return false;
    else
        for (int i = 0; i< b;i++){
            if (tab[i] != verif[i]){
            return false;
            break;
        }
        return true;
    }
}

void renvoi(bool a){
    if (a==true) printf("Test validé!\n");
    else
        printf("Test non validé ...:/\n");
}