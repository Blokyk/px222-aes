#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include "utils.h"

uint32_t mask (uint32_t a,uint32_t n){
    return(n & a);
}
// on choisit le byte qui nous convient


uint32_t take_byte (uint32_t word, int x){
    uint32_t n  = 0b11111111 << (x-1)*8;
    return(mask (word,n) >> ((x-1)*8));
}

void afficher_tab(uint32_t tab[4]){
    for (int i=  0; i<4; i++){
        int a = tab[i];
        printf("|%02x ", take_byte(a, 4));
        printf("%02x ", take_byte(a, 3));
        printf("%02x ", take_byte(a, 2));
        printf("%02x|\n", take_byte(a, 1));
    }
}

bool eq_tableau ( uint32_t tab[],int b, uint32_t verif[],int a){
    if( a != b){return false;}
    else
        for (int i = 0; i< b;i++){
            if (tab[i] != verif[i]){
            printf("On a: \n");
            afficher_tab(tab);
            printf("On voulait: \n");
            afficher_tab(verif);
            return false;
        }
        return true;
    }
}

void renvoi(bool a){
    if (a==true) printf("Test validé!\n");
    else
        printf("Test non validé ...:/\n");
}

uint32_t setByte (uint32_t word,int x, uint32_t b){
    uint32_t n = ~ (0b11111111<<((x-1)*8));
    uint32_t a = n & word;
    return (a | (b<<(x*8)));
}