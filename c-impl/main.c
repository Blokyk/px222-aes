#include <stdio.h>
#include "cipher.h"

int main (void){
    int tab[4][4] = {{0,1,2,3},{4,5,6,7},{8,9,10,11},{12,13,14,15}};
    int new[4][4] ;
    new[4][4] = ShiftRows(tab[4][4]);
    for (int y=0;y<4;y++){
      for (int x=0;x<4;x++){
          int c = new[y][x];
          printf("%c",c); 
      };
}
}