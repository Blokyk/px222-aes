#include <stdio.h>
#include <ctype.h>
#include "bitmap.h"
#include "cipher.h"

int encrypt_bitmap(char str[],byte key[],size_t keySize,char way){ // prend en argument le  nom dufichier
    FILE * fc;
    FILE * ff; // initialise le fichier final
    fc = fopen(str,"r");
    ff = fopen("image_finale.jpg","w+");
    int i =0;
    fichierEntete entete2;
    fread(&entete2,1,sizeof(entete2),fc);
    imageEntete image2;
    fread(&image2,1,sizeof(image2),fc);
    fseek(fc,entete2.offset,SEEK_SET);
    while(1){ // condition à revoir
        byte block[16];
        for(int x=0;x<16;x++){
            byte c = fgetc(fc);
            block[x] = c;
            if(NULL == fgetc(fc)){
                //procédure
            }
        }
        byte o[16];
        switch(way){
            case 'A' :
            encrypt_ecb(block,o,16,key,keySize);
            case 'B' :
            // decrypt_ecb
            case 'C' :
            // encrypt_cbc
            case 'D' :
            // decrypt_cbc
            default  :
            printf("Not an option, choose otherwise! \n");
            exit(1);
            return -1;

        }
        fputc(block,ff); //on écrit dans le fichier final
        i++;
    }
    fclose(fc);
    fclose(ff);
    return 1;
}
