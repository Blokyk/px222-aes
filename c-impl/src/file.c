#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include "bitmap.h"
#include "cipher.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

void modify_bitmap(char str[],byte key[],size_t keySize,char way){
    FILE * fc;
    FILE * ff; // initialise le fichier final
    fc = fopen(str,"r");
    ff = fopen("image_finale.bmp","w+");
    int i =0;
    fichierEntete entete2;
    fread(&entete2,1,sizeof(entete2),fc);
    imageEntete image2;
    fread(&image2,1,sizeof(image2),fc);
    fseek(fc,entete2.offset,SEEK_SET); // copier l'entête dans ff
    struct stat buf;
    fstat( fc, &buf );
    byte block[16];
    fread(fc,buf.st_size,buf.st_size,block) ;
    byte o[16];
    switch(way){
        case 'A' :
            encrypt_ecb(block,o,16,key,keySize);
        case 'B' :
        //decrypt_ecb
        case 'C' :
        // encrypt_cbc
        case 'D' :
        // decrypt_cbc
        default  :
            printf("Not an option, choose otherwise! \n");
            exit(1);
            return -1;

    }
    fwrite(block,4,4,ff); //on écrit dans le fichier final
    i++;
    fclose(fc);
    fclose(ff);
    return 1;
}
