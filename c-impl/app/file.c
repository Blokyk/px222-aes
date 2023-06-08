#include "file.h"

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../src/utils.h"
#include "../src/cipher.h"

int modify_bitmap(char str[],char str2[],byte key[],size_t keySize,char way){
    printf("debuting\n");
    FILE * fc;
    FILE * ff; // initialise le fichier final
    fc = fopen(str,"r");
    if (fc== NULL) return;
    ff = fopen(str2,"w+");
    fichierEntete entete2;
    fread(&entete2,1,sizeof(entete2),fc);
    imageEntete image2;
    fread(&image2,1,sizeof(image2),fc);
    fseek(fc,entete2.offset,SEEK_SET); // copier l'entête dans ff
    struct stat buf;
    fstat(fc->_fileno, &buf);
    printf("%ld\n",buf.st_size);
    int a = buf.st_size+14;
    printf("%d\n",a);
    byte block= malloc(a);
    for (int i=1; i<15 ; i++){
        int x = buf.st_size+14-i;
        byte block[x] = 0x00;
    }
    fread(block,buf.st_size,buf.st_size,fc) ;
    byte tmp[16];
    switch(way){
        case 'A' :
            encrypt_ecb(block,tmp,a,key,keySize);
            break;
        case 'B' :
            decrypt_ecb(block,tmp,buf.st_size,key,keySize);
            break;
        case 'C' :
            encrypt_cbc(block,tmp,buf.st_size,key,keySize);
            break;
        case 'D' :
            decrypt_cbc(block,tmp,buf.st_size,key,keySize);
            break;
        default  :
            printf("Not an option, choose otherwise! \n");
            exit(1);
            return -1;

    }
    fwrite(tmp,buf.st_size,1,ff); //on écrit dans le fichier final
    fclose(fc);
    fclose(ff);
    return 1;
}
