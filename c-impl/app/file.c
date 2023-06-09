#include "file.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "../src/cipher.h"

#include "bitmap.h"

void handle_fopen_error(const FILE *src, const char *filename) {
    if (src == NULL) {
        printf("Couldn't open '%s': %s (errno %d)\n", filename, strerror(errno), errno);
        exit(errno);
        return;
    }
}

__attribute_warn_unused_result__
FILE* prepare_bitmap(FILE *src, const char *destFilename) {
    FILE *dest;

    dest = fopen(destFilename,"wb+");
    handle_fopen_error(dest, destFilename);

    fichierEntete fileHeader;
    fread(&fileHeader, sizeof(fileHeader), 1, src);
    fwrite(&fileHeader, sizeof(fileHeader), 1, dest);

    imageEntete imgHeader;
    fread(&imgHeader, sizeof(imgHeader), 1, src);
    fwrite(&imgHeader, sizeof(imgHeader), 1, dest);

    // go to the start of the raw pixels
    fseek(src, fileHeader.offset, SEEK_SET);
    fseek(dest, fileHeader.offset, SEEK_SET);

    return dest;
}

typedef size_t cipherFunc(
    enum block_mode mode,
    const byte input[],
    byte **output,
    uint32_t dataSize,
    const byte key[],
    short keySize
);

// the two following functions aim to prepare the cipher of a bitmap
void __cipher_bitmap_core(cipherFunc f, const char *filename, const char *destFilename, enum block_mode mode, const byte key[], short keySize) {
    FILE *src = fopen(filename, "rb");
    handle_fopen_error(src, filename);

    struct stat srcInfo;
    fstat(fileno(src), &srcInfo);

    if (srcInfo.st_size >= MAX_UNBUFFERED_SIZE) { // -20 for size + padding
        printf("'%s' is too big (%ld) to be processed, must be %d bytes at most. Use --buffered instead", filename, srcInfo.st_size, MAX_UNBUFFERED_SIZE);
        exit(1);
        return;
    }

    FILE *dest = prepare_bitmap(src, destFilename);

    size_t bytesRead, bytesProcessed;

    byte *srcBuffer = malloc(srcInfo.st_size);

    if (srcBuffer == NULL) {
        printf("Couldn't allocate buffer of %lu bytes for unbuffered encryption/decryption. Maybe the buffered version?", srcInfo.st_size);
        exit(ENOMEM);
        return;
    }

    byte *dstBuffer;
    bytesRead = fread(srcBuffer, 1, srcInfo.st_size, src);
    bytesProcessed = f(mode, srcBuffer, &dstBuffer, bytesRead, key, keySize);

    fwrite(dstBuffer, 1, bytesProcessed, dest);
    printf("Data was %ld bytes long, wrote %ld bytes to dest\n", bytesRead, bytesProcessed);

    fclose(src);
    fclose(dest);

    free(srcBuffer);
    free(dstBuffer);
}

void __cipher_bitmap_core_buffered(cipherFunc f, const char *filename, const char *destFilename, enum block_mode mode, const byte key[], short keySize) {
    FILE *src = fopen(filename, "rb");
    handle_fopen_error(src, filename);

    FILE *dest = prepare_bitmap(src, destFilename);

    size_t bytesRead, bytesProcessed;
    static byte srcBuffer[BUFFER_SIZE];
    byte *dstBuffer = NULL;

    // fixme: why this is not working: when encrypting, we'll read 4096 bytes, but output
    //        e.g. 5016; when decrypting, we'll actually need to read all 5016 bytes at
    //        once if we want the decryption to go okay; except this contradicts the points
    //        of having a function that just takes a func pointer lol
    while ((bytesRead = fread(srcBuffer, 1, BUFFER_SIZE - 4, src)) != 0) {
        bytesProcessed = f(mode, srcBuffer, &dstBuffer, bytesRead, key, keySize);
        fwrite(dstBuffer, 1, bytesProcessed, dest);
        free(dstBuffer);
    }

    fclose(src);
    fclose(dest);
}

void encrypt_bitmap(const char *srcFilename, const char *destFilename, enum block_mode mode, const byte key[], short keySize) {
    __cipher_bitmap_core(encrypt, srcFilename, destFilename, mode, key, keySize);
}

void encrypt_bitmap_buffered(const char *srcFilename, const char *destFilename, enum block_mode mode, const byte key[], short keySize) {
    __cipher_bitmap_core_buffered(encrypt, srcFilename, destFilename, mode, key, keySize);
}

void decrypt_bitmap(const char *srcFilename, const char *destFilename, enum block_mode mode, const byte key[], short keySize) {
    __cipher_bitmap_core(decrypt, srcFilename, destFilename, mode, key, keySize);
}

void decrypt_bitmap_buffered(const char *srcFilename, const char *destFilename, enum block_mode mode, const byte key[], short keySize) {
    __cipher_bitmap_core_buffered(decrypt, srcFilename, destFilename, mode, key, keySize);
}