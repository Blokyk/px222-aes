#include <errno.h>
#include <search.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "file.h"

#include "../src/utils.h"

#define blocks 3

#define MSG_WRONG_KEY_SIZE "Key must be 16, 24 or 32 bytes long, not %ld\n"
#define MSG_MISSING_OPT_ARG "Option %s expects an argument\n"
#define MSG_BOTH_KEY_KEYSIZE "Can't use both --key-size and --key at the same time!\n"

void display_help(const char *appname) {
    printf("Usage: %s <input.bmp> <--key <key> | --key-size <size>> [--decrypt] [--output <output.bmp>] [--mode <ecb | cbc>] [--buffered]\n", appname);
    printf("  <input.bmp>                    Input bitmap image to encrypt/decrypt (must end with .bmp)\n"),
    printf("  -d, --decrypt                  Decrypt the input instead of encrypting it\n");
    printf("  -o, --output <filename.bmp>    Filename for the output image [default: input-enc/-dec.bmp]\n");
    printf("  -m, --mode <ECB | CBC>         Uses the given block mode for the cipher (case insensitive)\n");
    printf("  -k, --key <hex-bytes>          Specifies the key to use, written in hexadecimal\n");
    printf("  -s, --key-size <size>          Use a random key of size: 16 bytes (128 bits), 24 bytes (192 bits) or 32 bytes (256 bits). Incompatible with --decrypt\n");
    printf("  -b, --buffered                 Process the file in chunks of %d bytes of trying do the whole file at once (recommended for large files)\n", BUFFER_SIZE);
}

#define pop_argv(optName) do { if (!try_pop_argv()) { printf(MSG_MISSING_OPT_ARG, optName); exit(1); } } while(0)
#define try_pop_argv() __pop_argv(&argc, &argv, &curr_arg)
__attribute_warn_unused_result__
bool __pop_argv(int *argc, const char *(*argv[]), const char **curr_arg) {
    if (*argc <= 0) {
        printf("Not enough arguments!\n");
        exit(ENODATA);
        return false;
    }

    (*argc) -= 1;
    (*argv) += 1;

    if (*argc == 0) {
        return false;
    } else {
        *curr_arg = (*argv)[0];
        return true;
    }
}

#define in_argv(str) __in_argv(str, argv, argc)
bool __in_argv(const char *key, const char *argv[], int argc) {
    for (int i = 0; i < argc; i++) {
        if (strcmp(key, argv[i]) == 0)
            return true;
    }

    return false;
}

void demo() {
    uint8_t *dest;
    uint8_t data[blocks*16] = {
        0x00, 0x11, 0x22, 0x33,
        0x44, 0x55, 0x66, 0x77,
        0x88, 0x99, 0xaa, 0xbb,
        0xcc, 0xdd, 0xee, 0xff,

        0x8e, 0xa2, 0xb7, 0xca,
        0x51, 0x67, 0x45, 0xbf,
        0xea, 0xfc, 0x49, 0x90,
        0x4b, 0x49, 0x60, 0x89,

        0x01, 0x23, 0x45, 0x67,
        0x89, 0xab, 0xcd, 0xef,
        0xfe, 0xdc, 0xba, 0x98,
        0x76,
    };

    uint8_t key[32];

    for(int i = 0; i < 32; i++)
        key[i] = i; // rand();

    printf("Key:\n");

    uint8_t keyBlock[4][4];
    linear_to_column_first_block(key, keyBlock);
    print_block(keyBlock);

    printf("\nBlocks:\n");

    uint8_t blk[4][4];
    for (int i = 0; i < blocks; i++) {
        linear_to_column_first_block(data + i*16, blk);
        print_block(blk);
        printf("--------------\n");
    }

    size_t encryptedSize = encrypt(CBC_MODE, data, &dest, 45, key, 32);

    printf("\nResult:\n");
    for (int i = 0; i < blocks; i++) {
        linear_to_column_first_block(dest + i*16, blk);
        print_block(blk);
        printf("--------------\n");
    }

    byte *decrypted;

    size_t decryptedSize = decrypt(CBC_MODE, dest, &decrypted, encryptedSize, key, 32);

    printf("\nDecrypted:\n");
    print_array(decrypted, decryptedSize);
    // for (int i = 0; i < blocks; i++) {
    //     linear_to_column_first_block(decrypted + i*16, blk);
    //     print_block(blk);
    //     printf("--------------\n");
    // }

    free(dest);
    free(decrypted);
}

__attribute_warn_unused_result__
char *getDestinationFilename(const char *srcFilename, bool decrypt) {
    size_t filenameLength = strlen(srcFilename);

    size_t newNameLength = filenameLength + 5; // "-enc" + \0

    char *destFilename = malloc(newNameLength); // "-enc" + \0

    strncpy(destFilename, srcFilename, filenameLength - 4);
    destFilename[filenameLength - 4] = '\0'; // we need to append the null byte

    if (decrypt) {
        strcat(destFilename, "-dec.bmp");
    } else {
        strcat(destFilename, "-enc.bmp");
    }

    return destFilename;
}

__attribute_warn_unused_result__
uint8_t decode_digit(char c) {
    if (0x30 <= c && c <= 0x39) {
        return (c & 0xf); // lower nimble/half-byte
    }

    char cl = c | 0x20;

    if (0x61 <= cl && cl <= 0x70) {
        return ((cl - 1) & 0xf) + 10;
    }

    printf("Couldn't interpret character '%c' (0x%x) as an hexadecimal digit\n", c, c);
    exit(1);
    return -1;
}

__attribute_warn_unused_result__
short decode_key(const char *strKey, uint8_t key[]) {
    size_t strKeySize = strlen(strKey);

    int i = 0;

    // if the user added an hex prefix, ignore it
    if (strKeySize >= 2 && strKey[0] == '0' && strKey[1] == 'x') {
        // skip the first two characters;
        strKey += 2;
        strKeySize -= 2;
    }

    // we use (strKeySize+1)/2 instead of just strKeySize
    // cause we want to include odd-number-of-digits cases,
    // this way 31 digits will be seen as 32, 47 as 48, and 63 as 64
    short keySize = (strKeySize + 1) / 2;
    switch (keySize) {
        case 16:
        case 24:
        case 32:
            break;
        default:
            printf(MSG_WRONG_KEY_SIZE, (long)keySize);
            exit(1);
            return -1;
    }

    // if there's an odd number of digits, then we need to pretend there's a zero at the start
    if (strKeySize % 2 == 1) {
        key[0] = decode_digit(strKey[0]);
        i++;
    }

    for (; i < keySize; i++) {
        key[i] = (decode_digit(strKey[i*2]) << 4) | (decode_digit(strKey[i*2+1]));
    }

    log("Decoded '%s' into ", strKey);
    do_debug(print_hex_array(key, keySize));
    log("\n");

    return keySize;
}

int main(int argc, const char *argv[]) {
    srand(time(NULL));

    if (in_argv("-h") || in_argv("--help")) {
        display_help(argv[0]);
        return 0;
    }

    if (argc <= 1) {
        demo();
        return 0;
    }

    enum block_mode mode = ECB_MODE;
    const char *src = NULL;

    bool customDest = false;
    char *dest = NULL;

    bool useRandomKey = false;
    short keySize = 0;
    uint8_t key[32];

    bool buffered = false, decrypt = false;

    const char *curr_arg = argv[0];
    while (try_pop_argv()) { // we call it directly cause the first one is the app name, which we'll ignore anyway
        if (strcmp("-d", curr_arg) == 0 || strcmp("--decrypt", curr_arg) == 0) {
            decrypt = true;
            continue;
        }

        if (strcmp("-o", curr_arg) == 0 || strcmp("--output", curr_arg) == 0) {
            pop_argv("--output");

            if (dest != NULL) {
                printf("Can't specify more than one output!\n");
                exit(1);
                return 1;
            }

            customDest = true;
            dest = (char*)curr_arg; // discard const
            continue;
        }

        if (strcmp("-k", curr_arg) == 0 || strcmp("--key", curr_arg) == 0) {
            pop_argv("--key");

            if (useRandomKey) {
                printf(MSG_BOTH_KEY_KEYSIZE);
                exit(1);
                return 1;
            }

            keySize = decode_key(curr_arg, key);
            continue;
        }

        if (strcmp("-s", curr_arg) == 0 || strcmp("--key-size", curr_arg) == 0) {
            if (keySize != 0) {
                printf(MSG_BOTH_KEY_KEYSIZE);
                exit(1);
                return 1;
            }

            pop_argv("--key-size");

            useRandomKey = true;

            uint64_t l_keySize = strtoul(curr_arg, NULL, 10);

            switch(l_keySize) {
                case 16:
                case 128:
                    keySize = 16;
                    break;
                case 24:
                case 192:
                    keySize = 24;
                    break;
                case 32:
                case 256:
                    keySize = 32;
                    break;
                default:
                    printf(MSG_WRONG_KEY_SIZE, l_keySize);
                    exit(1);
                    return 1;
            };

            continue;
        }

        if (strcmp("-m", curr_arg) == 0 || strcmp("--mode", curr_arg) == 0) {
            pop_argv("--mode");

            if (strcmp("ecb", curr_arg) == 0 || strcmp("ECB", curr_arg) == 0) {
                mode = ECB_MODE;
                continue;
            }

            if (strcmp("cbc", curr_arg) == 0 || strcmp("CBC", curr_arg) == 0) {
                mode = CBC_MODE;
                continue;
            }

            printf("This app doesn't support the '%s' mode\n", curr_arg);
            exit(ENOTSUP);
            return 1;
        }

        if (strcmp("-b", curr_arg) == 0 || strcmp("--buffered", curr_arg) == 0) {
            buffered = true;
            continue;
        }

        if (strncmp(curr_arg, "-", 1) == 0) {
            printf("Couldn't understand option '%s'\n", curr_arg);
            exit(1);
            return 1;
        }

        // if we get here, then it's not an option, so it's the name of the input file
        if (src != NULL) {
            printf("Can't specify more than one input files!\n");
            exit(1);
            return 1;
        }

        src = curr_arg;
    }

    if (!useRandomKey && keySize == 0) {
        if (decrypt) {
            printf("You must specify a decryption key using --key\n");
        } else {
            printf("You must either specify a --key or generate a random one with --key-size\n");
        }

        exit(1);
        return 1;
    }

    if (useRandomKey && decrypt) {
        printf("The '--key-size' option to generate a random cannot be used with '--decrypt'\n");
        exit(1);
        return 1;
    }

    size_t filenameLength = strlen(src);
    if (filenameLength < 5 && strcmp(src + filenameLength - 4, ".bmp") != 0) {
        printf("File must be a .bmp image!\n");
        exit(1);
        return 1;
    }

    if (useRandomKey) {
        printf("Using random key because s was specified!\n");
        for(int i = 0; i < keySize; i++) key[i] = rand();
    }

    if (dest == NULL) {
        dest = getDestinationFilename(src, decrypt);
    }

    if (decrypt) {
        printf("Decrypting ");
    } else {
        printf("Encrypting ");
    }
    printf("'%s' into '%s' ", src, dest);
    printf("using AES-%d-%s ", keySize * 8, mode == ECB_MODE ? "ECB" : "CBC");
    printf("with key=");
    print_hex_array(key, keySize);
    if (buffered) {
        printf(" (buffered)");
    }
    printf("\n");

    if (buffered)
        if (decrypt)
            decrypt_bitmap_buffered(src, dest, mode, key, keySize);
        else
            encrypt_bitmap_buffered(src, dest, mode, key, keySize);
    else
        if (decrypt)
            decrypt_bitmap(src, dest, mode, key, keySize);
        else
            encrypt_bitmap(src, dest, mode, key, keySize);

    if (!customDest)
        free(dest);

    return 0;
}