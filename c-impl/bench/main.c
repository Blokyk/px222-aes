#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "../src/cipher.h"

struct benchmark_results {
    char *name;
    int64_t average_ms;
    int64_t min_ms;
    int64_t max_ms;
    int64_t kilobytes_per_second;
};

typedef struct benchmark_results results_t;

void free_result(results_t res) {
    free(res.name);
}

typedef void *benchmarkable(byte*, byte*, size_t, byte*, size_t);

void print_result_part(const char *partName, const char *sep, const char* suffix, int64_t value, uint8_t color) {
    printf("%s: \x1b[1;%dm", partName, color);

    if (value < 1000) {
        printf("%03ld", value);
    } else {
        printf("%02ld\x1b[0m%s\x1b[1;%dm%03ld", value / 1000, sep, color, value % 1000);
    }

    printf("\x1b[0m%s", suffix);
}

void print_results(results_t res, bool verbose) {
    printf("\x1b[97m%s\x1b[0m:\t", res.name);

    if (!verbose) {
        printf("\x1b[1;39m%05ld\x1b[0m kb/s ", res.kilobytes_per_second);
        printf("[\x1b[32m%03ld\x1b[0m/\x1b[1;36m%03ld\x1b[0m/\x1b[31m%03ld\x1b[0m]\n",
            res.min_ms, res.average_ms, res.max_ms);
        return;
    }

    print_result_part("Speed", "", "kb/s", res.kilobytes_per_second, 39);
    printf("    ");
    print_result_part("Average", "s", "ms", res.average_ms, 36);
    printf("    ");
    print_result_part("Min", "s", "ms", res.min_ms, 32);
    printf("    ");
    print_result_part("Max", "s", "ms", res.min_ms, 31);
    printf("\n");
}

struct benchmark_results benchmark(const char *name, benchmarkable f, size_t blocks, size_t keySize, int iteration, bool verbose) {
    size_t dataSize = blocks*16;

    uint8_t *data = malloc(dataSize);
    uint8_t *dest = malloc(dataSize);

    uint8_t *key = malloc(keySize);

    int64_t *ms_results = malloc(iteration*sizeof(int64_t));

    if (verbose) {
        printf("--- %s ---\n", name);
        printf("Params: blocks = %ld, key = %ld, N = %d\n", blocks, keySize, iteration);
    } else {
        printf("Running: %s (00/%02d)", name, iteration);
        fflush(stdout);
    }

    for (int i = 0; i < iteration; i++) {
        static clock_t start, end;

        int fd = open("/dev/random", O_RDONLY);
        dataSize = read(fd, data, dataSize);

        if (dataSize <= 0) {
            printf("Couldn't read from /dev/random, something is definitely wrong!\n");
            exit(1);
        }

        for(int i = 0; i < keySize; i++)
            key[i] = rand();

        if (verbose) {
            printf("| Iteration #%02d: ", i+1);
            fflush(stdout);
        } else {
            printf("\x1b[7D(%02d/%02d)", i+1, iteration);
            fflush(stdout);
        }

        start = clock();
        f(data, dest, dataSize, key, keySize);
        end = clock();

        double seconds = (double)(end - start) / CLOCKS_PER_SEC;
        long ms = (long)(seconds * 1000);

        if (verbose) {
            printf("%02.0lfs %03.0ldms |\n", seconds, ms % 1000);
        }

        ms_results[i] = ms;
    }

    printf("\n");

    struct benchmark_results res = {
        .average_ms = 0,
        .min_ms = INT64_MAX,
        .max_ms = INT64_MIN
    };

    for (int i = 0; i < iteration; i++) {
        int val = ms_results[i];

        if (res.min_ms >= val) res.min_ms = val;
        if (res.max_ms <= val) res.max_ms = val;

        res.average_ms += val;
    }

    res.average_ms /= iteration;

    res.kilobytes_per_second = (1000 * (16*blocks) / res.average_ms) / 1024;

    size_t nameLength = strlen(name);
    res.name = malloc(nameLength+1);
    memcpy(res.name, name, nameLength+1);

    if (verbose) {
        printf("----");
        for (int i = 0; i < nameLength; i++)
            printf("-");
        printf("----\n\n");
    }

    free(data);
    free(dest);

    return res;
}

void benchmark_ecb(int blocks, bool verbose) {


    struct benchmark_results res1 = benchmark(
        "ENCRYPTION (ECB-128)",
        encrypt_ecb,
        blocks, 16, 10,
        verbose
    );

    struct benchmark_results res2 = benchmark(
        "DECRYPTION (ECB-128)",
        decrypt_ecb,
        blocks, 16, 10,
        verbose
    );

    struct benchmark_results res3 = benchmark(
        "ENCRYPTION (ECB-192)",
        encrypt_ecb,
        blocks, 24, 10,
        verbose
    );

    struct benchmark_results res4 = benchmark(
        "DECRYPTION (ECB-192)",
        decrypt_ecb,
        blocks, 24, 10,
        verbose
    );

    struct benchmark_results res5 = benchmark(
        "ENCRYPTION (ECB-256)",
        encrypt_ecb,
        blocks, 32, 10,
        verbose
    );

    struct benchmark_results res6 = benchmark(
        "DECRYPTION (ECB-256)",
        decrypt_ecb,
        blocks, 32, 10,
        verbose
    );

    printf("\n");

    printf("----------\x1b[1m RESULTS \x1b[0m----------\n");
    print_results(res1, verbose);
    print_results(res2, verbose);
    print_results(res3, verbose);
    print_results(res4, verbose);
    print_results(res5, verbose);
    print_results(res6, verbose);

    free_result(res1);
    free_result(res2);
    free_result(res3);
    free_result(res4);
    free_result(res5);
    free_result(res6);
}

int main(int argc, char const *argv[]) {
    srand(time(NULL));

    const int blocks = 1000000;

    bool verbose = false;

    if (argc == 2) {
        if (strcmp(argv[0], "-v") || strcmp(argv[0], "--verbose"))
            verbose = true;
    }

    benchmark_ecb(blocks, verbose);
}