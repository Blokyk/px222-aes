#include <fcntl.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "../src/cipher.h"

struct benchmark_results {
    int64_t average_ms;
    int64_t min_ms;
    int64_t max_ms;
    int64_t kilobytes_per_second;
};

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

void print_results(struct benchmark_results res) {
    print_result_part("Average", "s", "ms", res.average_ms, 39);
    printf("    ");
    print_result_part("Min", "s", "ms", res.min_ms, 32);
    printf("    ");
    print_result_part("Max", "s", "ms", res.min_ms, 31);
    printf("    ");
    print_result_part("Speed", "", " KB/s", res.kilobytes_per_second, 36);
    printf("\n");
}

struct benchmark_results benchmark(benchmarkable f, size_t blocks, size_t keySize, int iteration) {
    size_t dataSize = blocks*16;

    uint8_t *data = malloc(dataSize);
    uint8_t *dest = malloc(dataSize);

    uint8_t *key = malloc(keySize);

    int64_t *ms_results = malloc(iteration*sizeof(int64_t));

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

        printf("| Iteration #%02d: ", i+1);
        fflush(stdout);

        start = clock();
        f(data, dest, dataSize, key, keySize);
        end = clock();

        double seconds = (double)(end - start) / CLOCKS_PER_SEC;
        long ms = (long)(seconds * 1000);

        printf("%02.0lfs %03.0ldms |\n", seconds, ms % 1000);

        ms_results[i] = ms;
    }

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

    res.kilobytes_per_second = 1000 * (16*blocks) / res.average_ms / 1024;

    free(data);
    free(dest);

    return res;
}

int main(void) {
    srand(time(NULL));

    const int blocks = 1000000;

    printf("--- ENCRYPTION (ECB-128) ---\n");
    struct benchmark_results res1 = benchmark(encrypt_ecb, blocks, 16, 10);
    printf("----------------------------\n");

    printf("\n");

    printf("--- DECRYPTION (ECB-128) ---\n");
    struct benchmark_results res2 = benchmark(decrypt_ecb, blocks, 16, 10);
    printf("----------------------------\n");

    printf("\n");

    printf("--- ENCRYPTION (ECB-192) ---\n");
    struct benchmark_results res3 = benchmark(encrypt_ecb, blocks, 24, 10);
    printf("----------------------------\n");

    printf("\n");

    printf("--- DECRYPTION (ECB-192) ---\n");
    struct benchmark_results res4 = benchmark(decrypt_ecb, blocks, 24, 10);
    printf("----------------------------\n");

    printf("\n");

    printf("--- ENCRYPTION (ECB-256) ---\n");
    struct benchmark_results res5 = benchmark(encrypt_ecb, blocks, 32, 10);
    printf("----------------------------\n");

    printf("\n");

    printf("--- DECRYPTION (ECB-256) ---\n");
    struct benchmark_results res6 = benchmark(decrypt_ecb, blocks, 32, 10);
    printf("----------------------------\n");

    printf("\n");

    printf("----------\x1b[1m RESULTS \x1b[0m----------\n");
    printf("ENCRYPTION (ECB-128): ");
    print_results(res1);
    printf("DECRYPTION (ECB-128): ");
    print_results(res2);
    printf("ENCRYPTION (ECB-192): ");
    print_results(res3);
    printf("DECRYPTION (ECB-192): ");
    print_results(res4);
    printf("ENCRYPTION (ECB-256): ");
    print_results(res5);
    printf("DECRYPTION (ECB-256): ");
    print_results(res6);
}