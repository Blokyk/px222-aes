#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <sys/time.h>

#include "../src/cipher.h"

#define ITER 10

int main(void) {
    srand(time(NULL));

    const int blocks = 1000000;

    uint8_t *data = malloc(blocks*16);
    uint8_t *dest = malloc(blocks*16);

    for(size_t i = 0; i < blocks*16; i++)
       data[i] = rand();

    uint8_t key[16];

    for(size_t i = 0; i < 16; i++)
       key[i] = rand();

    encrypt_ecb(data, dest, blocks*16, key, 16);

    free(data);
    free(dest);
    return 0;

    struct timeval results[ITER];

    for (int i = 0; i < ITER; i++) {
        struct timeval start, end;

        for(size_t i = 0; i < blocks*16; i++)
            data[i] = rand();

        uint8_t key[16];

        for(size_t i = 0; i < 16; i++)
            key[i] = rand();

        printf("Iteration #%02d ", i+1);
        fflush(stdout);

        gettimeofday(&start, NULL);
        encrypt_ecb(data, dest, blocks*16, key, 16);
        gettimeofday(&end, NULL);

        timersub(&end, &start, results+i);
        printf("%02lds %03ldms\n", results[i].tv_sec, results[i].tv_usec/1000);
    }

    int64_t msec_results[ITER];

    for (int i = 0; i < ITER; i++)
        msec_results[i] = results[i].tv_sec*1000 + results[i].tv_usec/1000;

    int64_t avrg_msec = 0;
    int64_t min_msec = INT32_MAX, max_msec = INT32_MIN;

    for (int i = 0; i < ITER; i++) {
        int val = msec_results[i];

        if (min_msec >= val) min_msec = val;
        if (max_msec <= val) max_msec = val;

        avrg_msec += val;
    }

    avrg_msec /= ITER;

    printf("Average: %02lds %03ldms    Min: %02lds %03ldms    Max: %02lds %03ldms\n",
        avrg_msec / 1000, avrg_msec % 1000,
        min_msec / 1000, min_msec % 1000,
        max_msec / 1000, max_msec % 1000
    );

    free(data);
    free(dest);
}