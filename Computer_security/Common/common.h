#ifndef _COMMON_H
#define _COMMON_H

#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#define IS_POWER_OF_TWO(x)        (((x) & ((x) - 1)) == 0)
#define VUNUSED(x)                ((void)(x))

void bitwise_or_array(const uint8_t *array1, const uint8_t *array2, uint8_t *result, const size_t len);
void xor_array(const uint8_t *array1, const uint8_t *array2, uint8_t *result, const size_t len);


void bitwise_or_array_onplace(uint8_t *dest, const uint8_t *src, const size_t len);
void xor_array_onplace(uint8_t *dest, const uint8_t *src, const size_t len);


void left_shift_array(uint8_t *array, const size_t len, const size_t bits);
void right_shift_array(uint8_t *array, const size_t len, const size_t bits);


void print_arr(uint8_t *bytes, size_t len);
bool check_buf_is_empty(const uint8_t *buf, size_t len);

uint8_t *read_hex_string(char *string);
#endif