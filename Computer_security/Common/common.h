#ifndef _COMMON_H
#define _COMMON_H

#include <stdint.h>
#include <string.h>

void bitwise_or_array(const uint8_t *array1, const uint8_t *array2, uint8_t *result, const size_t len);
void xor_array(const uint8_t *array1, const uint8_t *array2, uint8_t *result, const size_t len);


void bitwise_or_array_onplace(uint8_t *dest, const uint8_t *src, const size_t len);
void xor_array_onplace(uint8_t *dest, const uint8_t *src, const size_t len);


void left_shift_array(uint8_t *array, const size_t len, const size_t bits);
void right_shift_array(uint8_t *array, const size_t len, const size_t bits);


void print_arr(uint8_t *bytes, size_t len);

#endif