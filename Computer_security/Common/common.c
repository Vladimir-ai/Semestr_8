#include "common.h"

void left_shift_array(uint8_t *array, const size_t len, const size_t bits)
{
  const size_t full_shifts = (bits >> 3) % len;
  size_t curr_shift;
  uint8_t last_element;

  if (len != 1)
  {
    for(curr_shift = 0; curr_shift < full_shifts; curr_shift++)
    {
      last_element = array[0];

      memmove(array, array + 1, len - 1);

      array[len - 1] = last_element;
    }

    if ((curr_shift = bits & 7U))
    {
      last_element = array[len - 1] << curr_shift | array[0] >> (8U - curr_shift);

      for (size_t idx = 0; idx < len - 1; idx++)
      {
        array[idx] = array[idx + 1] << curr_shift | array[idx] >> (8U - curr_shift);
      }

      array[len - 1] = last_element;
    }
  }
  else
  {
    curr_shift = bits & 7U;
    array[0] = array[0] << curr_shift | array[0] >> (8U - curr_shift);
  }

}


void right_shift_array(uint8_t *array, const size_t len, const size_t bits)
{
  const size_t full_shifts = (bits >> 3) % len;
  size_t curr_shift;
  uint8_t first_element;

  if (len != 1)
  {
    for(curr_shift = 0; curr_shift < full_shifts; curr_shift++)
    {
      first_element = array[len - 1];

      memmove(array + 1, array, len - 1);

      array[0] = first_element;
    }

    if ((curr_shift = bits & 7U))
    {
      first_element = array[len - 1] >> curr_shift | array[0] << (8U - curr_shift);

      for (size_t idx = len - 1; idx > 0; idx--)
      {
        array[idx] = array[idx - 1] >> curr_shift | array[idx] << (8U - curr_shift);
      }

      array[0] = first_element;
    }
  }
  else
  {
    curr_shift = bits & 7U;
    array[0] = array[0] >> curr_shift | array[0] << (8U - curr_shift);
  }
}


void bitwise_or_array(const uint8_t *array1, const uint8_t *array2, uint8_t *result, const size_t len)
{
  for (size_t idx = 0; idx < len; idx++)
  {
    result[idx] = array1[idx] | array2[idx];
  }
}


void xor_array(const uint8_t *array1, const uint8_t *array2, uint8_t *result, const size_t len)
{
  for (size_t idx = 0; idx < len; idx++)
  {
    result[idx] = array1[idx] ^ array2[idx];
  }
}



void bitwise_or_array_onplace(uint8_t *dest, const uint8_t *src, const size_t len)
{
  for (size_t idx = 0; idx < len; idx++)
  {
    dest[idx] |= src[idx];
  }
}


void xor_array_onplace(uint8_t *dest, const uint8_t *src, const size_t len)
{
  for (size_t idx = 0; idx < len; idx++)
  {
    dest[idx] ^= src[idx];
  }
}


void print_arr(uint8_t *bytes, size_t len)
{
  for (size_t idx = 0; idx < len; idx++)
  {
    printf("%02x", bytes[idx]);
  }

  printf("\n");
}

