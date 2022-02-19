#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#define BLOCK_SIZE_BYTES          8U
#define BLOCK_PIECES_COUNT        4U
#define ROUND_COUNT               8U
#define TEXT_LEN_BYTES            16U

#define BLOCK_PIECE_SIZE_BYTES    ((BLOCK_SIZE_BYTES) / (BLOCK_PIECES_COUNT))
#define TEXT_LEN_BLOCKS           ((TEXT_LEN_BYTES) / (BLOCK_SIZE_BYTES))

#define IS_POWER_OF_TWO(x)        (((x) & ((x) - 1)) == 0)

#ifdef __GNUC__
_Static_assert(IS_POWER_OF_TWO(BLOCK_SIZE_BYTES * 8) && (BLOCK_SIZE_BYTES > 1),
  "Block size in bits should be power of 2");

_Static_assert(IS_POWER_OF_TWO(BLOCK_PIECES_COUNT) && (BLOCK_PIECES_COUNT > 1),
  "Block count should be power of 2");

_Static_assert(BLOCK_SIZE_BYTES >= BLOCK_PIECES_COUNT,
  "Block size (in bytes) should be greater than block count");

_Static_assert(ROUND_COUNT > 0,
  "Round count should be > 0");

_Static_assert(TEXT_LEN_BYTES % BLOCK_SIZE_BYTES == 0,
  "TEXT_LEN_BYTES % BLOCK_SIZE_IN_BYTES shouldn't be equal to 0");
#endif /* __GNUC__ */

typedef uint8_t block_elem_t[BLOCK_PIECE_SIZE_BYTES];

typedef union block_u
{
  uint8_t block_bytes[BLOCK_SIZE_BYTES];
  block_elem_t block_piece[BLOCK_PIECES_COUNT];
} block_t;


typedef union cipher_key_u {
  uint8_t cipher_key_bytes[BLOCK_PIECES_COUNT * ROUND_COUNT];
  block_elem_t key_piece[ROUND_COUNT];
} cipher_key_t;

typedef struct text_s
{
  union
  {
    uint8_t *text_chars;
    block_t *text_blocks;
  } data;
  size_t len_bytes;
} text_t;


static void apply_func(block_t *block, const block_elem_t key)
{
  block_elem_t block_piece;
  block_elem_t fst_block;
  block_elem_t shifted_key;

  memcpy(block_piece, block->block_piece[0], sizeof(block->block_piece[0]));
  memcpy(fst_block, block->block_piece[1], BLOCK_PIECE_SIZE_BYTES);
  memcpy(shifted_key, key, sizeof(shifted_key));

  left_shift_array(shifted_key, sizeof(shifted_key), 3);

  left_shift_array(fst_block, sizeof(block_piece), 8);

  xor_array_onplace(fst_block, key, sizeof(fst_block));

  memcpy(block->block_piece[0], fst_block, sizeof(fst_block));

  for (int idx = 2; idx < BLOCK_PIECES_COUNT; idx++)
  {
    memmove(block->block_piece[idx - 1], block->block_piece[idx], sizeof(*block->block_piece));
  }

  memmove(block->block_piece[BLOCK_PIECES_COUNT - 1], block_piece, sizeof(block_piece));
}


void cipher_text(text_t *text, cipher_key_t key)
{
  for (size_t block_idx = 0; block_idx < text->len_bytes; block_idx++)
  {
    for (size_t round = 0; round < ROUND_COUNT; round++)
    {
      right_shift_array(key.cipher_key_bytes, sizeof(key), 12 * round);
      apply_func(&text->data.text_blocks[block_idx], key.key_piece[0]);
    }
  }
}


static void generate_arr(uint8_t *arr, size_t size)
{
  for (size_t ind = 0; ind < size; ind++)
  {
    arr[ind] = rand() % UINT8_MAX;
  }
}


void generate_printable(uint8_t *bytes, size_t len)
{
  uint8_t last_char = 'a';
  for (size_t idx = 0; idx < len; idx++)
  {
    if(last_char == 'z')
    {
      last_char = 'a';
    }

    bytes[idx] = last_char;
    last_char++;
  }
}


int main(int argc, char *argv[])
{
  text_t text;
  cipher_key_t key;

  text.len_bytes = 140U;

  if (text.len_bytes % BLOCK_SIZE_BYTES)
  {
    text.data.text_chars = (uint8_t *)malloc(text.len_bytes / BLOCK_SIZE_BYTES * BLOCK_SIZE_BYTES + BLOCK_SIZE_BYTES);
  }
  else
  {
    text.data.text_chars = (uint8_t *)malloc(text.len_bytes);
  }

  generate_printable(text.data.text_chars, text.len_bytes);
  generate_arr(key.cipher_key_bytes, sizeof(key));

  printf("Plain text: ");
  print_arr(text.data.text_chars, text.len_bytes);
  printf("Key: ");
  print_arr(key.cipher_key_bytes, sizeof(key));

  cipher_text(&text, key);

  printf("Cipher text: ");
  print_arr(text.data.text_chars, text.len_bytes);

  cipher_text(&text, key);

  printf("Plain text: ");
  print_arr(text.data.text_chars, text.len_bytes);

  free(text.data.text_blocks);

  return 0;
}