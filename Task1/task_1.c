#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define BLOCK_SIZE_BYTES          8U
#define BLOCK_PIECES_COUNT        4U
#define ROUND_COUNT               8U
#define TEXT_LEN_BYTES            80U

#define BLOCK_PIECE_SIZE_BYTES    ((BLOCK_SIZE_BYTES) / (BLOCK_PIECES_COUNT))
#define TEXT_LEN_BLOCKS           ((TEXT_LEN_BYTES) / (BLOCK_SIZE_BYTES))

#define IS_POWER_OF_TWO(x)        (((x) & ((x) - 1)) == 0)

#ifdef __GNUC__
_Static_assert(IS_POWER_OF_TWO(BLOCK_SIZE_BYTES * 8) && (BLOCK_SIZE_BYTES > 1),
  "Block size in bits should be power of 2");

_Static_assert(IS_POWER_OF_TWO(BLOCK_PIECES_COUNT) && (BLOCK_PIECES_COUNT > 1),
  "Block count should be power of 2");

_Static_assert(BLOCK_SIZE_BYTES > BLOCK_PIECES_COUNT,
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

typedef union text_u
{
  uint8_t text_chars[TEXT_LEN_BYTES];
  block_t text_blocks[TEXT_LEN_BYTES / BLOCK_SIZE_BYTES];
} text_t;

void generate_arr(uint8_t *arr, size_t size)
{
  for (size_t ind = 0; ind < size; ind++)
  {
    arr[ind] = rand() % UINT8_MAX;
  }
}

void apply_func(block_t *block, const block_elem_t key)
{
  block_elem_t block_piece;

  memcpy(block_piece, block->block_piece[0], sizeof(block->block_piece[0]));

  for(size_t byte = 0; byte < sizeof(block_elem_t); byte++)
  {
    block->block_piece[0][byte] = block->block_piece[1][byte] ^ key[byte];
  }

  for (int idx = 2; idx < BLOCK_PIECES_COUNT; idx++)
  {
    memmove(block->block_piece[idx - 1], block->block_piece[idx], sizeof(*block->block_piece));
  }

  memmove(block->block_piece[BLOCK_PIECES_COUNT - 1], block_piece, sizeof(block_piece));
}

void process_text(text_t *text, cipher_key_t key)
{
  for (size_t block_idx = 0; block_idx < TEXT_LEN_BLOCKS; block_idx++)
  {
    for (size_t round = 0; round < ROUND_COUNT; round++)
    {
      apply_func(&text->text_blocks[block_idx], key.key_piece[round]);
    }
  }
}

void print_arr(uint8_t *bytes, size_t len)
{
  for (size_t idx = 0; idx < len; idx++)
  {
    printf("%c", bytes[idx]);
  }

  printf("\n");
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

  generate_printable(text.text_chars, sizeof(text));
  generate_arr(key.cipher_key_bytes, sizeof(key));

  print_arr(text.text_chars, sizeof(text));
  print_arr(key.cipher_key_bytes, sizeof(key));

  process_text(&text, key);

  print_arr(text.text_chars, sizeof(text));

  process_text(&text, key);

  print_arr(text.text_chars, sizeof(text));


  return 0;
}