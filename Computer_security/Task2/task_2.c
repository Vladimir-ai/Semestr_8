#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "common.h"

#define BLOCK_SIZE_BYTES          8U
#define BLOCK_PIECES_COUNT        4U
#define ROUND_COUNT               8U
#define TEXT_LEN_BYTES            (BLOCK_SIZE_BYTES * 10U)

#define BLOCK_PIECE_SIZE_BYTES    ((BLOCK_SIZE_BYTES) / (BLOCK_PIECES_COUNT))
#define KEY_LEN_BYTES             ((BLOCK_SIZE_BYTES) * 2U)
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
  uint8_t cipher_key_bytes[KEY_LEN_BYTES];
  block_elem_t key_piece[KEY_LEN_BYTES / BLOCK_SIZE_BYTES];
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


static bool try_add_padding_to_the_end(text_t *text)
{
  size_t idx;

  if (text->len_bytes % BLOCK_SIZE_BYTES)
  {
    for (idx = 0; (idx + text->len_bytes) % BLOCK_SIZE_BYTES != 0; idx++)
    {
      text->data.text_chars[idx + text->len_bytes] = 0;
    }

    text->data.text_chars[idx + text->len_bytes - 1] = idx;
    text->len_bytes = idx + text->len_bytes;

    return true;
  }

  return false;
}

static bool try_remove_padding_in_end(text_t *text)
{
  size_t padding;

  if ((padding = text->data.text_chars[text->len_bytes - 1] % BLOCK_SIZE_BYTES))
  {
    for (size_t idx = text->len_bytes - 2; padding > 1; padding--)
    {
      if(text->data.text_chars[idx] != 0)
      {
        return false;
      }
    }

    text->len_bytes -= text->data.text_chars[text->len_bytes - 1] % BLOCK_SIZE_BYTES;
  }

  return true;
}

/* Text pointer should allow to add padding bytes */
void cipher_text(text_t *text, cipher_key_t key)
{
  bool padding_added;

  padding_added = try_add_padding_to_the_end(text);

  for (size_t block_idx = 0; block_idx < text->len_bytes; block_idx++)
  {
    for (size_t round = 0; round < ROUND_COUNT; round++)
    {
      right_shift_array(key.cipher_key_bytes, sizeof(key), 12 * round);
      apply_func(&text->data.text_blocks[block_idx], key.key_piece[0]);
    }
  }

  if (!padding_added)
  {
    try_remove_padding_in_end(text);
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


static void print_help_to_stdout(void)
{
  printf("Arguments for this function:\n");
  printf("-s <string>\n");
  printf("-k <key>\n");
  printf("-f <input file> -o <output file>\n");
  printf("-d - run two times (Don't work with files.)\n");
}


int main(int argc, char *argv[])
{
  char c;

  text_t text = {0};
  cipher_key_t key = {0};
  FILE *input_file = NULL;
  FILE *output_file = NULL;
  bool two_runs = false;

  while ((c = getopt (argc, argv, "f:o:s:k:hd")) != -1)
    switch (c)
    {
      case 'h':
        print_help_to_stdout();
        break;
      case 's':
      {
        text.len_bytes = strlen(optarg);

        if (text.len_bytes % BLOCK_SIZE_BYTES)
        {
          text.data.text_chars = (uint8_t *)malloc(text.len_bytes / BLOCK_SIZE_BYTES * BLOCK_SIZE_BYTES + BLOCK_SIZE_BYTES);
        }
        else
        {
          text.data.text_chars = (uint8_t *)malloc(text.len_bytes);
        }

        memcpy(text.data.text_chars, optarg, text.len_bytes);
      }
        break;
      case 'f':
        input_file = fopen(optarg, "r");
        if (!input_file)
        {
          printf("File %s not found\n", optarg);
          printf("Aborting.\n");
          return 1;
        }
        break;
      case 'o':
        output_file = fopen(optarg, "w");
        if (!output_file)
        {
          printf("File %s not found\n", optarg);
          printf("Aborting.\n");
          return 1;
        }
        break;
      case 'k':
        if (strlen(optarg) < KEY_LEN_BYTES)
        {
          printf("Key is too short, should be %d bytes len.\nAborting.\n", KEY_LEN_BYTES);
          return 1;
        }
        else if (strlen(optarg) > KEY_LEN_BYTES)
        {
          printf("Warning: key is too long, truncating...\n");
        }
        memcpy(key.cipher_key_bytes, optarg, strlen(optarg));
        break;
      case 'd':
        two_runs = true;
        break;
      case '?':
        print_help_to_stdout();
        break;
    }

  if (!input_file && !text.data.text_chars)
  {
    printf("Warning: text nor file weren't specified.\n");
    printf("Generating text with length %d bytes\n", TEXT_LEN_BYTES);
  }

  if (input_file && text.data.text_chars)
  {
    printf("Error: you shouldn't specify str and file.\nAborting.\n");
    return 1;
  }

  if (check_buf_is_empty(key.cipher_key_bytes, sizeof(key.cipher_key_bytes)))
  {
    printf("Warning: key wasn't specified. Generating new key...\n");
    generate_arr(key.cipher_key_bytes, sizeof(key.cipher_key_bytes));
  }

  if (input_file)
  {
    ssize_t bytes_ctr;
    text.data.text_chars = malloc(TEXT_LEN_BYTES + 1);

    while((bytes_ctr = read(fileno(input_file), text.data.text_chars, TEXT_LEN_BYTES)))
    {
      text.len_bytes = bytes_ctr;
      cipher_text(&text, key);

      if(output_file)
      {
        fwrite(text.data.text_chars, text.len_bytes, 1, output_file);
        fclose(output_file);
      }
      else
      {
        print_arr(text.data.text_chars, text.len_bytes);
      }
    }

    fclose(input_file);
    free(text.data.text_chars);
    return 0;
  }

  if (!text.data.text_chars)
  {
    printf("Warning: text not specified, generating...\n");
    text.data.text_chars = malloc(TEXT_LEN_BYTES);
    text.len_bytes = TEXT_LEN_BYTES;

    generate_printable(text.data.text_chars, TEXT_LEN_BYTES);
  }

  printf("Plain text: ");
  print_arr(text.data.text_chars, text.len_bytes);

  printf("Key: ");
  print_arr(key.cipher_key_bytes, sizeof(key));

  cipher_text(&text, key);

  if (output_file)
  {
    fwrite(text.data.text_chars, text.len_bytes, 1, output_file);
    fclose(output_file);
  }
  else
  {
    printf("Cipher text: ");
    print_arr(text.data.text_chars, text.len_bytes);

    if (two_runs)
    {
      cipher_text(&text, key);

      printf("Plain text: ");
      print_arr(text.data.text_chars, text.len_bytes);
    }
  }

  free(text.data.text_blocks);
  return 0;
}