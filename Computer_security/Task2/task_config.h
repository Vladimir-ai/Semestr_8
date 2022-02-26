#ifndef TASK_CONFIG_H
#define TASK_CONFIG_H

#include <stddef.h>
#include "common.h"

#define BLOCK_SIZE_BYTES          8U
#define BLOCK_PIECES_COUNT        4U
#define IV_SIZE_BYTES             8U
#define ROUND_COUNT               8U
#define TEXT_LEN_BYTES            (BLOCK_SIZE_BYTES * 10U)

#define BLOCK_PIECE_SIZE_BYTES    ((BLOCK_SIZE_BYTES) / (BLOCK_PIECES_COUNT))
#define KEY_LEN_BYTES             ((BLOCK_SIZE_BYTES) * 2U)
#define TEXT_LEN_BLOCKS           ((TEXT_LEN_BYTES) / (BLOCK_SIZE_BYTES))

#define CPY_BLOCK(dst, src)        (memcpy((void *)(dst), (void *)(src), (BLOCK_SIZE_BYTES)))


#ifdef __GNUC__
_Static_assert(IS_POWER_OF_TWO(BLOCK_SIZE_BYTES * 8) && (BLOCK_SIZE_BYTES > 1),
  "Block size in bits should be power of 2");

_Static_assert(IS_POWER_OF_TWO(BLOCK_PIECES_COUNT) && (BLOCK_PIECES_COUNT > 1),
  "Block count should be power of 2");

_Static_assert(BLOCK_SIZE_BYTES >= BLOCK_PIECES_COUNT,
  "Block size (in bytes) should be greater than block count");

_Static_assert(ROUND_COUNT > 0,
  "Round count should be > 0");
#endif /* __GNUC__ */

typedef enum cipher_mode_e
{
  ECB = 1,
  CBC = 2,
  CFB = 3,
  OFB = 4,
  CTR = 5,
  MODE_NUM = 6
} cipher_mode_t;

typedef uint8_t block_elem_t[BLOCK_PIECE_SIZE_BYTES];

typedef union block_u
{
  uint8_t block_bytes[BLOCK_SIZE_BYTES];
  block_elem_t block_piece[BLOCK_PIECES_COUNT];
} block_t;


typedef uint8_t initial_vector_t[IV_SIZE_BYTES];

typedef union cipher_key_u {
  uint8_t cipher_key_bytes[KEY_LEN_BYTES];
  block_elem_t key_piece[KEY_LEN_BYTES / BLOCK_SIZE_BYTES];
} cipher_key_t;

typedef struct cipher_args_s {
  initial_vector_t init_vector;
  cipher_mode_t cipher_mode;
} cipher_args_t;

typedef struct text_s
{
  union
  {
    uint8_t *text_chars;
    block_t *text_blocks;
  } data;
  size_t len_bytes;
} text_t;

typedef void (*processing_func)(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher);

void cipher_text(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher);

#endif /* TASK_CONFIG_H */
