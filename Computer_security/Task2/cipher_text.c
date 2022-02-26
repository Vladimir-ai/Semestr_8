#include "task_config.h"
#include <stdlib.h>
#include <stdio.h>

void process_ecb(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher);
void process_cbc(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher);
void process_cfb(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher);
void process_ofb(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher);
void process_ctr(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher);

static processing_func process_algo[] =
{
  process_ecb,
  process_cbc,
  process_cfb,
  process_ofb,
  process_ctr
};

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


static void process_block(block_t *block, cipher_key_t key)
{
  for (size_t round = 0; round < ROUND_COUNT; round++)
  {
    right_shift_array(key.cipher_key_bytes, sizeof(key), 12 * round);
    apply_func(block, key.key_piece[0]);
  }
}


/* Text pointer should allow to add padding bytes */
void cipher_text(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher)
{
  if (text->len_bytes == 0)
  {
    return;
  }

  if (!decipher)
  {
    try_add_padding_to_the_end(text);
  }

  process_algo[args.cipher_mode - 1](text, key, args, decipher);

  if (decipher)
  {
    try_remove_padding_in_end(text);
  }
}


void process_ecb(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher)
{
  const size_t block_count = text->len_bytes / BLOCK_SIZE_BYTES;

  for (size_t block_idx = 0; block_idx < block_count; block_idx++)
  {
    process_block(&text->data.text_blocks[block_idx], key);
  }
}


void process_cbc(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher)
{
  const size_t block_count = text->len_bytes / BLOCK_SIZE_BYTES;
  block_t temp_block[2];

  if (!decipher)
  {
    xor_array_onplace(text->data.text_blocks[0].block_bytes,
                    args.init_vector,
                    BLOCK_SIZE_BYTES);

    process_block(&text->data.text_blocks[0], key);
  }
  else
  {
    temp_block[0] = text->data.text_blocks[0];

    process_block(&text->data.text_blocks[0], key);

    xor_array_onplace(text->data.text_blocks[0].block_bytes,
                    args.init_vector,
                    BLOCK_SIZE_BYTES);
  }

  for (size_t block_idx = 1; block_idx < block_count; block_idx++)
  {
    if (!decipher)
    {
      xor_array_onplace(text->data.text_blocks[block_idx].block_bytes,
                    text->data.text_blocks[block_idx - 1].block_bytes,
                    BLOCK_SIZE_BYTES);
    }
    else
    {
      temp_block[block_idx & 1U] = text->data.text_blocks[block_idx];
    }

    process_block(&text->data.text_blocks[block_idx], key);

    if (decipher)
    {
      xor_array_onplace(text->data.text_blocks[block_idx].block_bytes,
                      temp_block[!(block_idx & 1U)].block_bytes,
                      BLOCK_SIZE_BYTES);
    }
  }
}


void process_cfb(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher)
{
  const size_t block_count = text->len_bytes / BLOCK_SIZE_BYTES;
  block_t temp_value;

  memcpy(temp_value.block_bytes, args.init_vector, BLOCK_SIZE_BYTES);

  for (size_t block_idx = 0; block_idx < block_count; block_idx++)
  {
    left_shift_array(temp_value.block_bytes, BLOCK_SIZE_BYTES, 2);
    process_block(&temp_value, key);

    if (!decipher)
    {
      xor_array_onplace(text->data.text_blocks[block_idx].block_bytes,
                  temp_value.block_bytes,
                  BLOCK_SIZE_BYTES);

      memcpy(temp_value.block_bytes, text->data.text_blocks[block_idx].block_bytes, BLOCK_SIZE_BYTES);
    }
    else
    {
      block_t value_to_send;

      memcpy(value_to_send.block_bytes,
            text->data.text_blocks[block_idx].block_bytes,
            BLOCK_SIZE_BYTES);

      xor_array_onplace(text->data.text_blocks[block_idx].block_bytes,
                  temp_value.block_bytes,
                  BLOCK_SIZE_BYTES);

      memcpy(temp_value.block_bytes, value_to_send.block_bytes, BLOCK_SIZE_BYTES);
    }
  }
}


void process_ofb(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher)
{
  const size_t block_count = text->len_bytes / BLOCK_SIZE_BYTES;
  block_t temp_value;

  memcpy(temp_value.block_bytes,
    args.init_vector,
    sizeof(args.init_vector));

  for (size_t block_idx = 0; block_idx < block_count; block_idx++)
  {
    process_block(&temp_value, key);

    xor_array_onplace(text->data.text_blocks[0].block_bytes,
                    temp_value.block_bytes,
                    BLOCK_SIZE_BYTES);
  }
}

void process_ctr(text_t *text, cipher_key_t key, const cipher_args_t args, const bool decipher)
{
  const size_t block_count = text->len_bytes / BLOCK_SIZE_BYTES;
  block_t temp_value;

  memcpy(temp_value.block_bytes, args.init_vector, BLOCK_SIZE_BYTES);

  for (size_t block_idx = 0; block_idx < block_count; block_idx++)
  {
    add_to_bigint_arr(temp_value.block_bytes, BLOCK_PIECE_SIZE_BYTES);
    process_block(&temp_value, key);

    xor_array_onplace(text->data.text_blocks[block_idx].block_bytes,
        temp_value.block_bytes,
        BLOCK_SIZE_BYTES);
  }
}