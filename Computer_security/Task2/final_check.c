#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>

#include "task_config.h"
#include "common.h"

#define ITER_COUNT  100000UL
#define FILE_OUTPUT

static void print_help_to_stdout(void)
{
  printf("this file will process all modes.\n");
  printf("h - help\n");
  printf("k - key as hex string\n");
  printf("i - initial vector as hexstring\n");
}

static void check_text(cipher_key_t key, const cipher_args_t args)
{
  const size_t max_iter = ITER_COUNT;
  const size_t min_text_len = 1U;
  const size_t max_text_len = BLOCK_SIZE_BYTES * 2;
  size_t curr_len;
  size_t iter_num = 0;

#ifdef FILE_OUTPUT
  char *output_file_name = "output.txt";

  int fd = open(output_file_name, O_WRONLY | O_CREAT, 0644);
  if (fd == -1) {
      perror("open failed");
      exit(1);
  }

  if (dup2(fd, 1) == -1) {
      perror("dup2 failed");
      exit(1);
  }
#endif /* FILE_OUTPUT */

  for (curr_len = min_text_len; curr_len < max_text_len; curr_len++)
  {
    text_t text = {.len_bytes = curr_len, .data.text_chars = malloc(curr_len)};

    if (curr_len > min_text_len)
    {
      generate_arr(text.data.text_chars, curr_len);
    }
    else
    {
      memset(text.data.text_chars, 0, curr_len);
    }

    for (iter_num = 0; iter_num < max_iter; iter_num++)
    {
      text_t text_copy = {.len_bytes = curr_len, .data.text_chars = malloc(curr_len / BLOCK_SIZE_BYTES * BLOCK_SIZE_BYTES + BLOCK_SIZE_BYTES)};
      cipher_key_t key_copy = {0};
      cipher_args_t args_copy = {.cipher_mode = ECB};

      printf("=========== %ld iter ===========\n", iter_num + 1);
      printf("Key: ");
      print_arr(key.cipher_key_bytes, sizeof(key.cipher_key_bytes));

      printf("IV: ");
      print_arr(args.init_vector, sizeof(args.init_vector));

      printf("Initial plain text, %ld bytes: ", text.len_bytes);
      print_arr(text.data.text_chars, text.len_bytes);

      for (cipher_mode_t mode = ECB; mode < MODE_NUM; mode++)
      {
        printf("Mode: ");
        switch (mode)
        {
        case ECB:
          printf("ECB\n");
          break;

        case CBC:
          printf("CBC\n");
          break;

        case CFB:
          printf("CFB\n");
          break;

        case OFB:
          printf("OFB\n");
          break;

        case CTR:
          printf("CTR\n");
          break;

        default:
          break;
        }

        args_copy.cipher_mode = mode;
        memcpy(key_copy.cipher_key_bytes, key.cipher_key_bytes, KEY_LEN_BYTES);
        memcpy(args_copy.init_vector, args.init_vector, IV_SIZE_BYTES);
        memcpy(text_copy.data.text_chars, text.data.text_chars, text.len_bytes);

        cipher_text(&text_copy, key_copy, args_copy, false);

        printf("Cipher text, %ld bytes: ", text_copy.len_bytes);
        print_arr(text_copy.data.text_chars, text_copy.len_bytes);

        memcpy(key_copy.cipher_key_bytes, key.cipher_key_bytes, KEY_LEN_BYTES);

        cipher_text(&text_copy, key_copy, args_copy, true);

        printf("Plain text, %ld bytes: ", text.len_bytes);
        print_arr(text_copy.data.text_chars, text_copy.len_bytes);

        if (memcmp(text_copy.data.text_chars, text.data.text_chars, curr_len)
          || text.len_bytes != text_copy.len_bytes)
        {
          printf("ERROR: incorrect len\n");
        }
        else
        {
          printf("Ok\n");
        }
      }

      add_to_bigint_arr(text.data.text_chars, curr_len);

      free(text_copy.data.text_chars);
    }

    free(text.data.text_chars);
  }

#ifdef FILE_OUTPUT
  close(fd);
#endif /* FILE_OUTPUT */
}

int main(int argc, char *argv[])
{
  char c;
  cipher_key_t key = {0};
  cipher_args_t cipher_args = {.init_vector = {0}, .cipher_mode = ECB};
  text_t text = {0};
  srand(time(NULL));

  while ((c = getopt (argc, argv, "f:k:i:h")) != -1)
  {
    switch (c)
    {
      case 'h':
        print_help_to_stdout();
        return 0;
      case 'k':
      {
        uint8_t *input_key = read_hex_string(optarg);

        if (strlen((char *) input_key) < KEY_LEN_BYTES)
        {
          printf("Key is too short, should be %d bytes len.\nAborting.\n", KEY_LEN_BYTES);
          return 1;
        }
        else if (strlen((char *) input_key) > KEY_LEN_BYTES)
        {
          printf("Warning: key is too long, truncating...\n");
        }
        memmove(key.cipher_key_bytes, input_key, KEY_LEN_BYTES);
        free(input_key);
      }
      break;
      case 'i':
      {
        uint8_t *input_iv = read_hex_string(optarg);

        if (strlen((char *)input_iv) < IV_SIZE_BYTES)
        {
          printf("IV is too short, should be %d bytes len.\nAborting.\n", BLOCK_SIZE_BYTES);
          return 1;
        }
        else if (strlen((char *)input_iv) > IV_SIZE_BYTES)
        {
          printf("Warning: IV is too long, truncating...\n");
        }
        memcpy(cipher_args.init_vector, input_iv, IV_SIZE_BYTES);
        break;
      }
    }
  }

  if (check_buf_is_empty(cipher_args.init_vector, sizeof(cipher_args.init_vector)))
  {
    generate_arr(cipher_args.init_vector, sizeof(cipher_args.init_vector));
  }

  if (check_buf_is_empty(key.cipher_key_bytes, sizeof(key.cipher_key_bytes)))
  {
    generate_arr(key.cipher_key_bytes, sizeof(key.cipher_key_bytes));
  }

  text.data.text_chars = malloc(TEXT_LEN_BYTES + BLOCK_SIZE_BYTES);
  text.len_bytes = TEXT_LEN_BYTES + 1;

  generate_arr(text.data.text_chars, TEXT_LEN_BYTES + 1);
  check_text(key, cipher_args);
  free(text.data.text_chars);
}