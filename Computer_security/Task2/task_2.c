#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>

#include "task_config.h"


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


static void generate_iv_if_needed_with_print(cipher_args_t *args)
{
  if (args->cipher_mode != ECB &&
    check_buf_is_empty(args->init_vector, sizeof(args->init_vector)))
  {
    printf("Warning: generating IV...\n");

    generate_arr(args->init_vector, sizeof(args->init_vector));

    printf("IV: ");
    print_arr(args->init_vector, sizeof(args->init_vector));
  }
}

/* string should be allocated using malloc */
static uint8_t *update_len_if_needed(char *string)
{
  const size_t len = strlen((char *) string);
  uint8_t *ret;

  if (len % BLOCK_SIZE_BYTES)
  {
    ret = (uint8_t *)malloc(len / BLOCK_SIZE_BYTES * BLOCK_SIZE_BYTES + BLOCK_SIZE_BYTES + 1);
  }
  else
  {
    ret = (uint8_t *)malloc(len + 1);
  }

  memcpy(ret, string, len);

  return ret;
}


static void print_help_to_stdout(void)
{
  printf("Arguments for this function:\n");
  printf("-s <string> - try to cipher or decipher text from string\n");
  printf("-k <key>\n\n");

  printf("-m <mode_num> - use this mode (default mode: ECB):\n");
  printf( "1) Use ECB mode (Electronic codebook)\n");
  printf( "2) Use CBC mode (Cipher block chaining)\n");
  printf( "3) Use CFB mode (Cipher feedback)\n");
  printf( "4) Use OFB mode (Output feedback)\n");
  printf( "5) Use CTR mode (Counter)\n\n");

  printf("-i initial vector (IV)\n");

  printf("-f <input file> -o <output file>\n");
  printf("-c - cipher mode[default]\n");
  printf("-d - decipher mode\n");
}


int main(int argc, char *argv[])
{
  char c;
  bool decipher = false;
  bool cipher = false;

  text_t text = {0};
  cipher_key_t key = {0};
  cipher_args_t cipher_args = {.init_vector = {0}, .cipher_mode = ECB};
  FILE *input_file = NULL;
  FILE *output_file = NULL;

  while ((c = getopt (argc, argv, "f:o:s:k:m:i:b:hdc")) != -1)
    switch (c)
    {
      case 'h':
        print_help_to_stdout();
        return 0;
      case 'b':
        {
          uint8_t *ptr_to_free;
          text.data.text_chars = read_hex_string(optarg);

          ptr_to_free = text.data.text_chars;

          text.data.text_chars = update_len_if_needed((char *) text.data.text_chars);

          text.len_bytes = strlen((char *) text.data.text_chars);
          free(ptr_to_free);
        }
        break;
      case 's':
        {
          text.len_bytes = strlen(optarg);
          text.data.text_chars = malloc(text.len_bytes + BLOCK_SIZE_BYTES);
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
        {
          uint8_t *input_key = read_hex_string(optarg);

          if (strlen(optarg) < KEY_LEN_BYTES * 2)
          {
            printf("Key is too short, should be %d bytes len.\nAborting.\n", KEY_LEN_BYTES);
            return 1;
          }
          else if (strlen(optarg) > 2 * KEY_LEN_BYTES)
          {
            printf("Warning: key is too long, truncating...\n");
          }
          memmove(key.cipher_key_bytes, input_key, KEY_LEN_BYTES);
          free(input_key);
        }
        break;
      case 'd':
        decipher = true;
        break;
      case 'c':
        cipher = true;
        break;
      case 'm':
      {
        int32_t mode;
        if ((mode = atoi(optarg)) && mode > 0 && mode < MODE_NUM)
        {
          cipher_args.cipher_mode = mode;
        }
        else
        {
          printf("Invalid mode. Aboring.\n");
          return 1;
        }
      }
        break;
      case 'i':
      {
        uint8_t *input_key = read_hex_string(optarg);

        if (strlen(optarg) < IV_SIZE_BYTES * 2)
        {
          printf("IV is too short, should be %d bytes len.\nAborting.\n", BLOCK_SIZE_BYTES);
          return 1;
        }
        else if (strlen(optarg) > IV_SIZE_BYTES * 2)
        {
          printf("Warning: IV is too long, truncating...\n");
        }
        memcpy(cipher_args.init_vector, input_key, IV_SIZE_BYTES);
        break;
      }
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

  if ((!text.data.text_chars && !input_file) && (!cipher && decipher))
  {
    printf("Error: You should provide string to decipher\n");
    return 1;
  }

  if (check_buf_is_empty(key.cipher_key_bytes, sizeof(key.cipher_key_bytes)))
  {
    srand(time(NULL));
    printf("Warning: key wasn't specified. Generating new key...\n");
    generate_arr(key.cipher_key_bytes, sizeof(key.cipher_key_bytes));

    printf("Key: ");
    print_arr(key.cipher_key_bytes, sizeof(key.cipher_key_bytes));
  }

  generate_iv_if_needed_with_print(&cipher_args);

  if (input_file)
  {
    ssize_t bytes_ctr;

    fseek(input_file, 0UL, SEEK_END);
    text.len_bytes = ftell(input_file);
    rewind(input_file);
    text.data.text_chars = malloc(text.len_bytes + BLOCK_SIZE_BYTES);

    while((bytes_ctr = read(fileno(input_file), text.data.text_chars, text.len_bytes)))
    {
      text.len_bytes = bytes_ctr;
      if (cipher || (!decipher && !cipher))
      {
        cipher_text(&text, key, cipher_args, false);
      }
      else if (decipher)
      {
        cipher_text(&text, key, cipher_args, true);
      }

      if(output_file)
      {
        write(fileno(output_file), text.data.text_chars, text.len_bytes);
      }
      else
      {
        print_arr(text.data.text_chars, text.len_bytes);
      }

      if (cipher && decipher)
      {
        cipher_text(&text, key, cipher_args, true);

        printf("Deciphered text: ");
        print_arr(text.data.text_chars, text.len_bytes);
      }
    }

    if (output_file)
    {
      fclose(output_file);
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

  print_arr(text.data.text_chars, text.len_bytes);

  printf("Key: ");
  print_arr(key.cipher_key_bytes, sizeof(key));

  if (cipher || (!cipher && !decipher))
  {
    cipher_text(&text, key, cipher_args, false);
  }
  else if (decipher)
  {
    cipher_text(&text, key, cipher_args, true);
  }


  if (output_file)
  {
    fwrite(text.data.text_chars, text.len_bytes, 1, output_file);
    fclose(output_file);
  }
  else
  {
    print_arr(text.data.text_chars, text.len_bytes);
  }

  if (decipher && cipher)
  {
    cipher_text(&text, key, cipher_args, true);

    printf("Deciphered text as hex: ");
    print_arr(text.data.text_chars, text.len_bytes);

    printf("Deciphered text as ASCII: ");
    print_arr_ascii(text.data.text_chars, text.len_bytes);
  }

  free(text.data.text_blocks);
  return 0;
}