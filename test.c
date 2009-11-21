/* This is a file is for test the padding methor for Tiger.
 *
 * I, Mario Castelán Castro, the author of this file claim no
 * Copyright on it and put it on the public domain
 */
#include <stdio.h>
#include <stdint.h>
#include "tiger_padding.h"
#include "tiger_cmp.h"

int
main(void)
{
  char buffer[64];
  uint64_t hash[3];
  hash[0] = TIGER_INITIAL_A;
  hash[1] = TIGER_INITIAL_B;
  hash[2] = TIGER_INITIAL_C;
  
  buffer[0] = 'a';
  buffer[1] = 1;
  int i;
  for (i = 2; i < 64; i++)
    {
      buffer[i] = 0;
    }
  buffer[56] = 8;
  ntiger_compress(hash, buffer);
  printf("%llx %llx %llx\n", hash[0], hash[1], hash[2]);
  return 0;
}
