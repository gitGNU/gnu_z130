/* tiger_padding.c
   Copyright (C) 2009 Mario Castelán Castro

   This file is part of Zirrux 130

   Zirrux 130 is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Zirrux 130 is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public Lixcense for more details.

   You should have received a copy of the GNU General Public License
   along with Zirrux 130.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <string.h>
#include "tiger_padding.h"

int ntiger_finish(uint64_t * registers,
		  char * block,
		  unsigned int block_len,
		  unsigned int total_bytes)
{
  /* We need at least 9 bytes for padding, 64 - 9 = 55 */
  if (block_len <= 55)
    {
      /* Padding fit in current block */
      block[block_len] = 1;
      memset(block + block_len + 1, 0, 64 - block_len - 8);
      ((uint64_t *) block)[7] = total_bytes << 3;
      ntiger_compress(registers, block);
      return 0;
    }
  if (block_len == 64)
    {
      /* Current block is full and a new one is required only for
	 padding */
      ntiger_compress(registers, block);
      block[0] = 1;
      /* 8 bit for length and 1 to delimite the padding. 64 - 8 - 1 =
	 55 so we set the 55 rest bytes to 0 */
      memset(block + 1, 0, 55);
      ((uint64_t *) block)[7] = total_bytes << 3;
      ntiger_compress(registers, block);
      return 1;
    }

  /* Here the current block is not full and therefore need padding but
     the minium padding do not fit in this block so we need a new
     block only for padding */
  block[block_len] = 1;
  memset(block + 1, 0, block_len - 63);
  ntiger_compress(registers, block);
  memset(block, 0, 56);
  ((uint64_t *) block)[7] = total_bytes << 3;
  ntiger_compress(registers, block);
  return 1;
}
