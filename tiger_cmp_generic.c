/* tiger_cmp_generic.c
 * Copyright (C) 2009 Mario Castelan Castro
 *
 * This file is part of Zirrux 130
 *
 * Zirrux 130 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Zirrux 130 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Zirrux 130.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdint.h>
#include "tiger_cmp.h"

void
tiger_compress(uint64_t * registers, uint64_t * keys)
{
  uint64_t buffer[8] = {keys[0],
			keys[1],
			keys[2],
			keys[3],
			keys[4],
			keys[5],
			keys[6],
			keys[7]};
  ntiger_compress(registers, buffer);
}

void
ntiger_compress_blks(uint64_t * registers, uint64_t * keys, unsigned int len)
{
  for (; len; len--)
    {
      ntiger_compress(registers, keys);
      keys += 64;
    }
}

void
tiger_compress_blks(uint64_t * registers, uint64_t * keys, unsigned int len)
{
  for (; len; len--)
    {
      tiger_compress(registers, keys);
      keys += 64;
    }
}
