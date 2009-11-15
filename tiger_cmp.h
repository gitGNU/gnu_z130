/* tiger_cmp.h - Interface for the tiger compression function
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

#ifndef _TIGER_COMPRESS_H
#define _TIGER_COMPRESS_H
#include <stdint.h>

#define TIGER_INITIAL_STATE_A = 0x0123456789ABCDEF;
#define TIGER_INITIAL_STATE_B = 0xFEDCBA9876543210;
#define TIGER_INITIAL_STATE_C = 0xF096A5B4C3B2E187;
#define TIGER_INITIAL_STATE = {0x0123456789ABCDEF, 0xFEDCBA9876543210, 0xF096A5B4C3B2E187}

void tiger_compress(uint64_t * registers, uint64_t * keys);
void ntiger_compress(uint64_t * registers, uint64_t * keys);
void tiger_compress_blks(uint64_t * registers, uint64_t * keys, unsigned int len);
void ntiger_compress_blks(uint64_t * registers, uint64_t * keys, unsigned int len);

#endif /* _TIGER_COMPRESS_H */
