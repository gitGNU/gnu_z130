/* tiger_padding - Interface to the padder
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

#ifndef _TIGER_PADDING_H
#define _TIGER_PADDING_H

#include <stdint.h>

/* TODO: Provide documentation for this */
int ntiger_finish(uint64_t *, char *, unsigned int, unsigned int);

#endif /* _TIGER_PADDING_H */
