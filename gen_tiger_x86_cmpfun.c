/* gen_tiger_x86_cmpfun.h - Tiger compression function for x86 architecture for Zirrux 130
 * Copyright (C) 2009 Kevin Mas Ruiz
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
 * along with this Zirrux 130.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>

#define A (0)
#define B (8)
#define C (16)

#define LOCAL_MEMORY_SIZE   (A + B + C) // 24
#define MESSAGE_BLOCK 64 // 8 byte = 64 bits && 8 blocks
#define TOTAL_SIZE 88

void prepare_stack(void)
{
  puts("push ebp\n"
       "push esi\n"
       "push edi\n"
       "push ebx\n"
       "mov ebp, esp\n"
       "sub esp, 88");
}

void restart_stack(void)
{
  puts("mov esp, ebp\n"
       "pop ebx\n"
       "pop edi\n"
       "pop esi\n"
       "pop ebp");
}

void save_abc(void)
{
  puts("mov word [ebp], eax\n"
       "mov word [ebp - 4], ebx\n"
       "mov word [ebp - 8], ecx\n"
       "mov word [ebp - 12], edx\n"
       "mov word [ebp - 16], esi\n"
       "mov word [ebp - 20], edi");
}

void round(void *msg, int mul);
