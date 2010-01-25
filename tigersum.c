/* tiger.c - CLI to compute the Tiger hash of a set of files.
   Copyright (C) 2009 Mario Castelan Castro
 
   This file is part of Zirrux 130
 
   Zirrux 130 is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
 
   Zirrux 130 is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with Zirrux 130.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <fcntl.h>
#include <errno.h>
#include "tiger_cmp.h"
#include "tiger_padding.h"
#include "version.h"

const struct option options[] =
  {
    {"version", no_argument, NULL, 'v'},
    {"progress ", optional_argument, NULL, 'p'}
  };


int
print_version(void)
{
  printf("tigersum (Zirrux 130) " Z130_VERSION "\n");
  exit(0);
}

int
atoui(char * string)
{
  int result = 0;
  while (*string)
    {
      if (*string < '0' || *string > '9')
	{
	  return -1;
	}
      result = result * 10 + *string - 48;
    }
  return result;
}

/* Compute the Tiger Hash for a file. Return status says if there is
 * any error 0 indicates all ok, 1 indicates a error and see errno for
 * details.
 */
int
hash_file(char * filename, uint64_t * hash)
{
  int fd;
  int n;
  int block_len = 0; /* Length of last block readed */
  unsigned int total_len = 0;
  unsigned char buffer[64];
  if (strcmp("-", filename))
    {
      fd = open(filename, O_RDONLY);
      if (fd == -1)
	{
	  return 1;
	}
    }
  else
    {
      fd = 0;
    }
  hash[0] = TIGER_INITIAL_A;
  hash[1] = TIGER_INITIAL_B;
  hash[2] = TIGER_INITIAL_C;
  while (1)
    {
      n = read(fd, buffer + block_len, 64);
      block_len += n;
      total_len += n;
      int i;
      if (n == 64)
	{
	  ntiger_compress(hash, buffer);
	  block_len = 0;
	}
      else if (n == 0)
	{
	  ntiger_finish(hash, buffer, block_len, total_len);
	  break;
	}
    }
  close(fd);
  return 0;
}

void
print_tiger(FILE * stream, uint64_t * registers)
{
  int i;
  for (i = 0; i < 24; i++)
    {
      fprintf(stream, "%02x", ((unsigned char *) registers)[i] & 0xFF);
    }
}

int
main(int argc, char ** argv)
{
  int c;
  int progress_bar = 0;
  if (argc == 1)
    {
      print_version();
    }
  while ((c = getopt_long(argc, argv, "p:v", options, NULL)) != -1)
    {
      switch (c)
	{
	case 'v':
	  print_version();
	case 'p':
	  if (!strcmp(optarg, "bar"))
	    {
	      progress_bar = 1;
	    }
	  else
	    {
	      fprintf(stderr, "%s: That progress indicator is not supported currently\n", argv[0]);
	      exit(1);
	    }
	  break;
	}
      
    }
  for (;optind < argc; optind++)
    {
      uint64_t hash[3] = {0x0123456789ABCDEF,
			  0xFEDCBA9876543210,
			  0xF096A5B4C3B2E187};
      int status = hash_file(argv[optind], hash);
      switch (status)
	{
	case 0:
	  print_tiger(stdout, hash);
	  printf("  %s\n", argv[optind]);
	  break;
	case 1:
	  fprintf(stderr, "%s: %s\n", argv[0], strerror(errno));
	  break;
	case 2:
	  fprintf(stderr, "%s: Internal error computing %s\n", argv[0]);
	  break;
	}
    }
  return 0;
}
