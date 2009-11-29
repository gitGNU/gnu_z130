## Makefile - Hand-writed Makefile for Zirrux 130
## Copyright (C) 2009 Mario Castelan Castro
##
## This file is part of Zirrux 130
##
## Zirrux 130 is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Zirrux 130 is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Zirrux 130.  If not, see <http://www.gnu.org/licenses/>.
##

CC=gcc
CFLAGS=-ggdb

all: tigersum

clean:
	rm -r *.o tigersum

tiger_cmp_generic.o: tiger_cmp_generic.c tiger_cmp.h
	$(CC) $(CFLAGS) -c tiger_cmp_generic.c

tiger_cmp_generic_core.c: gen_tiger_cmp_generic.lisp tiger_cmp.h
	rm -f tiger_cmp_generic_core.c
	sbcl --load gen_tiger_cmp_generic.lisp tiger_cmp_generic_core.c

tiger_cmp_generic_core.o: tiger_cmp_generic_core.c tiger_cmp.h
	$(CC) $(CFLAGS) -c tiger_cmp_generic_core.c

tiger_padding.o: tiger_padding.c tiger_padding.h
	$(CC) $(CFLAGS) -c tiger_padding.c

tigersum.o: tigersum.c tiger_cmp.h tiger_padding.h
	$(CC) $(CFLAGS) -c tigersum.c

tigersum: tigersum.o tiger_cmp_generic.o tiger_cmp_generic_core.o tiger_padding.o
	$(CC) $(CFLAGS) -o tigersum tigersum.o tiger_cmp_generic.o tiger_padding.o tiger_cmp_generic_core.o
