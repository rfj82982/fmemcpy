### Makefile
#
# SPDX-License-Identifier: BSD-3-Clause

CC ?= gcc
FC ?= gfortran

FFLAGS = -g -std="f2018"
FFLAGS += -Wall -Wpedantic -Werror

CFLAGS = -g
CFLAGS += -Wall -Wpedantic -Werror

all: Fbuild

Fbuild: Cbuild fmemcpy.o
	$(FC) $(FFLAGS) test.f90 -o test fmemcpy.o cmemcpy.o

fmemcpy.o: fmemcpy.f90
	$(FC) $(FFLAGS) -c fmemcpy.f90 -o fmemcpy.o

Cbuild: cmemcpy.o

cmemcpy.o: cmemcpy.c
	$(CC) $(CFLAGS) -c cmemcpy.c -o cmemcpy.o

clean:
	rm -f *.o *.mod
