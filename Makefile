### Makefile
#
# SPDX-License-Identifier: BSD-3-Clause

CC ?= gcc
FC ?= gfortran

FFLAGS = -g -std="f2018"
FFLAGS += -Wall -Wpedantic -Werror

CFLAGS = -g
CFLAGS += -Wall -Wpedantic -Werror

SRCDIR = src
INCDIR = include
OBJDIR = obj
TESTDIR = tests

FFLAGS += -J$(INCDIR)

all: Fbuild

test: Fbuild
	$(FC) $(FFLAGS) $(TESTDIR)/test.f90 -o test $(OBJDIR)/fmemcpy.o $(OBJDIR)/cmemcpy.o

Fbuild: Cbuild $(OBJDIR)/fmemcpy.o

$(OBJDIR)/fmemcpy.o: $(SRCDIR)/fmemcpy.f90
	$(FC) $(FFLAGS) -c $(SRCDIR)/fmemcpy.f90 -o $(OBJDIR)/fmemcpy.o

Cbuild: $(OBJDIR)/cmemcpy.o

$(OBJDIR)/cmemcpy.o: $(SRCDIR)/cmemcpy.c
	$(CC) $(CFLAGS) -c $(SRCDIR)/cmemcpy.c -o $(OBJDIR)/cmemcpy.o

clean:
	rm -f $(OBJDIR)/*.o $(INCDIR)/*.mod
