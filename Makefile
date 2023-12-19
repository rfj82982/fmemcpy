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

FFILES = utils fmemcpy
FSRC = $(addprefix $(SRCDIR)/,$(addsuffix .f90,$(FFILES)))
FOBJ = $(addprefix $(OBJDIR)/,$(addsuffix .o,$(FFILES)))

all: Fbuild

test: Fbuild
	$(FC) $(FFLAGS) $(TESTDIR)/test.f90 -o test $(FOBJ) $(OBJDIR)/cmemcpy.o

Fbuild: Cbuild $(FOBJ)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FFLAGS) -c $^ -o $@

Cbuild: $(OBJDIR)/cmemcpy.o

$(OBJDIR)/cmemcpy.o: $(SRCDIR)/cmemcpy.c
	$(CC) $(CFLAGS) -c $(SRCDIR)/cmemcpy.c -o $(OBJDIR)/cmemcpy.o

clean:
	rm -f $(OBJDIR)/*.o $(INCDIR)/*.mod test
