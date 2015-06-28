# Copyright (C) 1997-2000 Terry J. Westley
# Copyright (C) Simon Wright <simon@pushface.org>

# This package is free software; you can redistribute it and/or
# modify it under terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version. This package is distributed in
# the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU General Public License for more
# details. You should have received a copy of the GNU General Public
# License distributed with this package; see file COPYING.  If not,
# write to the Free Software Foundation, 59 Temple Place - Suite
# 330, Boston, MA 02111-1307, USA.

# $Id$

# This makefile builds and runs the test programs.
#
# To check for memory leaks (with gnatmem):
#
# 1) add "-g" debugging option to CARGS macro (in makeconf file,
#    or make a private one here).
#
# 2) execute with gnatmem:
#
#    make test GNATMEM=gnatmem

include ../makeconf

GPRBUILD ?= gprbuild

all::

# Tcl/Tk versions later than 8.4 don't provide the interfaces relied
# on by the Tash.* package hierarchy, so (by default) they won't be
# built.

TCL_PROGRAMS = \
  compare$(EXE) \
  tashtest$(EXE)

TASH_PROGRAMS = \
  test_arrays$(EXE) \
  test_file$(EXE) \
  test_file_io$(EXE) \
  test_floats$(EXE) \
  test_format$(EXE) \
  test_integers$(EXE) \
  test_lists$(EXE) \
  test_platform$(EXE) \
  test_regexp$(EXE) \
  test_strings$(EXE) \
  test_system$(EXE)

# Compile and link all programs
#------------------------------
all::
	[ -d .build ] || mkdir .build
	$(GPRBUILD) -Ptash_tests

VERBOSE =
GNATMEM =

test::
	cp compare.adb test_compare.ada
	$(GNATMEM) ./compare compare.adb test_compare.ada
	@echo "./compare compare.adb tash-arrays-test.adb"
	@($(GNATMEM) ./compare compare.adb tashtest.adb && \
	  echo "FAILED: comparison didn't fail!"; \
	  true)

test::
	$(GNATMEM) ./tashtest tashtest.tcl

ifeq ($(SUPPORTS_TASH),yes)
test::
	for p in $(TASH_PROGRAMS); do \
	  echo ./$$p; \
	  $(GNATMEM) ./$$p $(VERBOSE); \
	done
endif

lib:
	@ make -C ../src

# Distribution: expect DIST to be defined as the place beneath which
# the tests directory is to appear.

SRC = \
  makefile \
  tash_tests.gpr \
  tashtest.tcl \
  compare.adb \
  tashtest.adb \
  testapp.adb \
  testapp.ads

SRC += \
  tash-arrays-test.adb \
  tash-arrays-test.ads \
  tash-floats-test.adb \
  tash-floats-test.ads \
  tash-integers-test.adb \
  tash-integers-test.ads \
  tash-lists-test.adb \
  tash-lists-test.ads \
  tash-strings-test.adb \
  tash-strings-test.ads \
  tash-test.adb \
  tash-test.ads \
  test_arrays.adb \
  test_file.adb \
  test_file_io.adb \
  test_floats.adb \
  test_format.adb \
  test_integers.adb \
  test_lists.adb \
  test_platform.adb \
  test_regexp.adb \
  test_strings.adb \
  test_system.adb

MATERIEL = Shortcut.txt.lnk

TESTS = $(DIST)/tests

dist:
	-rm -rf $(TESTS)
	mkdir $(TESTS)
	cp $(SRC) $(MATERIEL) $(TESTS)/

clean:
	-rm -rf .build *~ $(TCL_PROGRAMS) $(TASH_PROGRAMS)
