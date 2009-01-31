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

# This makefile builds the TASH library for either Unix or Windows.
# To build correctly, you must have previously run the setup program,
# setup.tcl, in the main TASH directory.  This will create the makeconf
# file included below.

# Build TASH by simply executing "make" in this directory.  Then, you
# can execute "make test" to run a simple "hello world" program to
# verify that everything built correcly.

# Next, cd to ../tests and run "make all test" to build and run more
# comprehensive TASH tests.

# To build and run the demos, cd to ../demos and run "make all test".

# To build and run the sample application programs, cd to ../apps and
# run "make all test" again.

all::

include ../makeconf

ADA_SRC = \
cargv.adb \
cargv.ads \
chelper.adb \
chelper.ads \
tcl-ada.adb \
tcl-ada.ads \
tcl-tk-ada.adb \
tcl-tk-ada.ads \
tcl-tk.adb \
tcl-tk.ads \
tcl.adb \
tcl.ads

ADA_SRC += tcl_record_sizes.ads

C_SRC = tclmacro.c tkmacro.c

# Tcl/Tk versions later than 8.4 don't provide the interfaces relied
# on by the Tash.* package hierarchy, so (by default) they won't be
# built.

ifeq ($(SUPPORTS_TASH),yes)

ADA_SRC += \
  tash-arrays.adb \
  tash-arrays.ads \
  tash-file.adb \
  tash-file.ads \
  tash-file_io.adb \
  tash-file_io.ads \
  tash-float_arrays.ads \
  tash-float_lists.ads \
  tash-floats.adb \
  tash-floats.ads \
  tash-integer_arrays.ads \
  tash-integer_lists.ads \
  tash-integers.adb \
  tash-integers.ads \
  tash-lists.adb \
  tash-lists.ads \
  tash-platform.adb \
  tash-platform.ads \
  tash-regexp.adb \
  tash-regexp.ads \
  tash-strings.adb \
  tash-strings.ads \
  tash-system.adb \
  tash-system.ads \
  tash.adb \
  tash.ads

C_SRC += \
  tcl_backward_compatibility_glue.c

endif

SCRIPTS = makefile build_tash_library.gpr

all:: build
.PHONY: build

build: .build_lib tcl_record_sizes.ads
	@rm -rf ../lib;  mkdir ../lib
	@rm -rf ../include;  mkdir ../include
	gprbuild -Pbuild_tash_library
	chmod -w ../lib/*.ali
	cp -p $(ADA_SRC) ../include/

.build_lib:
	mkdir $@

tcl_record_sizes.ads: tcl_record_sizes$(EXE)
	./$< >$@

tcl_record_sizes$(EXE): tcl_record_sizes.c
	$(CC) $(TCL_INCLUDE) -o $@ $<

tcl_record_sizes.c: tcl_record_sizes.tcl
	tclsh $< >$@

DATE = $(shell date +%Y%m%d)
DIST = tash-$(TASH_VERSION)-$(OS)-$(DATE)
SRC-DIST = tash-$(TASH_VERSION)-src-$(DATE)

SUBDIRS = ../apps ../demos ../tests

# To distribute source ...
src-dist:
	-rm -rf $(SRC-DIST) $(SRC-DIST).zip
	mkdir $(SRC-DIST)
	cp -R ../COPYING ../README ../setup.tcl ../tash.gpr $(SRC-DIST)/
	mkdir $(SRC-DIST)/src
	cp $(ADA_SRC) $(C_SRC) $(SCRIPTS) $(SRC-DIST)/src/ 
	for s in $(SUBDIRS); do \
	  $(MAKE) -C $$s DIST=../src/$(SRC-DIST) dist; \
	done
	zip -r $(SRC-DIST).zip $(SRC-DIST)

# To distribute binary ...
dist: all
	-rm -rf $(DIST) $(DIST).zip
	mkdir $(DIST)
	cp -R ../COPYING ../README ../makeconf ../tash_options.gpr \
	  ../tash.gpr ../lib ../include $(DIST)/
	for s in $(SUBDIRS); do \
	  $(MAKE) -C $$s DIST=../src/$(DIST) dist; \
	done
	zip -r $(DIST).zip $(DIST)

clean:
	-rm -rf .build_lib *~

.PHONY: src-dist dist
