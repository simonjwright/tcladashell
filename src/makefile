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
tash-integer_arrays.ads \
tash-test.adb \
cargv.ads \
tash-integer_lists.ads \
tash-test.ads \
chelper.adb \
tash-integers.adb \
tash.adb \
chelper.ads \
tash-integers.ads \
tash.ads \
tash-arrays.adb \
tash-lists.adb \
tash-arrays.ads \
tash-lists.ads \
tcl-ada.adb \
tash-file.adb \
tash-platform.adb \
tcl-ada.ads \
tash-file.ads \
tash-platform.ads \
tcl-tk-ada.adb \
tash-file_io.adb \
tash-regexp.adb \
tcl-tk-ada.ads \
tash-file_io.ads \
tash-regexp.ads \
tcl-tk.adb \
tash-float_arrays.ads \
tash-strings.adb \
tcl-tk.ads \
tash-float_lists.ads \
tash-strings.ads \
tcl.adb \
tash-floats.adb \
tash-system.adb \
tcl.ads \
tash-floats.ads \
tash-system.ads

all::	tash_options.gpr

all::
	gprmake -P build_tash_library -p
	gnatmake -P build_tash_library -p
	chmod -w lib/*.ali
	cp $(ADA_SRC) lib/

all::
	gnatmake -P hello_world -p

tash_options.gpr: ../tash_options.gpr
	cp $< $@

test:
	./hello_world
.PHONY: test

DIST = tash-$(TASH_VERSION)-$(OS)-$(shell date +%Y%m%d)

dist: all
	-rm -rf $(DIST) $(DIST).zip
	mkdir $(DIST)
	cp -R tash_options.gpr tash.gpr lib $(DIST)
	zip -r $(DIST).zip $(DIST)

clean:
	-rm -rf hello_world$(EXE)  .build_lib .build lib *~
