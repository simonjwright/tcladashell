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

# build directory, needed by tash.gpr
all:: .build
.build:
	mkdir $@

all:: hello_world$(EXE)

# Compile C files
#----------------
tclmacro.o: tclmacro.c
	$(CC) -c $(CARGS) $(TCL_INCLUDE) $?

tkmacro.o: tkmacro.c
	$(CC) -c $(CARGS) $(TCL_INCLUDE) $(X11_INCLUDE) $?

# Build Hello_World program
#--------------------------
hello_world$(EXE): tclmacro.o tkmacro.o ../makeconf
	gnatmake -Ptash hello_world

# Build Tcl and Tk libraries (Windows only)
#------------------------------------------
libtcl84.a: $(TCLHOME)/bin/tcl84.dll
	pexports $(TCLHOME)/bin/tcl84.dll > tcl84.def
	dlltool --def tcl84.def --dllname tcl84.dll --output-lib $@

libtk84.a: $(TCLHOME)/bin/tk84.dll
	pexports $(TCLHOME)/bin/tk84.dll > tk84.def
	dlltool --def tk84.def --dllname tk84.dll --output-lib $@

windows: libtcl84.a libtk84.a

test:
	./hello_world

compile::
	gnatmake -Ptash -c
compile:: tclmacro.o tkmacro.o

clean:
	@$(TCLSH) ../bin/clean.tcl hello_world *.def *.a make.out
