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

# This makefile builds and runs the application programs.

all::

include ../makeconf

GPRBUILD ?= gprbuild

TCL_PROGRAMS =

TASH_PROGRAMS = plang$(EXE)

# Because there are no Tcl-only programs in Apps, we have not to even
# try to build if Tash isn't supported (gprbuild tries to build the
# world if there are no mains).
ifeq ($(SUPPORTS_TASH),yes)
all::
	[ -d .build ] || mkdir .build
	$(GPRBUILD) -Ptash_apps
endif

test::

ifeq ($(SUPPORTS_TASH),yes)
test::
	./plang ../demos/*
endif

lib:
	@ make -C ../src

# Distribution: expect DIST to be defined as the place beneath which
# the apps directory is to appear

SRC = \
makefile \
tash_apps.gpr \
plang.adb

APPS = $(DIST)/apps

dist:
	-rm -rf $(APPS)
	mkdir $(APPS)
	cp $(SRC) $(APPS)/

clean:
	-rm -rf .build *~ $(TCL_PROGRAMS) $(TASH_PROGRAMS)
