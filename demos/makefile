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

# This makefile builds and runs the demo programs.

include ../makeconf

TCL_PROGRAMS = \
freq$(EXE) \
futurevalue$(EXE) \
hello_world$(EXE) \
tashell$(EXE) \
timer$(EXE) \
twashell$(EXE) \
wordify$(EXE)

TASH_PROGRAMS = \
freq2$(EXE)

all::
	[ -d .build ] || mkdir .build
	gnatmake -P tash_demos

test::
	./timer

test::
	./futurevalue

test::
	./tashell testfreq.tcl ../include words

test::
	./freq2 < words > freq2.unsorted

profile:
	./freq < words > freq.unsorted
	gprof freq gmon.out > freq.gprof

profile2:
	./freq2 < words > freq2.unsorted
	gprof freq2 gmon.out > freq2.gprof

lib:
	@ make -C ../src

# Distribution: expect DIST to be defined as the place beneath which
# the demos directory is to appear

SRC = \
makefile \
tash_demos.gpr \
freq.tcl \
futurevalue.tcl \
testfreq.tcl \
timer.tcl \
freq.adb \
freq2.adb \
futurevalue.adb \
hello_world.adb \
tashapp.adb \
tashapp.ads \
tashell.adb \
timer.adb \
twashapp.adb \
twashapp.ads \
twashell.adb \
wordify.adb

DEMOS = $(DIST)/demos

dist:
	-rm -rf $(DEMOS)
	mkdir $(DEMOS)
	cp $(SRC) $(DEMOS)/

clean:
	-rm -rf .build *~ $(TCL_PROGRAMS) $(TASH_PROGRAMS) 

