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

# Builds tcl_record_sizes.ads.

# Naming the executable tcl_record_sizes.exe means we don't need to
# worry whether we're running on Windows.

TCLSH = tclsh
ifeq (, $(shell command -v $(TCLSH) 2>/dev/null))
TCLSH = tclsh8.6
endif

all:: tcl_record_sizes.ads

tcl_record_sizes.ads: tcl_record_sizes.exe
	./$< >$@

tcl_record_sizes.exe: tcl_record_sizes.c
	gprbuild -P tcl_record_sizes.gpr -o $@

tcl_record_sizes.c: tcl_record_sizes.tcl
	$(TCLSH) $< >$@
