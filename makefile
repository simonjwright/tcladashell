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

# This makefile builds the TASH library for either Unix or Windows. To
# build correctly, you must have previously run the setup program,
# setup.tcl, in this directory. This will create the makeconf file
# included below.

# Build and test TASH by executing "make all test" in this directory.
# Or, you can cd into each of the subdirectories, src, demos, tests,
# and apps, and execute "make all test" in each one in turn.

# If you want to install the TASH library elsewhere, copy the contents
# of ../lib directory to the install directory.

include makeconf

SUBDIRS = src tests demos apps

FOR_ALL_SUBUNITS = +for i in ${SUBDIRS}; do ${MAKE} -w -C $${i} $@ || exit 1; done

all:
	$(FOR_ALL_SUBUNITS)

test:
	$(FOR_ALL_SUBUNITS)

clean:
	$(FOR_ALL_SUBUNITS)

# RPM related variables/rules :

INSTALLROOT     := opt/tash
ARCHITECTURE     = $(shell uname -m)

rpm: all
	rm -rf ./rpm_build && mkdir -p ./rpm_build
	rpmbuild --buildroot "`pwd`/rpm_build" \
                 --define "InstallPath $(INSTALLROOT)" \
                 --define "ChangeDefault YES" \
                 --define "TashVersion $(TASH_VERSION)" \
                 --define "TashRelease $(TASH_RELEASE)" \
                 --define "_topdir `pwd`" \
                 --define "_builddir `pwd`" \
                 --define "_rpmdir %{_topdir}" \
                 --define "_sourcedir %{_topdir}" \
                 --define "_specdir %{_topdir}" \
                 --define "_srcrpmdir %{_topdir}" \
                 --define "_rpmfilename %%{NAME}-%%{VERSION}-%%{RELEASE}.$(ARCHITECTURE).rpm" \
                 -bb tash.spec

# Rules for maintaining the SourceForge web pages (will disappear when
# it's all under the Wiki).

RSYNC ?= rsync

SFUSER ?= simonjwright

upload-docs: force
	$(RSYNC) \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  web/* \
	  $(SFUSER)@shell.sourceforge.net:/home/groups/t/tc/tcladashell/htdocs/

.PHONY: force
