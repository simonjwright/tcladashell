######################################################################
#
# This makefile builds and runs the application programs.
#
######################################################################

all::

include ../makeconf

# build directory, needed by tash_apps.gpr
all:: .build
.build:
	mkdir $@

PROGRAMS = plang

all:: $(PROGRAMS)

$(PROGRAMS): *.adb ../makeconf
	gnatmake -Ptash_apps

test: plang
	./plang ../demos/*

lib:
	@ cd ../src ; make all install

clean:
	@$(TCLSH) ../bin/clean.tcl $(PROGRAMS)
