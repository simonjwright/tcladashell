######################################################################
#
# This makefile builds and runs the application programs.
#
######################################################################

include ../makeconf

PROGRAMS = plang

all : $(PROGRAMS)

$(PROGRAMS) : *.adb ../makeconf
	gnatmake $(GARGS) $@ -cargs $(CARGS) -bargs $(BARGS) -largs $(LARGS)

test : plang
	./plang ../demos/*

lib :
	@ cd ../src ; make all install

clean :
	@$(TCLSH) ../bin/clean.tcl $(PROGRAMS)
