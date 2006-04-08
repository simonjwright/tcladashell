######################################################################
#
# This makefile builds and runs the application programs.
#
######################################################################

include ../makeconf

PROGRAMS = plang

all : $(PROGRAMS)

$(PROGRAMS) : *.adb ../makeconf
	gnatmake -A../lib $@ -cargs $(CC_FLAGS) \
	   -largs $(ALL_LINK_SWITCHES)

test : plang
	./plang ../demos/* /dev/null

lib :
	@ cd ../src ; make all install

clean :
	@$(TCLSH) ../bin/clean.tcl $(PROGRAMS)
