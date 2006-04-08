######################################################################
#
# This makefile builds and runs the test programs.
#
# To check for memory leaks (with gnatmem):
#
# 1) add "-g" debugging option to CC_FLAGS macro (in makeconf file,
#    or make a private one here).
#
# 2) execute with gnatmem:
#
#    make test GNATMEM=gnatmem
#
######################################################################

include ../makeconf

PROGRAMS = \
        tashtest \
	test_strings \
        test_integers \
        test_floats \
        test_lists \
        test_format \
        test_regexp

all : $(PROGRAMS)

# Compile and link all programs
#------------------------------
$(PROGRAMS) : *.ads *.adb ../lib/*.ads ../lib/*.adb ../makeconf
	gnatmake -A../lib $@ -cargs $(CC_FLAGS) \
	   -largs $(ALL_LINK_SWITCHES)

VERBOSE =
GNATMEM =

test : 
	@ $(GNATMEM) ./tashtest tashtest.tcl
	@ $(GNATMEM) ./test_strings $(VERBOSE)
	@ $(GNATMEM) ./test_integers $(VERBOSE)
	@ $(GNATMEM) ./test_floats $(VERBOSE)
	@ $(GNATMEM) ./test_lists $(VERBOSE)
	@ $(GNATMEM) ./test_regexp $(VERBOSE)
	@ $(GNATMEM) ./test_format $(VERBOSE)

vtest :
	$(MAKE) test VERBOSE=-verbose

WHAT = lists

gdb : test_$(WHAT)
	@ gdb ./test_$(WHAT) &

lib :
	@ cd ../src ; make

clean :
	@$(TCLSH) ../bin/clean.tcl $(PROGRAMS)

