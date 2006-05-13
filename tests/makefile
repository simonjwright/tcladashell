######################################################################
#
# This makefile builds and runs the test programs.
#
# To check for memory leaks (with gnatmem):
#
# 1) add "-g" debugging option to CARGS macro (in makeconf file,
#    or make a private one here).
#
# 2) execute with gnatmem:
#
#    make test GNATMEM=gnatmem
#
######################################################################

all::

include ../makeconf

# build directory, needed by tash.gpr
all:: .build
.build:
	mkdir $@

PROGRAMS = \
        compare \
        tashtest \
        test_lists \
        test_format \
        test_regexp \
        test_arrays \
        test_file \
        test_file_io \
        test_system \
        test_platform

all:: $(PROGRAMS)

# Compile and link all programs
#------------------------------
$(PROGRAMS): *.ads *.adb ../makeconf
	gnatmake $(GARGS) $@ -cargs $(CARGS) $(AARGS) -bargs $(BARGS) -largs $(LARGS)

VERBOSE =
GNATMEM =

test: compare tash lists regexp format arrays \
       file file_io system platform

tash:
	@ $(GNATMEM) ./tashtest tashtest.tcl

lists: 
	@ $(GNATMEM) ./test_lists $(VERBOSE)

regexp:
	@ $(GNATMEM) ./test_regexp $(VERBOSE)

format:
	@ $(GNATMEM) ./test_format $(VERBOSE)

arrays:
	@ $(GNATMEM) ./test_arrays $(VERBOSE)

old-arrays:
	$(GNATMEM) ./test_arrays $(VERBOSE) > test_arrays.out
	./compare test_arrays.out test_arrays.expected-output
	type test_arrays.out

file:
	@ $(GNATMEM) ./test_file $(VERBOSE)

file_io:
	@ $(GNATMEM) ./test_file_io $(VERBOSE)

system:
	@ $(GNATMEM) ./test_system $(VERBOSE)

platform:
	@ $(GNATMEM) ./test_platform $(VERBOSE)

lib:
	@ make -C ../src

clean:
	@$(TCLSH) ../bin/clean.tcl $(PROGRAMS) make.out
