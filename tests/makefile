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

SIMPLE_PROGRAMS = \
        test_arrays \
        test_file \
        test_file_io \
	test_floats \
        test_format \
	test_integers \
        test_lists \
        test_platform \
        test_regexp \
	test_strings \
        test_system

# Compile and link all programs
#------------------------------
all::
	gnatmake -P tash_tests -p

VERBOSE =
GNATMEM =

test::
	cp compare.adb test_compare.ada
	$(GNATMEM) ./compare compare.adb test_compare.ada
	@echo "./compare compare.adb tash-arrays-test.adb"
	@($(GNATMEM) ./compare compare.adb tash-arrays-test.adb && \
	  echo "FAILED: comparison didn't fail!"; \
	  true)

test::
	$(GNATMEM) ./tashtest tashtest.tcl

test::
	for p in $(SIMPLE_PROGRAMS); do \
	  echo ./$$p; \
	  $(GNATMEM) ./$$p $(VERBOSE); \
	done

# Don't understand this
old-arrays:
	$(GNATMEM) ./test_arrays $(VERBOSE) > test_arrays.out
	./compare test_arrays.out test_arrays.expected-output
	type test_arrays.out

lib:
	@ make -C ../src

# TBD
clean:

.PHONY: force
