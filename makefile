######################################################################
#
# This makefile builds TASH library for either Unix or Windows.
# To build correctly, you must have previously run the setup program,
# setup.tcl, in this.  This will create the makeconf file included below.
#
# Build and test TASH by executing "make all test" in this directory.
# Or, you can cd into each of the subdirectories, src, demos, tests, and
# apps, and execute "make all test" in each one in turn.
#
# If you want to install the TASH library elsewhere, copy the contents
# of ../lib directory to the install directory.
#
######################################################################

include makeconf

all test : 
	make -C src   $@
	make -C tests $@
	make -C demos $@
	make -C apps  $@

clean : 
	@ $(TCLSH) bin/clean.tcl . src tests demos apps
	make -C src   $@
	make -C tests $@
	make -C demos $@
	make -C apps  $@
