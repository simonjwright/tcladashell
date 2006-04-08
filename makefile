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
	cd src   ; make $@
	cd tests ; make $@
	cd demos ; make $@
	cd apps  ; make $@

clean : 
	@ $(TCLSH) bin/clean.tcl . *.htm COPYING tash.css \
	   docs src bin lib \* images \*
	@ cd src   ; make clean
	@ cd tests ; make clean
	@ cd demos ; make clean
	@ cd apps  ; make clean
