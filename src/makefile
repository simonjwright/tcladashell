######################################################################
#
# This makefile builds the TASH library for either Unix or Windows.
# To build correctly, you must have previously run the setup program,
# setup.tcl, in the main TASH directory.  This will create the makeconf
# file included below.
#
# Build TASH by simply executing "make" in this directory.  Then, you
# can execute "make test" to run a simple "hello world" program to
# verify that everything built correcly.
#
# Next, cd to ../tests and run "make all test" to build and run more
# comprehensive TASH tests.
#
# To build and run the demos, cd to ../demos and run "make all test".
#
# To build and run the sample application programs, cd to ../apps and
# run "make all test" again.
#
######################################################################

include ../makeconf

all : hello_world

# Compile C files
#----------------
tclmacro.o : tclmacro.c
	$(CC) -c $(CARGS) $(TCL_INCLUDE) $?

tkmacro.o : tkmacro.c
	$(CC) -c $(CARGS) $(TCL_INCLUDE) $(X11_INCLUDE) $?

# Build Hello_World program
#--------------------------
hello_world : $(TCL_LIBRARY) $(TK_LIBRARY) tclmacro.o tkmacro.o ../makeconf
	gnatmake $(GARGS) hello_world -cargs $(CARGS) $(AARGS) -bargs $(BARGS) -largs $(LARGS)

# Build Tcl and Tk libraries (Windows only)
#------------------------------------------
../src/libtcl84.a : $(TCLHOME)/bin/tcl84.dll
	dll2def $(TCLHOME)/bin/tcl84.dll > tcl84.def
	dlltool --def tcl84.def --dllname tcl84.dll --output-lib $@

../src/libtk84.a : $(TCLHOME)/bin/tk84.dll
	dll2def $(TCLHOME)/bin/tk84.dll > tk84.def
	dlltool --def tk84.def --dllname tk84.dll --output-lib $@

windows : tclmacro.o tkmacro.o ../src/libtcl84.a ../src/libtk84.a

test :
	./hello_world

compile :
	gnatmake $(GARGS) hello_world -cargs $(CARGS) $(AARGS)

clean :
	@$(TCLSH) ../bin/clean.tcl hello_world *.def *.a make.out
