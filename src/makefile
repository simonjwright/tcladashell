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
	gnatmake $(GARGS) hello_world -cargs $(CARGS) -bargs $(BARGS) -largs $(LARGS)

# Build Tcl and Tk libraries (Windows only)
#------------------------------------------
../src/libtcl83.a : $(TCLHOME)/bin/tcl83.dll
	dll2def $(TCLHOME)/bin/tcl83.dll > tcl83.def
	dlltool --def tcl83.def --dllname tcl83.dll --output-lib $@

../src/libtk83.a : $(TCLHOME)/bin/tk83.dll
	dll2def $(TCLHOME)/bin/tk83.dll > tk83.def
	dlltool --def tk83.def --dllname tk83.dll --output-lib $@

windows : tclmacro.o tkmacro.o ../src/libtcl83.a ../src/libtk83.a

test :
	./hello_world

clean :
	@$(TCLSH) ../bin/clean.tcl hello_world *.def *.a
