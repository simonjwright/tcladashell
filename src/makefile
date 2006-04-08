######################################################################
#
# This makefile builds TASH library for either Unix or Windows.
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
# To build and run the demos, cd to ../demos and run "make all test."
#
# To build and run the sample application programs, cd to ../apps and
# run "make all test" again.
#
# If you want to install the TASH library elsewhere, copy the contents
# of ../lib directory to the install directory.
#
######################################################################

include ../makeconf

all : $(TASH_LIBRARY) hello_world

# Compile C files and create the tash archive library.
# We put the .o files immediately into the tash library
# archive so that the library exists by the time
# hello_world is built.
#------------------------------------------------------
tclmacro.o : tclmacro.c
	$(CC) -c $(CC_FLAGS) $(TCL_INCLUDE) $?
	$(AR) cr $(TASH_LIBRARY) $@

tkmacro.o : tkmacro.c
	$(CC) -c $(CC_FLAGS) $(TCL_INCLUDE) $(X11_INCLUDE) $?
	$(AR) cr $(TASH_LIBRARY) $@

# Build TASH library in ../lib
#-----------------------------
$(TASH_LIBRARY) : *.ads *.adb tclmacro.o tkmacro.o ../makeconf
	gnatmake -c hello_world.adb -cargs $(CC_FLAGS)
	$(AR) cr $(TASH_LIBRARY) *.o
	$(TCLSH) ../bin/install.tcl \
	   *.ads tcl-ada.adb tcl*.ali tash*.ali chelper.ali cargv.ali \
	   ../lib

# Build Hello_World program
#--------------------------
hello_world : $(TASH_LIBRARY) $(TCL_LIBRARY) $(TK_LIBRARY) ../makeconf
	gnatmake hello_world -cargs $(CC_FLAGS) \
	   -largs $(ALL_LINK_SWITCHES)

# Build Tcl and Tk libraries for windows.
#--------------------------------------------------------
../lib/libtcl82.a : $(TCLHOME)/bin/tcl82.dll
	dll2def $(TCLHOME)/bin/tcl82.dll > tcl82.def
	dlltool --def tcl82.def --dllname $(TCLHOME)/bin/tcl82.dll \
	   --output-lib ../lib/libtcl82.a

../lib/libtk82.a : $(TCLHOME)/bin/tk82.dll
	dll2def $(TCLHOME)/bin/tk82.dll > tk82.def
	dlltool --def tk82.def --dllname $(TCLHOME)/bin/tk82.dll \
	   --output-lib ../lib/libtk82.a

test :
	./hello_world

clean :
	@$(TCLSH) ../bin/clean.tcl hello_world *.def
