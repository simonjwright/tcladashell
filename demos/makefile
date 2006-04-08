######################################################################
#
# This makefile builds and runs the demo programs.
#
######################################################################

include ../makeconf

PROGRAMS = tashell \
           twashell \
           freq \
           timer \
           futurevalue \
           wordify \
           compare

all : $(PROGRAMS)

$(PROGRAMS) : *.adb ../makeconf
	gnatmake -A../lib $@ -cargs $(CC_FLAGS) \
	   -largs $(ALL_LINK_SWITCHES)

test : tashell freq compare wordify timer futurevalue
	./timer
	./futurevalue
	./tashell testfreq.tcl ../lib words

lib :
	@ cd ../src ; make all install

clean :
	@$(TCLSH) ../bin/clean.tcl words freq.*.out $(PROGRAMS)
