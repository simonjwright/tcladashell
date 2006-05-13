######################################################################
#
# This makefile builds and runs the demo programs.
#
######################################################################

all::

include ../makeconf

# build directory, needed by tash_demos.gpr
all:: .build
.build:
	mkdir $@

PROGRAMS = tashell \
           twashell \
           freq \
           freq2 \
           timer \
           futurevalue \
           wordify

all:: $(PROGRAMS)

$(PROGRAMS): *.adb ../makeconf
	gnatmake -Ptash_demos $@

test: test-timer test-fv

test-timer: timer
	./timer

test-fv: futurevalue
	./futurevalue

test-freq: tashell freq freq2 wordify
	./tashell testfreq.tcl ../src words

test-freq2: freq2
	./freq2 < words > freq2.unsorted

profile: freq
	./freq < words > freq.unsorted
	gprof freq gmon.out > freq.gprof

profile2: freq2
	./freq2 < words > freq2.unsorted
	gprof freq2 gmon.out > freq2.gprof

lib:
	@ cd ../src ; make all

clean:
	@$(TCLSH) ../bin/clean.tcl words freq.*.out $(PROGRAMS)
