#!/bin/sh
#\
exec wish $0 $@

#------------------------------------------------
#
# timer.tcl --
#    This script provides a simple stop watch timer facility.
#    It was adapted from the demo program distributed with Tk.
#
#    Execute it in any of several ways:
#       1) wish -f timer.tcl
#       2) twash timer.tcl
#       3) ./timer.tcl
#
# Copyright (c) 1996-1997 Terry J. Westley
#
# See the file "license.htm" for information on usage and
# redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
#
#------------------------------------------------

proc Update {} {
    global seconds hundredths
    .counter config -text [format "%d.%02d" $seconds $hundredths]
}

proc tick {} {
    global seconds hundredths stopped
    if $stopped return
    after 50 tick
    set hundredths [expr $hundredths+5]
    if {$hundredths >= 100} {
	set hundredths 0
	set seconds [expr $seconds+1]
    }
    Update
}

proc Start {} {
    global stopped
    if $stopped {
	set stopped 0
        .stop config -text Stop -command Stop
	tick
    }
}

proc Reset {} {
    global seconds hundredths stopped
    set seconds 0
    set hundredths 0
    set stopped 1
    Update
}

proc Stop {} {
    global stopped
    set stopped 1
    .stop config -text Reset -command Reset
}

label .counter -text 0.00 -relief raised -width 10
pack .counter -side bottom -fill both

button .start -text Start -command Start
pack .start -side left -fill both -expand yes

button .stop -text Reset -command Reset
pack .stop -side left -fill both -expand yes

bind . <Control-c> {destroy .;exit}
bind . <Control-q> {destroy .;exit}

Reset

focus .
