#!/bin/sh
#\
exec wish $0 $@

#------------------------------------------------
#
# futurevalue.tcl --
#    This program the use of Tcl/Tk to implement a simple GUI for
#    computing Future Value of a series of fixed monthly investments.
#
# Copyright (c) 1997 Terry J. Westley
#
# See the file "license.htm" for information on usage and
# redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
#
#------------------------------------------------

proc computeFutureValue {} {
   set msa [.msa.entry get]
   if {$msa == "" || $msa < 0} return

   set int [.int.entry get]
   if {$int == "" || $int < 0} return

   set yrs [.yrs.entry get]
   if {$yrs == "" || $yrs < 0} return

   set mint [expr ($int) / 1200.0]
   set mos  [expr ($yrs) * 12]
   set fv [format "%7.2f" [expr ($msa) * (pow(1+$mint,$mos) - 1)/$mint]]
   .fv.result configure -text $fv
}

wm title . "Future Value of Savings"

set form [list \
   [list msa "Monthly Savings Amount:" 100] \
   [list int "Annual Interest Rate:"   8] \
   [list yrs "Number of Years:"        30]]

# Create and initialize three widgets for:
#    Monthly Savings Amount,
#    Annual Interest Rate, and
#    Number of Years.
#----------------------------------------
foreach field $form {
   set name  [lindex $field 0]
   set label  [lindex $field 1]
   set value [lindex $field 2]
   frame .$name -bd 2
   pack  .$name -side top -fill x
   entry .$name.entry -width 20 -bg white
   pack  .$name.entry -side right
   label .$name.label -text $label
   pack  .$name.label -side right
   .$name.entry insert end $value
}

focus .msa.entry

frame  .fv -bd 2
pack   .fv -side top -fill x
label  .fv.result -width 20 -relief sunken
pack   .fv.result -side right
button .fv.button -text "Compute Future Value:" \
   -command computeFutureValue -pady 1
pack   .fv.button -side right

bind .fv.button <Return> {computeFutureValue}

