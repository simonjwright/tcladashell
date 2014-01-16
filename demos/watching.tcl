#!/usr/bin/tcl

# Created 2014 by Simon Wright <simon@pushface.org> from
# http://www.wellho.net/forum/The-Tcl-programming-language/Tracing-a-variable-in-Tcl.html
#
# This unit is free software; you can redistribute it and/or modify it
# as you wish. This unit is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# This program (with watching.adb) demonstrates the use of
# Tcl_SetVar[2]() to get an Ada-domain value back into the Tcl
# domain without the use of polling.

proc watch {varname key op} {
        if {$key != ""} {
                set varname ${varname}($key)
                }
        upvar $varname var
        puts "$varname is $var (operation $op)"
        }

trace variable tellback w watch
trace variable value w watch

puts -nonewline "Give me a big number: "
flush stdout

gets stdin value

while {$value > 100} {
    set value [expr $value / 2 - 2]
    puts "square $value returns [square $value]"
}

puts "final value is $value"
