#--------------------------------------------------------------------
#
# freq.tcl --
#    This program reads lines from standard input and counts the number
#    of occurrences of each unique line.  Frequency counts are written
#    to standard out.
#
# Copyright (c) 1995-1997 Terry J. Westley
#
# See the file "license.htm" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#--------------------------------------------------------------------

# read lines from standard input until
# end of file encountered
while {[gets stdin line] >= 0} {
   if [info exists Freq($line)] {
      # the item is already in the array,
      # so just increment its count
      incr Freq($line)
   } else {
      # the item is not in the array yet,
      # so initialize its count
      set Freq($line) 1
   }
}

# iterate through every item and print it
# and its frequency count
foreach item [array names Freq] {
   puts stdout "$item $Freq($item)"
}
