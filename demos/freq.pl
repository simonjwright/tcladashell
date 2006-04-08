#--------------------------------------------------------------------
#
# freq.pl --
#    This program reads lines from standard input and counts the number
#    of occurrences of each unique line.  Frequency counts are written
#    to standard out.  This Perl version was written after both the
#    Tcl and Ada versions because I wanted to add another timing
#    benchmark.  Since Perl is one of the faster scripting languages,
#    I thought it would be interesting to see how it does against
#    the Tcl bytecode compiler and Ada 
#
# Copyright (c) 1998 Terry J. Westley
#
# See the file "license.htm" for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#--------------------------------------------------------------------

# read lines from standard input until end of file encountered,
# incrementing frequency count for each line
#-------------------------------------------------------------
while (<>) {
   chop;
   $Freq{$_}++;
}

# iterate through every item and print it and its frequency count
#----------------------------------------------------------------
foreach $item (keys (%Freq)) {
   print "$item $Freq{$item}\n";
}
