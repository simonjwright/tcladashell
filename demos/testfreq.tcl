#------------------------------------------------
#
# testfreq.tcl --
#    This is a Tcl script which tests the freq program.
#
#------------------------------------------------

proc cequal {left right} {
   return [expr [string compare $left $right] == 0]
}

proc lempty {string} {
   return [expr [string length $string] == 0]
}

# Get command line arguments
#---------------------------
set source_dir [lindex $argv 0]
set words      [lindex $argv 1]

set pwd        [pwd]
set wordify    [file join $pwd        wordify]
set input_file [file join $source_dir tcl.ads]
set tashell    [file join $pwd        tashell]
set freq       [file join $pwd        freq]
set compare    [file join $pwd        compare]

puts stdout "Testing freq"

# Delete output files
#--------------------
puts stdout "   Deleting output files..."
file delete $words freq.tcl.out freq.ada.out freq.perl.out

# Prepare file of words
#----------------------
puts stdout "   Preparing file of words..."
exec $wordify < $input_file > $words

# Execute Tcl script version
#---------------------------
puts stdout "   Executing Tcl script version..."
set tcl_time [time "exec $tashell freq.tcl < $words | sort > freq.tcl.out" 1]

# Execute Ada version
#--------------------
puts stdout "   Executing Ada version..."
set ada_time [time "exec $freq < $words | sort > freq.ada.out" 1]

# Execute Perl version
#---------------------
puts stdout "   Executing Perl version..."
if [catch {
   time "exec perl freq.pl < $words | sort > freq.perl.out" 1
} perl_time] {
    puts stdout "      $perl_time"
    unset perl_time
}

# Compare outputs of both versions
#---------------------------------
puts stdout "   Comparing outputs..."
catch {exec $compare freq.tcl.out freq.ada.out} diff
catch {exec $compare freq.tcl.out freq.perl.out} perldiff
if {[lempty $diff] && [lempty $perldiff]} {
   puts stdout "Freq test PASSED"
} elseif { ! [lempty $diff] } {
   puts stdout "Tcl Freq test FAILED: $diff"
} elseif { ! [lempty $perldiff] } {
   puts stdout "Perl Freq test FAILED: $perldiff"
}

# Display timing results
#-----------------------
set tcl_time [expr [lindex $tcl_time 0] / 1000000.0]
puts stdout [format "   Elapsed time for executing Tcl version:  \
   %6.2f seconds" $tcl_time]

set ada_time [expr [lindex $ada_time 0] / 1000000.0]
puts stdout [format "   Elapsed time for executing Ada version:  \
   %6.2f seconds" $ada_time]

if [info exists perl_time] {
   set perl_time [expr [lindex $perl_time 0] / 1000000.0]
   puts stdout [format "   Elapsed time for executing Perl version: \
      %6.2f seconds" $perl_time]
}
