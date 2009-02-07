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
set source_dir    [lindex $argv 0]
set words         [lindex $argv 1]
set supports_tash [lindex $argv 2]
set head          [lindex $argv 3]

set pwd        [pwd]
set wordify    [file join $pwd        wordify]
set input_file [file join $source_dir tcl.ads]
set tashell    [file join $pwd        tashell]
set freq       [file join $pwd        freq]
set freq2      [file join $pwd        freq2]
set compare    [file join $pwd        .. tests compare]

puts stdout "Testing freq"

# Delete output files
#--------------------
puts stdout "   Deleting output files..."
file delete $words freq.tcl.out freq.ada.out freq.perl.out freq2.ada.out

# Prepare file of words
#----------------------
puts stdout "   Preparing file of words..."
if [lempty $head] {
    exec $wordify < $input_file > $words
} else {
    # catch PROGRAM_ERROR from wordify
    catch {exec $wordify < $input_file | head -$head > $words}
}

# Execute Tcl script version
#---------------------------
puts stdout "   Executing Tcl script version..."
set tcl_time [time "exec $tashell freq.tcl < $words > freq.unsorted" 1]
exec sort freq.unsorted > freq.tcl.out

# Execute Ada version
#--------------------
puts stdout "   Executing Ada version..."
set ada_time [time "exec $freq < $words > freq.unsorted" 1]
exec sort freq.unsorted > freq.ada.out

# Execute Ada version 2
#----------------------
if {$supports_tash == yes} {
    puts stdout "   Executing Ada version 2..."
    set ada_time2 [time "exec $freq2 < $words > freq2.unsorted" 1]
    exec sort freq.unsorted > freq2.ada.out
}

# Execute Perl version
#---------------------
puts stdout "   Executing Perl version..."
if [catch {
    set perl_time [time "exec perl freq.pl < $words > freq.unsorted" 1]
   exec sort freq.unsorted > freq.perl.out
} error] {
    puts stdout "      $error"
    set perl_time 0
}

# Compare outputs of all versions
#--------------------------------
puts stdout "   Comparing outputs..."
catch {exec $compare freq.tcl.out freq.ada.out} diff
if {$supports_tash == yes} {
    catch {exec $compare freq.tcl.out freq2.ada.out} diff2
} else {
    set diff2 ""
}
catch {exec $compare freq.tcl.out freq.perl.out} perldiff
if {[lempty $diff] && [lempty $diff2] && [lempty $perldiff]} {
   puts stdout "Freq test PASSED"
} else {
   if { ! [lempty $diff] } {
      puts stdout "Ada Freq test FAILED: $diff"
   }
   if { ! [lempty $diff2] } {
      puts stdout "Ada Freq 2 test FAILED: $diff2"
   }
   if { ! [lempty $perldiff] } {
      puts stdout "Perl Freq test FAILED: $perldiff"
   }
}

# Display timing results
#-----------------------
set tcl_time [expr [lindex $tcl_time 0] / 1000000.0]
puts stdout [format "   Elapsed time for executing Tcl version:     \
   %6.2f seconds" $tcl_time]

set ada_time [expr [lindex $ada_time 0] / 1000000.0]
puts stdout [format "   Elapsed time for executing Ada version:     \
   %6.2f seconds" $ada_time]

if {$supports_tash == yes} {
    set ada_time2 [expr [lindex $ada_time2 0] / 1000000.0]
    puts stdout [format "   Elapsed time for executing Ada version 2:   \
   %6.2f seconds" $ada_time2]
}

set perl_time [expr [lindex $perl_time 0] / 1000000.0]
puts stdout [format "   Elapsed time for executing Perl version:    \
   %6.2f seconds" $perl_time]
