#------------------------------------------------
#
# NAME:		clean.tcl
#
# ABSTRACT:	This Tcl script cleans up after a build.
#		It was written so that a common command can be
#		executed to clean up after a make regardless of
#		whether this is a Windows or Unix platform.
#
# USAGE:	tclsh clean.tcl [ file | directory ] ...
#
# DESCRIPTION:	Deletes all object files (*.o), Ada library
#		files and other GNAT artifacts (*.ali, b_*.c),
#		various temporary editor files (*~ .*~ #* .#*),
#		executable files (*.exe), core files, and all
#		files specified in command line arguments.
#
#		If first argument is a directory, changes to
#		the directory before deleting files.  For any
#		subsequent directories, changes to the it
#		relative to original current working directory
#		and deletes files there.
#
#------------------------------------------------

# Procedure compares two strings, returns 1 if equal, 0 if not
#-------------------------------------------------------------
proc cequal {left right} {
   return [expr [string compare $left $right] == 0]
}

# Procedure deletes files in current working directory
#-----------------------------------------------------
proc Clean {} {

   # get tail of directory so we can later test whether
   # we're in Windows or not
   #---------------------------------------------------
   set tail [file tail [pwd]]

   if ![catch {eval glob *.o *.ali b_*.c *~ .*~ #* .#* *.exe core} files] {

      foreach file $files {
	 if [cequal $tail "win"] {
	    # we don't want to delete tclmacro.o and tkmacro.o
	    # in Windows because not everyone will have a C
	    # compiler so they can rebuild them.
	    #-------------------------------------------------
	    if [regexp "macro.o" $file] {
	       continue
	    }
	 }

	 # delete the file
	 #----------------
	 catch {file delete $file}
      }
   }
}

set firstArg [lindex $argv 0]
if [cequal $firstArg ""] {

   # no command line arguments, but make sure
   # current directory is cleaned up
   #-----------------------------------------
   set argv [list .]

} elseif ![file isdirectory $firstArg] {

   # first command line argument is not a directory,
   # so prepend current directory onto argument list
   #------------------------------------------------
   set argv [lreplace $argv 0 0 . $firstArg]
}
   
# save original "current working directory"
#------------------------------------------
set origCWD [pwd]

# loop through each command line argument
#----------------------------------------
foreach arg $argv {

   if [file isdirectory [file join $origCWD $arg]] {
      # Command line argument is a directory, so
      # cd into it and delete files.
      #-----------------------------------------
      cd [file join $origCWD $arg]
      Clean
   } else {
      # Command line argument is a regular file, so just delete it
      #-----------------------------------------------------------
      if ![catch {eval glob $arg} files] {
	 catch {eval file delete $files}
      }
   }

}

