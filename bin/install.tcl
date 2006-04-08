#----------------------------------------------------------------
#
# NAME:		install.tcl
#
# ABSTRACT:	This Tcl script installs files into a target
#		directory.  It only copies files which don't
#		exist or are older in the target directory.
#
#		It was written so that a common command can be
#		executed to install TASH files regardless of
#		whether this is a Windows or Unix platform.
#
proc usage {} {
   puts stderr "usage: tclsh install.tcl \[-v\] \[-s\] \[-x exclude-regexp\]\
      file... target-directory"
   puts stderr "   -v                : verify; don't install files"
   puts stderr "   -s                : silent"
   puts stderr "   -x exclude-regexp : exclude files whose names match"
   puts stderr "                       this regular expression"
   puts stderr "   file...           : names of files or directories"
   puts stderr "                       to be installed"
   puts stderr "   target-directory  : directory in which to install"
}
#
#----------------------------------------------------------------

proc copyFile {source target verify silent exclude} {
   if { [string compare $exclude ""] != 0 && [regexp $exclude $source] } {
      return
   }
   if [file isdirectory $source] {
      copyDir $source $target $verify $silent $exclude
      return
   }
   set sourceFile $source

   # build full target file name
   #----------------------------
   if [file isdirectory $target] {
      set name [file tail $sourceFile]
      set targetFile [file join $target $name]
   } else {
      set targetFile $target
   }

   if [file exists $targetFile] {

      # file already exists, so check last modified time
      #-------------------------------------------------
      set sourceFileTime [file mtime $sourceFile]
      set targetFileTime [file mtime $targetFile]
      if {$sourceFileTime > $targetFileTime} {
	 if $verify {
            puts stdout "need to update $targetFile"
         } else {
	    if { ! $silent } {
	       puts stdout "updating $targetFile"
            }
	    if [catch {file copy -force $sourceFile $targetFile} error] {
	       puts stderr "   $error"
            }
	 }
      }

   } else {

      # file doesn't exist, so copy it
      #-------------------------------
      if $verify {
         puts stdout "need to install $targetFile"
      } else {
	 if { ! $silent } {
	    puts stdout "installing $targetFile"
	 }
         if [catch {file copy -force $sourceFile $targetFile} error] {
	    puts stderr "   $error"
         }
      }
   }
}

proc copyDir {sourceDir targetDir verify silent exclude} {
   if [regexp $exclude $sourceDir] {
      return
   }
   set sourceTail [file tail $sourceDir]
   set targetTail [file tail $targetDir]

   if {[string compare $sourceTail $targetTail] == 0} {
      if [file isdirectory $targetDir] {
         # target directory already exists and is a directory,
         # so just copy the files in $sourceDir
         #----------------------------------------------------
         if { ! [catch {glob [file join $sourceDir *]} files] } {
            foreach file $files {
               copyFile $file $targetDir $verify $silent $exclude
            }
        }
      } elseif [file exists $targetDir] {
         # target is not a directory
         #--------------------------
         puts stderr "$targetDir is not a directory"
      } else {
         # target directory does not exist, so create it
         #----------------------------------------------
         if $verify {
            puts stdout "need to create $targetDir"
         } else {
	    if { ! $silent } {
	       puts stdout "creating $targetDir"
	    }
            if [catch {file mkdir $targetDir} error] {
               puts stderr "   $error"
            } else {
               copyDir $sourceDir $targetDir $verify $silent $exclude
	    }
         }
      }
   } else {
      copyDir $sourceDir [file join $targetDir $sourceTail] $verify \
         $silent $exclude
   }
}


# show usage if no arguments
#---------------------------
if {[llength $argv] < 2} {
   usage
   exit
}

# get target directory and remove it from command line arguments
#---------------------------------------------------------------
set target [lindex $argv end]
set argv   [lreplace $argv end end]

# check for verify (-v) command line argument
#--------------------------------------------
set pos [lsearch -exact $argv "-v"]
if {$pos >= 0} {
   set verify 1
   set argv [lreplace $argv $pos $pos]
} else {
   set verify 0
}

# check for silent (-s) command line argument
#--------------------------------------------
set pos [lsearch -exact $argv "-s"]
if {$pos >= 0} {
   set silent 1
   set argv [lreplace $argv $pos $pos]
} else {
   set silent 0
}

# check for exclude-regexp (-x) command line argument
#----------------------------------------------------
set pos [lsearch -exact $argv "-x"]
if {$pos >= 0} {
   set excludepos [expr $pos + 1]
   set exclude [lindex $argv $excludepos]
   set argv [lreplace $argv $pos $excludepos]
} else {
   set exclude ""
}

if $verify {
   puts stdout "Verify:  $verify"
   puts stdout "Silent:  $silent"
   puts stdout "Exclude: $exclude"
   puts stdout "Files:   $argv"
   puts stdout "Target:  $target"
}

# check if target directory really is a directory
#------------------------------------------------
if { ! [file exists $target] } {
   file mkdir $target
} elseif { ! [file isdirectory $target] } {
   puts stderr "Error: target directory, $target, is a file!"
   exit
}


# copy the files
#---------------
foreach file [eval glob $argv] {
   copyFile $file $target $verify $silent $exclude
}

exit

