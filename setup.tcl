#!/bin/sh
# -*- Tcl -*- (for Emacs)
#\
exec wish $0 $@

# Copyright (C) 1997-2000 Terry J. Westley
# Copyright (C) Simon Wright <simon@pushface.org>

# This package is free software; you can redistribute it and/or
# modify it under terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version. This package is distributed in
# the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU General Public License for more
# details. You should have received a copy of the GNU General Public
# License distributed with this package; see file COPYING.  If not,
# write to the Free Software Foundation, 59 Temple Place - Suite
# 330, Boston, MA 02111-1307, USA.

# $Id$

# This is a Tcl/Tk script which helps install TASH.  It collects
# information about the environment and creates a file, makeconf,
# which is included in makefiles to customize to the local
# environment.

# The option --nogui takes the defaults.

set tash_version "8.6"
set tash_release "0"

proc cequal {left right} {
    return [expr [string compare $left $right] == 0]
}

proc lempty {string} {
    return [expr [string length $string] == 0]
}

proc setvar {name value comments} {
    global tashvar tashorder tashcomments
    set tashvar($name) $value
    set tashcomments($name) $comments
    lappend tashorder $name
}

# The Tash.* packages of TASH are not supported under Tcl > 8.4.
proc supportsTash {} {
    global tcl_version
    set majmin [split $tcl_version .]
    set maj [lindex $majmin 0]
    set min [lindex $majmin 1]
    if {$maj == 8 && $min <= 4} {
	return  "yes"
    } else {
	return  "no"
    }
}

# Write makefile macro
#---------------------
proc WriteOneMacro {f name value comments} {
    puts $f ""
    foreach line [split $comments "\n"] {
	puts $f "# [string trimleft $line]"
    }
    puts $f [format "%-18s = %s" $name $value]
}

# Create linker options package
#------------------------------
proc CreateLinkerOptions {} {
    global tashvar

    set filename [file join src tash_linker_options.ads]

    if [catch {open $filename w} f] {
	set text "Couldn't create linker options package because $f"
	tk_messageBox -icon error -message $text \
	    -parent . -title Error -type ok
	return
    }

    puts $f "package TASH_Linker_Options is"
    foreach macro [list LARGS] {
	foreach option $tashvar($macro) {
	    # substitute value of embedded macros
	    if [regexp {\$\(([^)]*)\)} $option dummy embeddedMacro] {
		regsub {\$\([^)]*\)} $option $tashvar($embeddedMacro) option
            }
            # write the option as a Linker_Options pragma
            puts $f "   pragma Linker_Options (\"$option\");"
        }
    }
    puts $f "end TASH_Linker_Options;"
    close $f
}

# Edit tcl.adb to "with" the linker options package
#--------------------------------------------------
proc EditSourceFile {} {
    global tashvar

    set pwd [pwd]
    cd src

    set errorPrefix "Couldn't edit tcl.adb to with linker options\
      package because"

    if [catch {

	if { ! [file exists tcl.adb.orig] } {
	    file copy -force tcl.adb tcl.adb.orig
	}
	set inputFileName  tcl.adb.orig
	set outputFileName tcl.adb

	# open input file
	#-----------------
	if [catch {open $inputFileName r} ifid] {
	    tk_messageBox -icon error -message "$errorPrefix $ifid" \
		-parent . -title Error -type ok
	    return
	}

	# open output file
	#-----------------
	if [catch {open $outputFileName w} ofid] {
	    tk_messageBox -icon error -message "$errorPrefix $ofid" \
		-parent . -title Error -type ok
	    return
	}

	# Read input file and copy to output file 'til we find line
	# that contains start of the package body.  Insert "with" for
	# tash linker options package, then break out.
	#--------------------------------------------------------------
	while {[gets $ifid line] >= 0} {
	    set lcline [string tolower $line]
	    if [regexp "^ *package +body +tcl +is" $lcline] {
		puts $ofid "with TASH_Linker_Options;"
		puts $ofid ""
		puts $ofid $line
		break
	    }
	    puts $ofid $line
	}

	# Finish copying input to output
	#-------------------------------
	while {[gets $ifid line] >= 0} {
	    puts $ofid $line
	}

	close $ifid
	close $ofid

    } error] {
	tk_messageBox -icon error -message "$errorPrefix $error" \
	    -parent . -title Error -type ok
    }

    # We're done so return to the original working directory
    #-------------------------------------------------------
    cd $pwd
}

# Where does GNAT install GPSs?
proc FindInstallationPrefix {} {
    # This proc runs "gnatls -v". It ignores all lines up to the
    # 'Project Search Path" section, and then finds the shortest line
    # ending in /lib/gnat/; this is the directory where GNAT looks for
    # installed GPR files.

    # The prefix (as typically set by configure, ie the top level
    # directory for the package installation) is then set two levels
    # up from the discovered path.

    # NB, this approach works for GNAT GPL and FSF GCC; it does not
    # work for Debian, which uses a different directory structure.

    set gnatls [open {|gnatls -v}]

    set search_path_found 0
    set project_dir ""

    while {[gets $gnatls line] >= 0} {
        if {$search_path_found} {
            # Backslash separators => forward slashes, in case we're
            # on Windows.
            regsub -all {\\} $line {/} line
            set line [string trim $line]
            # On Windows, don't get the trailing '/'.
            if [regexp {/lib/gnat/?$} $line] {
                set pdl [string length $project_dir]
                if {$pdl == 0 || [string length $line] < $pdl} {
                    set project_dir $line
                }
            }
        } elseif {[regexp {^Project} $line]} {
            set search_path_found 1
        }
    }
    catch {close $gnatls}

    set cwd [pwd]
    set prefix ""
    catch {
        cd $project_dir/../..
        set prefix [pwd]
    }
    cd $cwd

    return $prefix
}

# Create the makeconf file
#--------------------------------
proc Createmakefile {makefile} {
    global tashorder tashvar tashcomments useLinkerOptions library_switches
    if [catch {open $makefile w} makefid] {
	puts stderr $makefid
	exit
    }
    puts $makefid "#$makefile"
    puts -nonewline $makefid {
#####################################################
#
# This file, makeconf, contains macros used to
# customize makefiles in various TASH directories.
#
# It is automatically generated by the setup.tcl script.
# To change them, you may either edit them directly
# with a text editor or run setup.tcl again.
#
#####################################################
}

    foreach name $tashorder {
	WriteOneMacro $makefid $name $tashvar($name) $tashcomments($name)
    }

    WriteOneMacro $makefid USE_LINKER_OPTIONS $useLinkerOptions \
	{Specifies whether to use pragma Linker_Options build method}

    if $useLinkerOptions {
	WriteOneMacro $makefid TASH_LINKER_OPTIONS tash_linker_options.ads \
	    {Source file containing TASH linker options}
        WriteOneMacro $makefid LARGS "" \
	    {All link switches macro is empty because we are
		using pragma Linker_Options method}
    } else {
	WriteOneMacro $makefid TASH_LINKER_OPTIONS "" \
	    {There is no source file containing TASH linker options}
	WriteOneMacro $makefid LARGS [join "-ltash" $library_switches] \
	    {All link switches for TASH, Tcl, and Tk}
    }

    #WriteOneMacro $makefid prefix [FindInstallationPrefix] \
    #    {Installation location}

    catch {close $makefid}
}

# Create the tash_options.gpr file
#--------------------------------
proc CreateGprFile {} {
    global library_switches gpr tashvar
    if [catch {open $gpr w} gprfid] {
	puts stderr $gprfid
	exit
    }
    puts $gprfid "--  $gpr
----------------------------------------------------------
--
--  This file, tash_options.gpr, contains global options
--  used for building and using Tash.
--
--  It is automatically generated by the setup.tcl script.
--
----------------------------------------------------------

project Tash_Options is

   for Source_Dirs use ();

   --  Tcl/Tk versions later than 8.4 don't provide the interfaces
   --  relied on by the Tash.* package hierarchy, so (by default) they
   --  won't be built if the Tcl/Tk version is later.
   Supports_Tash := external (\"SUPPORTS_TASH\", \"$tashvar(SUPPORTS_TASH)\");

   --  These are the Ada compiler options used to build Tash.
   Compiler_Options :=
     (
      \"[join $tashvar(AARGS) "\",\n      \""]\"
     );

   --  These are the C compiler options used to build Tash.
   C_Compiler_Options :=
     (
      \"[join [concat $tashvar(CARGS) $tashvar(TCL_INCLUDE)] "\",\n      \""]\"
     );

   --  These options determine the location of the system's Tcl, Tk
   --  libraries.
   Linker_Options :=
     (
      \"[join $library_switches "\",\n      \""]\"
     );

end Tash_Options;"
    catch {close $gprfid}
}

# Implement Save button command.  Creates makefile and
# tash_options.gpr, and optionally creates linker options package.
#-----------------------------------------------------------------
proc Save {g} {
    global tashorder tashvar useLinkerOptions makefile tcl_platform
    set row 0
    foreach name $tashorder {
        set w [string tolower $name]
        set tashvar($name) [$g.$w-entry get]
	incr row
    }
    Createmakefile $makefile
    CreateGprFile
    if $useLinkerOptions {
	CreateLinkerOptions
	EditSourceFile
    } else {
	# restore original tcl.adb file, if necessary
	set pwd [pwd]
	cd src
	if [file exists tcl.adb.orig] {
	    file copy -force tcl.adb.orig tcl.adb
	}
	cd $pwd
    }
}

proc fileDialog {w ent title initial} {
    set types {
	{"All files" *}
    }
    set file [tk_getOpenFile -filetypes $types -parent $w \
		  -initialdir $initial -title $title]
    if [string compare $file ""] {
	$ent delete 0 end
	$ent insert 0 $file
	$ent xview end
    }
}

# Establish values for all macros depending on platform
#------------------------------------------------------
proc Set_Macros {platform os osVersion} {
    global tcl_version tk_version tcl_interactive tcl_library tk_library env
    global tash_version tash_release library_switches gpr_switches

    set x11home           ""
    set x11_lib           ""
    set x11_include       ""
    set exec_suffix       ""

    regsub -all {[ \t]+} $os "_" os

    if [cequal $os "Darwin"] {
	set tclhome "/usr"
	set tcl_include "/usr/include"
    } else {
	set tclhome [file dirname [file dirname [info nameofexecutable]]]
	set tcl_include [file join $tclhome include]
	if {![info exists [file join $tcl_include tcl.h]]} {
	    set tcl_include [file join $tcl_include [file tail $tcl_library]]
	}
    }
    set library_switches  ""

    set pwd               [pwd]

    switch $platform {
	"windows" {
	    # This assumes ActiveTcl. Cygwin Tcl/Tk (at 30 Oct 2006)
	    # wouldn't run properly when called from Ada.
	    #
	    # It also assumes a GNAT that recognises tcl84.lib as a
	    # candidate for the linker switch -ltcl84.
	    #
	    # Most development tools get confused by paths with spaces.
	    regsub {PROGRAM FILES} $tclhome "PROGRA~1" tclhome
	    regsub {\.} $tcl_version {} tcl_short_version
	    regsub {\.} $tk_version  {} tk_short_version
	    set tclsh "tclsh${tcl_short_version}"
	    set libtcl ""
	    set tcldll "tcl${tcl_short_version}.dll"
	    set libtk ""
	    set tkdll  "tk${tk_short_version}.dll"
	    append library_switches "-L$tclhome/lib "
	    append library_switches "-ltk$tk_short_version "
	    append library_switches "-ltcl$tcl_short_version "
	    set exec_suffix ".exe"
	}
	"unix" {
	    set tclsh "tclsh"
	    set dynlib [info sharedlibextension]
	    set libtcl "$tclhome/lib/libtcl${tcl_version}$dynlib"
	    set libtk  "$tclhome/lib/libtk${tk_version}$dynlib"

	    # Not quite sure why we need X11 here, because neither
	    # Linux (Mandrake 8.2) nor Darwin need it.
	    set PossibleXHomes [list /usr/openwin /usr/X /usr/X11R6 /usr]
	    foreach dir $PossibleXHomes {
		set lib [file join $dir lib]
		foreach file [list "libX11$dynlib" "libX11.a"] {
		    if [file exists [file join $lib $file]] {
			set x11home $dir
			set x11_lib [file join $x11home lib]
			break
		    }
		}
	    }
	    if [file isdirectory [file join $x11home include]] {
		set x11_include [file join $x11home include]
	    } else {
		foreach dir $PossibleXHomes {
		    set include [file join $dir include]
		    if [file isdirectory $include] {
			set x11_include $include
			break
		    }
		}
	    }
	    if [cequal $os "SunOS"] {
		append library_switches " -R$tclhome/lib -L$tclhome/lib"
		append library_switches " -ltk$tk_version -ltcl$tcl_version"
	    } elseif [cequal $os "Darwin"] {
		append library_switches " -L$tclhome/lib"
		append library_switches " -ltk$tk_version -ltcl$tcl_version"
	    } else {
		# Must be Linux (?)
		append library_switches " -Wl,-rpath,$tclhome/lib"
		append library_switches " -L$tclhome/lib"
		append library_switches " -ltk$tk_version -ltcl$tcl_version"
	    }
	}
    }

    setvar PLATFORM          $platform            {OS platform}
    setvar OS                $os                  {Operating system}
    setvar OSVERSION         $osVersion           {Operating system version}
    setvar TASH_VERSION      "$tash_version"      {TASH version}
    setvar TASH_RELEASE      "$tash_release"      {TASH release}
    setvar INSTALLROOT       "/opt/tash"          {TASH installation directory}
    setvar prefix            "[FindInstallationPrefix]" \
                                          {GNAT Project installation directory}
    if [lempty $x11home] {
	setvar X11HOME       ""                   {X11 home directory}
    } else {
	setvar X11HOME       "$x11home"           {X11 home directory}
    }
    if [lempty $x11_lib] {
	setvar X11_LIB       ""                   {X11 library directory}
    } else {
	setvar X11_LIB       "$x11_lib"           {X11 library directory}
    }
    if [lempty $x11_include] {
	setvar X11_INCLUDE   ""                   {X11 include directory}
    } else {
	setvar X11_INCLUDE   "-I$x11_include"     {X11 include directory}
    }
    setvar TCLSH             "$tclsh"             {Tclsh executable}
    setvar TCLHOME           "$tclhome"           {Tcl Home directory}
    setvar TCL_INCLUDE       "-I$tcl_include"     {TCL include directory}
    setvar TCL_VERSION       "$tcl_version"       {Tcl version}
    setvar TCL_LIBRARY       "$libtcl"            {Tcl library}
    setvar TK_VERSION        "$tk_version"        {Tk version}
    setvar TK_LIBRARY        "$libtk"             {Tk library}
    setvar SUPPORTS_TASH     "[supportsTash]"     {Are Tash.* supported?}
    if [catch {exec gnatgcc -v >/dev/null 2>/dev/null}] {
        setvar CC            "gcc"                \
            {The gcc compiler for the C files; uses gnatmake for Ada files.}
    } else {
        setvar CC            "gnatgcc"            \
            {The gcc compiler for the C files; uses gnatmake for Ada files.}
    }
    setvar CARGS             "-g -O2"             {C compiler switches}
    setvar AARGS             "-g -O2 -gnatqQafoy -gnatwaL" \
        {Ada compiler switches}
    setvar BARGS             "-E"                 {gnatbind switches}
    setvar EXE               "$exec_suffix"       {suffix for executable files}
}

set useLinkerOptions 0

# Establish values for all macros depending on platform
#------------------------------------------------------
Set_Macros $tcl_platform(platform) $tcl_platform(os) $tcl_platform(osVersion)

# Create window for installer to review and edit macro values
#------------------------------------------------------------
set makefile [file join [pwd] makeconf]
set gpr [file join [pwd] tash_options.gpr]
wm title    . "TASH $tashvar(TASH_VERSION)-$tashvar(TASH_RELEASE) Setup -- $makefile"
wm iconname . "TASH Setup"

message .instructions -justify left -aspect 500 -pady 10 -padx 20 -text \
"This program, setup.tcl, customizes the TASH installation by\
creating the files $makefile and $gpr. These files customize TASH by\
specifying variables which control compilation and linking\
of TASH and applications using it.  Setup.tcl guesses \"reasonable\"\
values for the macros, but you may have to edit them.  After you're\
happy with the macro values, press \"Save\" to save the files."

pack .instructions -side top -fill x -expand yes

set f [frame .link]
pack $f -side top

set g [frame .grid]
pack $g -side top

set row 0

foreach name $tashorder {
    set w [string tolower $name]
    label $g.$w-label -text "$name: " -anchor e
    grid $g.$w-label  -row $row -column 0 -sticky e
    entry $g.$w-entry -width 40
    $g.$w-entry insert end $tashvar($name)
    switch -regexp $name {
	"(TK|TCL|TASH)_(VERSION|RELEASE)" {
	    $g.$w-entry configure -state disabled
	}
	"EXE" {
	    $g.$w-entry configure -state disabled
	}
	default {
	    $g.$w-entry configure -bg white
	}
    }
    grid $g.$w-entry  -row $row -column 1
    incr row
}

if {[lindex $argv 0] == "--nogui"} {
    Save $g;exit
} else {
    frame .buttons
    pack .buttons -side bottom -fill x -pady 2m
    button .buttons.save   -text Save   -command "Save $g; exit"
    button .buttons.cancel -text Cancel -command exit
    pack .buttons.save .buttons.cancel -side left -expand 1
}
