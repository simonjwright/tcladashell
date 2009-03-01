--------------------------------------------------------------------
--
--  Tashell.adb -- Tcl Ada SHell.  This program is the Ada version of the
--             tclsh program included in the Tcl distribution.
--
--  Copyright (c) 1995-1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with CArgv;
with TashApp;
with Tcl;     use Tcl;

procedure Tashell is --  Tcl Ada SHell

   --  Argc and Argv include the command name
   Argc : C.int;
   Argv : CArgv.Chars_Ptr_Ptr;

begin --  Tashell

   --  Get command-line arguments and put them into C-style "argv",
   --  as required by Tcl_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl (and never return!)
   Tcl_Main (Argc, Argv, TashApp.Init'Access);

end Tashell;
