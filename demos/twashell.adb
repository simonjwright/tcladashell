--------------------------------------------------------------------
--
--  twashell.adb -- Tcl Windowing Ada SHell.  This program is the Ada version
--              of the wish program included in the Tk distribution.
--
--  Copyright (c) 1995-1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--
--------------------------------------------------------------------

with CArgv;
with Interfaces.C;
with Tcl.Tk;
with Tcl;
with TWashApp;

procedure TWASHell is --  Tcl Windowing Ada SHell

   package C renames Interfaces.C;

   --  Argc and Argv include the command name
   Argc : C.int;
   Argv : CArgv.Chars_Ptr_Ptr;

begin --  TWASHell

   --  Get command-line arguments and put them into C-style "argv,"
   --  as required by Tk_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl
   Tcl.Tk.Tk_Main (Argc, Argv, TWashApp.Init'Access);

end TWASHell;
