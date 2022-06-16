--------------------------------------------------------------------
--
--  testapp.ads -- This package provides the Init function required
--                 in the call to Tcl_Main.  It creates several new
--                 Tcl commands which are used to test the TASH
--                 Ada/Tcl interface.
--
--  Copyright (c) 1995-1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Interfaces.C;
with Tcl; use Tcl;

package TestApp is

   package C renames Interfaces.C;

   function Init (Interp : Tcl_Interp) return C.int;
   pragma Convention (C, Init);

end TestApp;
