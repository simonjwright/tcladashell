--------------------------------------------------------------------
--
--  twashapp.ads -- This package provides the Init function required
--                 in the call to Tk_Main.
--
--  Copyright (c) 1995-1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Interfaces.C;
with Tcl;

package TWashApp is

   package C renames Interfaces.C;

   function Init (Interp : Tcl.Tcl_Interp) return C.int;
   pragma Convention (C, Init);

end TWashApp;
