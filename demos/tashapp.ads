--------------------------------------------------------------------
--
--  tashapp.ads -- This package provides the Init function required
--                in the call to Tcl_Main.
--
--  Copyright (c) 1995-1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--  Note, this function is in a separate library package to satisfy
--  accessibility levels in refering to Init'access in call to Tcl_Init.
--
--------------------------------------------------------------------

with Interfaces.C;
with Tcl;

package TashApp is

   package C renames Interfaces.C;

   function Init (Interp : in Tcl.Tcl_Interp) return C.int;
   pragma Convention (C, Init);

end TashApp;
