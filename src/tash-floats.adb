--------------------------------------------------------------------
--
--  Unit Name:    Tash.Floats body
--
--  File Name:    tash-floats.adb
--
--  Purpose:      This package exports a Tash float type along with
--               its operations.
--
--  Copyright (c) 1999 Terry J. Westley
--
--  Tash is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version. Tash is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. You should have received a copy of the GNU General
--  Public License distributed with Tash; see file COPYING. If not, write to
--
--          Free Software Foundation
--          59 Temple Place - Suite 330
--          Boston, MA 02111-1307, USA
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
--  Tash is maintained at http://tcladashell.sourceforge.net/.
--
--------------------------------------------------------------------

with CHelper;

package body Tash.Floats is

   use type Interfaces.C.int;
   use type Interfaces.C.double;

   function Is_Zero (TFloat : in Tash_Float) return Boolean is
   begin --  Is_Zero
      return Is_Null (TFloat) or else To_Float (TFloat) = 0.0;
   end Is_Zero;
   pragma Inline (Is_Zero);

   function To_Tash_Float (Str : in String) return Tash_Float is
      --
      New_Obj : Tcl.Tcl_Obj;
   begin --  To_Tash_Float
      New_Obj := Tcl.Tcl_NewDoubleObj (Interfaces.C.double'Value (Str));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_Float;

   function To_String (TFloat : in Tash_Float) return String is
      --
      Length : aliased Interfaces.C.int;
   begin --  To_String
      if Is_Null (TFloat) then
         return "0.0";
      else
         return CHelper.Value
                  (Tcl.Tcl_GetStringFromObj (TFloat.Obj, Length'Access));
      end if;
   end To_String;

   function To_Tash_Float (LFloat : in Tash_Float_Range) return Tash_Float is
      --
      New_Obj : Tcl.Tcl_Obj;
   begin --  To_Tash_Float
      New_Obj := Tcl.Tcl_NewDoubleObj (Interfaces.C.double (LFloat));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_Float;

   function "+" (LFloat : in Tash_Float_Range) return Tash_Float is
   begin --  "+"
      return To_Tash_Float (+LFloat);
   end "+";

   function "-" (LFloat : in Tash_Float_Range) return Tash_Float is
   begin --  "-"
      return To_Tash_Float (-LFloat);
   end "-";

   function To_Float (TFloat : in Tash_Float) return Tash_Float_Range is
      --
      Result : Interfaces.C.int;
      Value  : aliased Interfaces.C.double;
      Interp : Tcl.Tcl_Interp;
   begin --  To_Float
      if Is_Null (TFloat) then
         return 0.0;
      end if;
      Tash_Interp.Get (Interp);
      Result :=
         Tcl.Tcl_GetDoubleFromObj
           (interp    => Interp,
            objPtr    => TFloat.Obj,
            doublePtr => Value'Access);
      Tash_Interp.Release (Interp);
      if Result /= Tcl.TCL_OK then
         raise Constraint_Error;
      end if;
      return Tash_Float_Range (Value);
   end To_Float;

   function "="
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  "="
      return To_Float (Left) = To_Float (Right);
   end "=";

   function "="
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean
   is
   begin --  "="
      return To_Float (Left) = Right;
   end "=";

   function "="
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  "="
      return Left = To_Float (Right);
   end "=";

   function "<"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  "<"
      return To_Float (Left) < To_Float (Right);
   end "<";

   function "<"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean
   is
   begin --  "<"
      return To_Float (Left) < Right;
   end "<";

   function "<"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  "<"
      return Left < To_Float (Right);
   end "<";

   function "<="
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  "<="
      return To_Float (Left) <= To_Float (Right);
   end "<=";

   function "<="
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean
   is
   begin --  "<="
      return To_Float (Left) <= Right;
   end "<=";

   function "<="
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  "<="
      return Left <= To_Float (Right);
   end "<=";

   function ">"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  ">"
      return To_Float (Left) > To_Float (Right);
   end ">";

   function ">"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean
   is
   begin --  ">"
      return To_Float (Left) > Right;
   end ">";

   function ">"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  ">"
      return Left > To_Float (Right);
   end ">";

   function ">="
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  ">="
      return To_Float (Left) >= To_Float (Right);
   end ">=";

   function ">="
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean
   is
   begin --  ">="
      return To_Float (Left) >= Right;
   end ">=";

   function ">="
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean
   is
   begin --  ">="
      return Left >= To_Float (Right);
   end ">=";

   function "abs" (Right : in Tash_Float) return Tash_Float is
   begin --  "abs"
      return To_Tash_Float (Standard. "abs" (To_Float (Right)));
   end "abs";

   function "+"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "+"
      return To_Tash_Float
               (Standard. "+" (To_Float (Left), To_Float (Right)));
   end "+";

   function "+"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float
   is
   begin --  "+"
      return To_Tash_Float (Standard. "+" (To_Float (Left), Right));
   end "+";

   function "+"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "+"
      return To_Tash_Float (Standard. "+" (Left, To_Float (Right)));
   end "+";

   function "-"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "-"
      return To_Tash_Float
               (Standard. "-" (To_Float (Left), To_Float (Right)));
   end "-";

   function "-"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float
   is
   begin --  "-"
      return To_Tash_Float (Standard. "-" (To_Float (Left), Right));
   end "-";

   function "-"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "-"
      return To_Tash_Float (Standard. "-" (Left, To_Float (Right)));
   end "-";

   function "*"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "*"
      return To_Tash_Float
               (Standard. "*" (To_Float (Left), To_Float (Right)));
   end "*";

   function "*"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float
   is
   begin --  "*"
      return To_Tash_Float (Standard. "*" (To_Float (Left), Right));
   end "*";

   function "*"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "*"
      return To_Tash_Float (Standard. "*" (Left, To_Float (Right)));
   end "*";

   function "/"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "/"
      return To_Tash_Float
               (Standard. "/" (To_Float (Left), To_Float (Right)));
   end "/";

   function "/"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float
   is
   begin --  "/"
      return To_Tash_Float (Standard. "/" (To_Float (Left), Right));
   end "/";

   function "/"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float
   is
   begin --  "/"
      return To_Tash_Float (Standard. "/" (Left, To_Float (Right)));
   end "/";

   function "**"
     (Left  : in Tash_Float;
      Right : in Integer)
      return  Tash_Float
   is
   begin --  "**"
      return To_Tash_Float (Standard. "**" (To_Float (Left), Right));
   end "**";

end Tash.Floats;
