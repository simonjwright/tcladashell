--------------------------------------------------------------------
--
--  Unit Name:    Tash.Integers body
--
--  File Name:    tash-integers.adb
--
--  Purpose:      This package exports a Tash integer type along with
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

package body Tash.Integers is

   use type Interfaces.C.int;
   use type Interfaces.C.long;

   function Is_Zero (TInteger : in Tash_Integer) return Boolean is
   begin --  Is_Zero
      return Is_Null (TInteger) or else To_Integer (TInteger) = 0;
   end Is_Zero;
   pragma Inline (Is_Zero);

   function To_Tash_Integer (Str : in String) return Tash_Integer is
      --
      New_Obj : Tcl.Tcl_Obj;
   begin --  To_Tash_Integer
      New_Obj := Tcl.Tcl_NewLongObj (Interfaces.C.long'Value (Str));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_Integer;

   function To_String (TInteger : in Tash_Integer) return String is
      --
      Length : aliased Interfaces.C.int;
   begin --  To_String
      if Is_Null (TInteger) then
         return "0";
      else
         return CHelper.Value
                  (Tcl.Tcl_GetStringFromObj (TInteger.Obj, Length'Access));
      end if;
   end To_String;

   function To_Tash_Integer
     (LInteger : in Tash_Integer_Range)
      return     Tash_Integer
   is
      --
      New_Obj : Tcl.Tcl_Obj;
   begin --  To_Tash_Integer
      New_Obj := Tcl.Tcl_NewLongObj (Interfaces.C.long (LInteger));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_Integer;

   function "+" (LInteger : in Tash_Integer_Range) return Tash_Integer is
   begin --  "+"
      return To_Tash_Integer (+LInteger);
   end "+";

   function "-" (LInteger : in Tash_Integer_Range) return Tash_Integer is
   begin --  "-"
      return To_Tash_Integer (-LInteger);
   end "-";

   function To_Integer
     (TInteger : in Tash_Integer)
      return     Tash_Integer_Range
   is
      --
      Result : Interfaces.C.int;
      Value  : aliased Interfaces.C.long;
      Interp : Tcl.Tcl_Interp;
   begin --  To_Integer
      if Is_Null (TInteger) then
         return 0;
      end if;
      Tash_Interp.Get (Interp);
      Result :=
         Tcl.Tcl_GetLongFromObj
           (interp  => Interp,
            objPtr  => TInteger.Obj,
            longPtr => Value'Access);
      Tash_Interp.Release (Interp);
      if Result /= Tcl.TCL_OK then
         raise Constraint_Error;
      end if;
      return Tash_Integer_Range (Value);
   end To_Integer;

   procedure Incr
     (TInteger : in out Tash_Integer;
      By       : in Long_Integer := 1)
   is
      --
      Result : Interfaces.C.int;
      Value  : aliased Interfaces.C.long;
      Interp : Tcl.Tcl_Interp;
   begin --  Incr
      if Is_Null (TInteger) then
         TInteger := To_Tash_Integer (1);
         return;
      end if;
      Tash_Interp.Get (Interp);
      Result :=
         Tcl.Tcl_GetLongFromObj
           (interp  => Interp,
            objPtr  => TInteger.Obj,
            longPtr => Value'Access);
      Tash_Interp.Release (Interp);
      if Result /= Tcl.TCL_OK then
         raise Constraint_Error;
      end if;
      Value := Value + Interfaces.C.long (By);
      Tcl.Tcl_SetLongObj (TInteger.Obj, Value);
   end Incr;

   procedure Incr
     (TInteger : in out Tash_Integer;
      Amount   : in Tash_Integer)
   is
      --
      Result : Interfaces.C.int;
      Value  : aliased Interfaces.C.long;
      Interp : Tcl.Tcl_Interp;
   begin --  Incr
      if Is_Null (TInteger) then
         TInteger := To_Tash_Integer (1);
         return;
      end if;
      Tash_Interp.Get (Interp);
      Result :=
         Tcl.Tcl_GetLongFromObj
           (interp  => Interp,
            objPtr  => TInteger.Obj,
            longPtr => Value'Access);
      Tash_Interp.Release (Interp);
      if Result /= Tcl.TCL_OK then
         raise Constraint_Error;
      end if;
      Value := Value + Interfaces.C.long (To_Integer (Amount));
      Tcl.Tcl_SetLongObj (TInteger.Obj, Value);
   end Incr;

   function "="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  "="
      return To_Integer (Left) = To_Integer (Right);
   end "=";

   function "="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean
   is
   begin --  "="
      return To_Integer (Left) = Right;
   end "=";

   function "="
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  "="
      return Left = To_Integer (Right);
   end "=";

   function "<"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  "<"
      return To_Integer (Left) < To_Integer (Right);
   end "<";

   function "<"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean
   is
   begin --  "<"
      return To_Integer (Left) < Right;
   end "<";

   function "<"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  "<"
      return Left < To_Integer (Right);
   end "<";

   function "<="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  "<="
      return To_Integer (Left) <= To_Integer (Right);
   end "<=";

   function "<="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean
   is
   begin --  "<="
      return To_Integer (Left) <= Right;
   end "<=";

   function "<="
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  "<="
      return Left <= To_Integer (Right);
   end "<=";

   function ">"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  ">"
      return To_Integer (Left) > To_Integer (Right);
   end ">";

   function ">"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean
   is
   begin --  ">"
      return To_Integer (Left) > Right;
   end ">";

   function ">"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  ">"
      return Left > To_Integer (Right);
   end ">";

   function ">="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  ">="
      return To_Integer (Left) >= To_Integer (Right);
   end ">=";

   function ">="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean
   is
   begin --  ">="
      return To_Integer (Left) >= Right;
   end ">=";

   function ">="
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean
   is
   begin --  ">="
      return Left >= To_Integer (Right);
   end ">=";

   function "abs" (Right : in Tash_Integer) return Tash_Integer is
   begin --  "abs"
      return To_Tash_Integer (Standard. "abs" (To_Integer (Right)));
   end "abs";

   function "+"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "+"
      return To_Tash_Integer
               (Standard. "+" (To_Integer (Left), To_Integer (Right)));
   end "+";

   function "+"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer
   is
   begin --  "+"
      return To_Tash_Integer (Standard. "+" (To_Integer (Left), Right));
   end "+";

   function "+"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "+"
      return To_Tash_Integer (Standard. "+" (Left, To_Integer (Right)));
   end "+";

   function "-"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "-"
      return To_Tash_Integer
               (Standard. "-" (To_Integer (Left), To_Integer (Right)));
   end "-";

   function "-"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer
   is
   begin --  "-"
      return To_Tash_Integer (Standard. "-" (To_Integer (Left), Right));
   end "-";

   function "-"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "-"
      return To_Tash_Integer (Standard. "-" (Left, To_Integer (Right)));
   end "-";

   function "*"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "*"
      return To_Tash_Integer
               (Standard. "*" (To_Integer (Left), To_Integer (Right)));
   end "*";

   function "*"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer
   is
   begin --  "*"
      return To_Tash_Integer (Standard. "*" (To_Integer (Left), Right));
   end "*";

   function "*"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "*"
      return To_Tash_Integer (Standard. "*" (Left, To_Integer (Right)));
   end "*";

   function "/"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "/"
      return To_Tash_Integer
               (Standard. "/" (To_Integer (Left), To_Integer (Right)));
   end "/";

   function "/"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer
   is
   begin --  "/"
      return To_Tash_Integer (Standard. "/" (To_Integer (Left), Right));
   end "/";

   function "/"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "/"
      return To_Tash_Integer (Standard. "/" (Left, To_Integer (Right)));
   end "/";

   function "rem"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "rem"
      return To_Tash_Integer
               (Standard. "rem" (To_Integer (Left), To_Integer (Right)));
   end "rem";

   function "rem"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer
   is
   begin --  "rem"
      return To_Tash_Integer (Standard. "rem" (To_Integer (Left), Right));
   end "rem";

   function "rem"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "rem"
      return To_Tash_Integer (Standard. "rem" (Left, To_Integer (Right)));
   end "rem";

   function "mod"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "mod"
      return To_Tash_Integer
               (Standard. "mod" (To_Integer (Left), To_Integer (Right)));
   end "mod";

   function "mod"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer
   is
   begin --  "mod"
      return To_Tash_Integer (Standard. "mod" (To_Integer (Left), Right));
   end "mod";

   function "mod"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer
   is
   begin --  "mod"
      return To_Tash_Integer (Standard. "mod" (Left, To_Integer (Right)));
   end "mod";

   function "**"
     (Left  : in Tash_Integer;
      Right : in Natural)
      return  Tash_Integer
   is
   begin --  "**"
      return To_Tash_Integer (Standard. "**" (To_Integer (Left), Right));
   end "**";

end Tash.Integers;
