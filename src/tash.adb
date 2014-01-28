--------------------------------------------------------------------
--
--  Unit Name:    Tash body
--
--  File Name:    tash.adb
--
--  Purpose:      This package is the root of a family of packages
--               which implement a binding to Tcl.
--
--  Copyright (c) 1999-2000 Terry J. Westley
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

with Ada.Command_Line;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with CHelper;
with Interfaces.C.Strings;

package body Tash is

   use type Interfaces.C.int;

   function Image (Num : in Integer) return String;
   function Image (Num : in Interfaces.C.int) return String;

   function Is_Null (TObject : in Tash_Object) return Boolean is
      use type Tcl.Tcl_Obj;
   begin --  Is_Null
      return TObject.Obj = null;
   end Is_Null;
   pragma Inline (Is_Null);

   procedure Finalize (Obj : in out Tcl.Tcl_Obj) is
      pragma Warnings (Off, Obj); -- logically in out
      use type Tcl.Tcl_Obj;
   begin --  Finalize
      if Obj /= null then
         if Tcl.Tcl_GetRefCount (Obj) > 0 then
            Tcl.Tcl_DecrRefCount (Obj);
            if Tash.Verbose then
               Ada.Text_IO.Put_Line ("Finalize: " & Internal_Rep (Obj));
            end if;
         end if;
      end if;
   end Finalize;

   procedure Finalize (TObject : in out Tash_Object) is
   begin --  Finalize
      Finalize (TObject.Obj);
   end Finalize;

   procedure Adjust (TObject : in out Tash_Object) is
      use type Tcl.Tcl_Obj;
   begin --  Adjust
      if TObject.Obj /= null then
         Tcl.Tcl_IncrRefCount (TObject.Obj);
         if Tash.Verbose then
            Ada.Text_IO.Put_Line ("Adjust: " & Internal_Rep (TObject));
         end if;
      end if;
   end Adjust;

   protected body Tash_Interp is

      entry Get (Interp : out Tcl.Tcl_Interp) when not Seized is
      begin --  Get
         --  Ada.Text_IO.Put_Line ("T.TI.Get");
         Seized := True;
         Interp := Tcl_Interp;
      end Get;

      procedure Release (Interp : in Tcl.Tcl_Interp) is
      begin --  Release
         --  Ada.Text_IO.Put_Line ("T.TI.Release");
         Seized     := False;
         Tcl_Interp := Interp;
      end Release;

      procedure Assert
        (Interp      : in Tcl.Tcl_Interp;
         Return_Code : in Interfaces.C.int;
         E           : in Ada.Exceptions.Exception_Id)
      is
      begin --  Assert
         if Return_Code = Tcl.TCL_ERROR then
            Raise_Exception (Interp, E);
         end if;
      end Assert;

      procedure Raise_Exception
        (Interp : in Tcl.Tcl_Interp;
         E      : in Ada.Exceptions.Exception_Id)
      is
         --
         Result : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin --  Raise_Exception
         Seized     := False;
         Tcl_Interp := Interp;
         Ada.Exceptions.Raise_Exception (E => E, Message => Result);
      end Raise_Exception;

      procedure Raise_Exception
        (Interp  : in Tcl.Tcl_Interp;
         E       : in Ada.Exceptions.Exception_Id;
         Message : in String)
      is
      begin --  Raise_Exception
         Seized     := False;
         Tcl_Interp := Interp;
         Ada.Exceptions.Raise_Exception (E => E, Message => Message);
      end Raise_Exception;

   end Tash_Interp;

   function Type_Of (TObject : in Tash_Object'Class) return String is
   begin --  Type_Of
      return CHelper.Value (Tcl.Tcl_GetObjTypeName (TObject.Obj));
   end Type_Of;
   pragma Inline (Type_Of);

   function Ref_Count (TObject : in Tash_Object'Class) return Natural is
   begin --  Ref_Count
      return Natural (Tcl.Tcl_GetRefCount (TObject.Obj));
   end Ref_Count;
   pragma Inline (Ref_Count);

   procedure PrintObj (TObject : in Tash_Object'Class) is
   begin --  PrintObj
      Tcl.Tcl_PrintObj (TObject.Obj);
   end PrintObj;

   function Image (TObject : in Tcl.Tcl_Obj) return String is
      --
      Length : aliased Interfaces.C.int;
      use type Tcl.Tcl_Obj;
   begin --  Image
      if TObject = null then
         return "NULL";
      elsif Tcl.Tcl_GetRefCount (TObject) = 0 then
         return "FINALIZED";
      else
         return CHelper.Value
                  (Tcl.Tcl_GetStringFromObj (TObject, Length'Access));
      end if;
   exception
      when Id : others =>
         return Ada.Exceptions.Exception_Name (Id);
   end Image;

   function Image (Num : in Integer) return String is
   begin --  Image
      return Ada.Strings.Fixed.Trim
               (Source => Integer'Image (Num),
                Side   => Ada.Strings.Left);
   end Image;

   function Image (Num : in Interfaces.C.int) return String is
   begin --  Image
      return Ada.Strings.Fixed.Trim
               (Source => Interfaces.C.int'Image (Num),
                Side   => Ada.Strings.Left);
   end Image;
   pragma Inline (Image);

   function Internal_Rep (TObject : in Tash_Object) return String is
   begin --  Internal_Rep
      return "(s=""" &
             Image (TObject.Obj) &
             """ t=" &
             Type_Of (TObject) &
             " tag=" &
             Ada.Tags.External_Tag (Tash_Object'Tag) &
             " c=" &
             Image (Ref_Count (TObject)) &
             ")";
   end Internal_Rep;

   function Internal_Rep (TObj : in Tcl.Tcl_Obj) return String is
   begin --  Internal_Rep
      return "(s=""" &
             Image (TObj) &
             """ t=" &
             CHelper.Value (Tcl.Tcl_GetObjTypeName (TObj)) &
             " c=" &
             Image (Tcl.Tcl_GetRefCount (TObj)) &
             ")";
   end Internal_Rep;

   function To_Tcl_Obj (Str : in String) return Tcl.Tcl_Obj is
      C_Str   : aliased Interfaces.C.char_array := Interfaces.C.To_C (Str);
      New_Obj : Tcl.Tcl_Obj;
   begin --  To_Tcl_Obj
      New_Obj :=
         Tcl.Tcl_NewStringObj
           (Interfaces.C.Strings.To_Chars_Ptr (C_Str'Unchecked_Access),
            Interfaces.C.int (Str'Length));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return New_Obj;
   end To_Tcl_Obj;

   function To_Tcl_Obj (Num : in Integer) return Tcl.Tcl_Obj is
   begin --  To_Tcl_Obj
      return To_Tcl_Obj
               (Ada.Strings.Fixed.Trim
                   (Source => Integer'Image (Num),
                    Side   => Ada.Strings.Left));
   end To_Tcl_Obj;

begin --  Tash

   --  Create and initialize the Tcl interpreter
   --------------------------------------------
   declare
      Command_Name : constant String :=
         Ada.Command_Line.Command_Name;
      Executable   : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (Command_Name);
      Local_Interp : Tcl.Tcl_Interp;
   begin

      --  Tcl needs to know the path name of the executable
      --  otherwise Tcl.Tcl_Init below will fail.
      ----------------------------------------------------
      Tcl.Tcl_FindExecutable
        (Interfaces.C.Strings.To_Chars_Ptr (Executable'Unchecked_Access));

      --  Create one Tcl interpreter
      -----------------------------
      Tash_Interp.Get (Local_Interp);
      Local_Interp := Tcl.Tcl_CreateInterp;

      --  Initialize Tcl
      -----------------
      if Tcl.Tcl_Init (Local_Interp) = Tcl.TCL_ERROR then
         Ada.Text_IO.Put_Line
           (Command_Name &
            ": Tcl_Init failed: " &
            CHelper.Value (Tcl.Tcl_GetStringResult (Local_Interp)));
         Tash_Interp.Release (Local_Interp);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      Tash_Interp.Release (Local_Interp);

   end;

end Tash;
