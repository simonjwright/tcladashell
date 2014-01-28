--------------------------------------------------------------------
--
--  Unit Name:    Tash.Arrays body
--
--  File Name:    tash-arrays.adb
--
--  Purpose:      Defines the Tash array type which is
--                an associative array whose indices are
--                strings and contents may be any Tash data type.
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
--
--  We have chosen to implement Tash arrays by calling
--  Tcl_ArrayObjCmd.  This will provide a complete implementation
--  and is easier to develop than other possible implementation methods:
--
--   1) Call Tcl hash facilities directly (see tcl.ads), or
--   2) Create Tash version of Tcl hash facilities and use it.
--
--------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with CHelper;
with Tcl.Ada;
with System;

package body Tash.Arrays is

   function Get_Element (Interp : in Tcl.Tcl_Interp;
                         TArray : in Tcl.Tcl_Obj;
                         Index  : in String) return Tcl.Tcl_Obj;
   function Type_Of_Array_Element (ObjPtr : in Tcl.Tcl_Obj) return String;

   use type Interfaces.C.int;

   Array_Cmd    : constant Tcl.Tcl_Obj := Tash.To_Tcl_Obj ("array");

   Get_Option   : constant Tcl.Tcl_Obj := Tash.To_Tcl_Obj ("get");
   Names_Option : constant Tcl.Tcl_Obj := Tash.To_Tcl_Obj ("names");
   Set_Option   : constant Tcl.Tcl_Obj := Tash.To_Tcl_Obj ("set");
   Size_Option  : constant Tcl.Tcl_Obj := Tash.To_Tcl_Obj ("size");
   Unset_Option : constant Tcl.Tcl_Obj := Tash.To_Tcl_Obj ("unset");

   function Tcl_ArrayObjCmd (
      dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array
   ) return Interfaces.C.int;
   pragma Import (C, Tcl_ArrayObjCmd, "Tcl_ArrayObjCmd");

   function TclInfoExistsCmd (
      dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array
   ) return Interfaces.C.int;
   pragma Import (C, TclInfoExistsCmd, "TclInfoExistsCmd");

   protected Variable_Count is
      procedure Next (Value : out Natural);
   private
      Count : Natural := 0;
   end Variable_Count;

   protected body Variable_Count is
      procedure Next (Value : out Natural) is
      begin -- Next
         Value := Count;
         Count := Count + 1;
      end Next;
   end Variable_Count;

   function Is_Empty (
      TArray : in Tash_Array) return Boolean is
   begin -- Is_Empty
      return Is_Null (TArray) or else Length (TArray) = 0;
   end Is_Empty;
   pragma Inline (Is_Empty);

   function To_Tash_Array (
      Str : in String) return Tash_Array is

      Count   : Natural;
      New_Obj : Tcl.Tcl_Obj;
      Objc    : constant Interfaces.C.int := 4;
      Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
      TArray  : Tash_Array;
      Interp  : Tcl.Tcl_Interp;
      Result  : Interfaces.C.int;

   begin -- To_Tash_Array

      --  A Tash array is merely a unique variable name for a
      --  Tcl array.  We will now create a Tcl object and a
      --  unique variable name string to store in it.
      ------------------------------------------------------
      Variable_Count.Next (Count);
      New_Obj := Tash.To_Tcl_Obj ("array" &
         Ada.Strings.Fixed.Trim (Natural'Image (Count), Ada.Strings.Left));
      TArray := (Ada.Finalization.Controlled with
                 Obj  => New_Obj);

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Set_Option;
      Objv (3) := TArray.Obj;
      Objv (4) := Tash.To_Tcl_Obj (Str);

      --  Use Tcl_ArrayObjCmd to create a new Tcl array
      ------------------------------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tcl.Tcl_DecrRefCount (Objv (4));
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

      PArray (TArray);

      return TArray;

   end To_Tash_Array;

   function "+" (
      Str : in String) return Tash_Array is
   begin -- "+"
      return To_Tash_Array (Str);
   end "+";
   pragma Inline ("+");

--   function Copy (
--      TArray   : in Tash_Array) return Tash_Array;

--   function Duplicate (
--      TArray   : in Tash_Array) return Tash_Array;

   function To_String (
      TArray : in Tash_Array) return String is

      Objc      : constant Interfaces.C.int := 3;
      Objv      : Tcl.Tcl_Obj_Array (1 .. Objc);
      Interp    : Tcl.Tcl_Interp;
      Result    : Interfaces.C.int;
      InterpObj : Tcl.Tcl_Obj;
      Length    : aliased Interfaces.C.int;

   begin -- To_String

      if Is_Empty (TArray) then
         return "";
      end if;

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Get_Option;
      Objv (3) := TArray.Obj;

      --  Get the array value as a string into
      --  Interp's result object.
      ---------------------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      InterpObj := Tcl.Tcl_GetObjResult (Interp);

      --  Get and return the string in Interp's result object
      ------------------------------------------------------
      declare
         Image : constant String := CHelper.Value (
            Tcl.Tcl_GetStringFromObj (InterpObj, Length'Access));
      begin
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Image;
      end;

   end To_String;

   procedure PArray (
      TArray : in Tash_Array) is
   --
      Interp    : Tcl.Tcl_Interp;
   begin -- PArray
      Tash_Interp.Get (Interp);
      Tcl.Ada.Tcl_Eval (Interp, "parray " & Tash.Image (TArray.Obj));
      Tash_Interp.Release (Interp);
   end PArray;

   function Length (
      TArray : in Tash_Array) return Natural is
   --
      Objc      : constant Interfaces.C.int := 3;
      Objv      : Tcl.Tcl_Obj_Array (1 .. Objc);
      Interp    : Tcl.Tcl_Interp;
      Result    : Interfaces.C.int;
      InterpObj : Tcl.Tcl_Obj;
      C_Length  : aliased Interfaces.C.long;
   begin -- Length
      Objv (1) := Array_Cmd;
      Objv (2) := Size_Option;
      Objv (3) := TArray.Obj;
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      InterpObj := Tcl.Tcl_GetObjResult (Interp);
      Result := Tcl.Tcl_GetLongFromObj (
         Interp, InterpObj, C_Length'Access);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);
      return Integer (C_Length);
   end Length;

   function Get_Element (
      Interp : in Tcl.Tcl_Interp;
      TArray : in Tcl.Tcl_Obj;
      Index  : in String) return Tcl.Tcl_Obj is

      Objc       : constant Interfaces.C.int := 4;
      Objv       : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result     : Interfaces.C.int;
      InterpObj  : Tcl.Tcl_Obj;
      ElementObj : aliased Tcl.Tcl_Obj;

      use type Tcl.Tcl_Obj;

   begin -- Get_Element

      if TArray = null then
         Tash_Interp.Raise_Exception
           (Interp  => Interp,
            E       => Array_Error'Identity,
            Message =>
              "Element """ & Index & """ does not exist: array is null");
      end if;

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Get_Option;
      Objv (3) := TArray;
      Objv (4) := Tash.To_Tcl_Obj (Index);

      --  Get the array element in Interp's result object
      --------------------------------------------------
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tcl.Tcl_DecrRefCount (Objv (4));
      if Result = Tcl.TCL_ERROR then
         Tash_Interp.Raise_Exception (
            Interp  => Interp,
            E       => Array_Error'Identity,
            Message => CHelper.Value (Tcl.Tcl_GetStringResult (Interp)));
      end if;

      --  The 2nd element of the list in the Interp's result object
      --  is the array element value we're looking for.
      ------------------------------------------------------------
      InterpObj  := Tcl.Tcl_GetObjResult (Interp);
      Result := Tcl.Tcl_ListObjIndex (
         interp    => Interp,
         listPtr   => InterpObj,
         index     => 1,
         objPtrPtr => ElementObj'Unchecked_Access);
      if Result = Tcl.TCL_ERROR then
         Tash_Interp.Raise_Exception (
            Interp  => Interp,
            E       => Array_Error'Identity,
            Message => CHelper.Value (Tcl.Tcl_GetStringResult (Interp)));
      end if;
      if ElementObj = null then
         Tash_Interp.Raise_Exception (
            Interp  => Interp,
            E       => Array_Error'Identity,
            Message => "Element """ & Index & """ does not exist");
      end if;
      return ElementObj;

   end Get_Element;

   function Get_Element (
      TArray : in Tash_Array;
      Index  : in String) return String is
   --
      Interp  : Tcl.Tcl_Interp;
   begin -- Get_Element
      Tash_Interp.Get (Interp);
      declare
         Obj   : constant Tcl.Tcl_Obj :=
           Get_Element (Interp, TArray.Obj, Index);
         Len   : aliased Interfaces.C.int;
      begin
         Tash_Interp.Release (Interp);
         return Tcl.Ada.Tcl_GetStringFromObj (Obj, Len'Access);
      end;
   end Get_Element;

   function Get_Element (
      TArray : in Tash_Array;
      Index  : in String) return Tash.Lists.Tash_List is
   --
      Interp : Tcl.Tcl_Interp;
   begin -- Get_Element
      Tash_Interp.Get (Interp);
      declare
         Obj : constant Tcl.Tcl_Obj :=
           Get_Element (Interp, TArray.Obj, Index);
      begin
         Tash_Interp.Release (Interp);
         Tcl.Tcl_IncrRefCount (Obj);
         return (Ada.Finalization.Controlled with Obj => Obj);
      end;
   end Get_Element;

   function Exists (
      TArray : in Tash_Array;
      Index  : in String) return Boolean is

      Objc      : constant Interfaces.C.int := 2;
      Objv      : Tcl.Tcl_Obj_Array (1 .. Objc);
      Interp    : Tcl.Tcl_Interp;
      Result    : Interfaces.C.int;
      InterpObj : Tcl.Tcl_Obj;
      Length    : aliased Interfaces.C.int;

   begin -- Exists

      if Is_Empty (TArray) then
         return False;
      end if;

      --  Create object parameters for TclInfoExistsCmd call
      ----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("exists");
      Objv (2) := Tash.To_Tcl_Obj (Tash.Image (TArray.Obj)
                                   & "(" & Index & ")");

      --  Execute the info command
      ---------------------------
      Tash_Interp.Get (Interp);
      Result := TclInfoExistsCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      InterpObj := Tcl.Tcl_GetObjResult (Interp);
      Tash_Interp.Release (Interp);

      --  Get the string in Interp's result object and
      --  return whether it is equal to "1" or not
      -----------------------------------------------
      declare
         Image : constant String := CHelper.Value (
            Tcl.Tcl_GetStringFromObj (InterpObj, Length'Access));
      begin
         return Image = "1";
      end;

   end Exists;

   --  Determine type of array element
   ----------------------------------
   function Type_Of_Array_Element (
      ObjPtr : in Tcl.Tcl_Obj) return String is
   --
      use type Tcl.Tcl_Obj;
   begin -- Type_Of_Array_Element (
      if ObjPtr = null then
         return "null";
      end if;
      return CHelper.Value (Tcl.Tcl_GetObjTypeName (ObjPtr));
   end Type_Of_Array_Element;

   function Element_Is_String (
      TArray : in Tash_Array;
      Index  : in String) return Boolean is
   --
      Interp : Tcl.Tcl_Interp;
      Result : Boolean;
   begin -- Element_Is_String
      Tash_Interp.Get (Interp);
      Result := Type_Of_Array_Element (
         Get_Element (Interp, TArray.Obj, Index)) /= "list";
      Tash_Interp.Release (Interp);
      return Result;
   exception
      when others => return False;
   end Element_Is_String;

   function Element_Is_List (
      TArray : in Tash_Array;
      Index  : in String) return Boolean is
   --
      Interp : Tcl.Tcl_Interp;
      Result : Boolean;
   begin -- Element_Is_List
      Tash_Interp.Get (Interp);
      Result := Type_Of_Array_Element (
         Get_Element (Interp, TArray.Obj, Index)) = "list";
      Tash_Interp.Release (Interp);
      return Result;
   exception
      when others => return False;
   end Element_Is_List;

   function Get_Elements (
      TArray  : in Tash_Array;
      Pattern : in String := "") return Tash.Lists.Tash_List is

      Objc       : Interfaces.C.int := 4;
      Objv       : Tcl.Tcl_Obj_Array (1 .. Objc);
      Interp     : Tcl.Tcl_Interp;
      Result     : Interfaces.C.int;
      InterpObj  : Tcl.Tcl_Obj;

   begin -- Get_Elements

      if Tash.Arrays.Is_Empty (TArray) then
         raise Array_Error;
      end if;

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Get_Option;
      Objv (3) := TArray.Obj;
      if Pattern = "" then
         Objc := 3;
      else
         Objv (4) := Tash.To_Tcl_Obj (Pattern);
      end if;

      --  Get the array elements in Interp's result object
      --------------------------------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      if Objc = 4 then
         Tcl.Tcl_DecrRefCount (Objv (4));
      end if;
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);

      --  Return the list in the Interp's result object.
      -------------------------------------------------
      InterpObj  := Tcl.Tcl_GetObjResult (Interp);
      Tcl.Tcl_IncrRefCount (InterpObj);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);
      return Tash.Lists.Tash_List'(
         (Ada.Finalization.Controlled with
            Obj  => InterpObj));

   end Get_Elements;

   function Get_Sorted_Elements (
      TArray  : in Tash_Array;
      Pattern : in String := "";
      Mode    : in Tash.Lists.Sort_Mode := Tash.Lists.SM_ASCII;
      Order   : in Tash.Lists.Ordering  := Tash.Lists.Increasing)
         return Tash.Lists.Tash_List is

      Return_List : Tash.Lists.Tash_List;

   begin -- Get_Sorted_Elements

      if Tash.Arrays.Is_Empty (TArray) then
         return Tash.Lists.Null_Tash_List;
      end if;

      --  Get and sort the indices, then get the
      --  array elements in order of the sorted
      --  indices.
      -----------------------------------------
      declare
         Sorted_Indices : constant Tash.Lists.Tash_List :=
            Tash.Lists.Sort (
               List  => Get_Indices (TArray, Pattern),
               Mode  => Mode,
               Order => Order);
      begin
         for I in 1 .. Tash.Lists.Length (Sorted_Indices) loop
            declare
               Index   : constant String :=
                 Tash.Lists.Get_Element (Sorted_Indices, I);
               Element : constant String :=
                 Get_Element (TArray, Index);
            begin
               Tash.Lists.Append (Return_List, Index);
               Tash.Lists.Append (Return_List, Element);
            end;
         end loop;
      end;

      --  return the sorted indices for now for testing
      ------------------------------------------------
      return Return_List;

   end Get_Sorted_Elements;

   function Get_Indices (
      TArray  : in Tash_Array;
      Pattern : in String := "") return String is

      Objc      : Interfaces.C.int := 4;
      Objv      : Tcl.Tcl_Obj_Array (1 .. Objc);
      Interp    : Tcl.Tcl_Interp;
      Result    : Interfaces.C.int;
      InterpObj : Tcl.Tcl_Obj;
      Length    : aliased Interfaces.C.int;

   begin -- Get_Indices

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Names_Option;
      Objv (3) := TArray.Obj;
      if Pattern = "" then
         Objc := 3;
      else
         Objv (4) := Tash.To_Tcl_Obj (Pattern);
      end if;

      --  Get array indices
      -------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      if Objc = 4 then
         Tcl.Tcl_DecrRefCount (Objv (4));
      end if;
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      InterpObj := Tcl.Tcl_GetObjResult (Interp);

      --  Convert indices string to a Tash list
      ----------------------------------------
      declare
         Indices : constant String := CHelper.Value (
            Tcl.Tcl_GetStringFromObj (InterpObj, Length'Access));
      begin
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Indices;
      end;

   end Get_Indices;

   function Get_Indices (
      TArray  : in Tash_Array;
      Pattern : in String := "") return Tash.Lists.Tash_List is
   --
      Indices : constant String := Get_Indices (TArray, Pattern);
   begin -- Get_Indices
      return Tash.Lists.To_Tash_List (Indices);
   end Get_Indices;

   procedure Set_Element (
      TArray : in out Tash_Array;
      Index  : in     String;
      Value  : in     String) is

      Count   : Natural;
      New_Obj : Tcl.Tcl_Obj;
      Objc    : constant Interfaces.C.int := 4;
      Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
      Listc   : constant Interfaces.C.int := 2;
      Listv   : Tcl.Tcl_Obj_Array (1 .. Listc);
      ListObj : Tcl.Tcl_Obj;
      Result  : Interfaces.C.int;
      Interp  : Tcl.Tcl_Interp;

      use type Tcl.Tcl_Obj;

   begin -- Set_Element

      if TArray.Obj = null then
         --  A Tash array is merely a unique variable name for a
         --  Tcl array.  We will now create a Tcl object and a
         --  unique variable name string to store in it.
         ------------------------------------------------------
         Variable_Count.Next (Count);
         New_Obj := Tash.To_Tcl_Obj ("array" &
            Ada.Strings.Fixed.Trim (Natural'Image (Count), Ada.Strings.Left));
         TArray := (Ada.Finalization.Controlled with
                    Obj  => New_Obj);
      end if;

      --  Create a Tcl list to hold the index and value
      ------------------------------------------------
      Listv (1) := Tash.To_Tcl_Obj (Index);
      Listv (2) := Tash.To_Tcl_Obj (Value);
      ListObj  := Tcl.Tcl_NewListObj (objc => Listc, objv => Listv);
      Tcl.Tcl_IncrRefCount (ListObj);
      Tcl.Tcl_DecrRefCount (Listv (1));

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Set_Option;
      Objv (3) := TArray.Obj;
      Objv (4) := ListObj;

      --  Set the array element
      ------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tcl.Tcl_DecrRefCount (ListObj);
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Element;

   procedure Set_Element (
      TArray : in out Tash_Array;
      Index  : in     String;
      Value  : in     Tash.Lists.Tash_List) is

      Count   : Natural;
      New_Obj : Tcl.Tcl_Obj;
      Objc    : constant Interfaces.C.int := 4;
      Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
      Listc   : constant Interfaces.C.int := 2;
      Listv   : Tcl.Tcl_Obj_Array (1 .. Listc);
      ListObj : Tcl.Tcl_Obj;
      Result  : Interfaces.C.int;
      Interp  : Tcl.Tcl_Interp;

      use type Tcl.Tcl_Obj;

   begin -- Set_Element

      if TArray.Obj = null then
         --  A Tash array is merely a unique variable name for a
         --  Tcl array.  We will now create a Tcl object and a
         --  unique variable name string to store in it.
         ------------------------------------------------------
         Variable_Count.Next (Count);
         New_Obj := Tash.To_Tcl_Obj ("array" &
            Ada.Strings.Fixed.Trim (Natural'Image (Count), Ada.Strings.Left));
         TArray := (Ada.Finalization.Controlled with
                    Obj  => New_Obj);
      end if;

      --  Create a Tcl list to hold the index and value
      ------------------------------------------------
      Listv (1) := Tash.To_Tcl_Obj (Index);
      Listv (2) := Tash.Tash_Object (Value).Obj;
      ListObj  := Tcl.Tcl_NewListObj (objc => Listc, objv => Listv);
      Tcl.Tcl_IncrRefCount (ListObj);
      Tcl.Tcl_DecrRefCount (Listv (1));

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Set_Option;
      Objv (3) := TArray.Obj;
      Objv (4) := ListObj;

      --  Set the array element
      ------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tcl.Tcl_DecrRefCount (ListObj);
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Element;

   procedure Set_Elements (
      TArray : in out Tash_Array;
      List   : in     Tash.Lists.Tash_List) is

      Count   : Natural;
      New_Obj : Tcl.Tcl_Obj;
      Objc    : constant Interfaces.C.int := 4;
      Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result  : Interfaces.C.int;
      Interp  : Tcl.Tcl_Interp;

      use type Tcl.Tcl_Obj;

   begin -- Set_Elements

      if TArray.Obj = null then
         --  A Tash array is merely a unique variable name for a
         --  Tcl array.  We will now create a Tcl object and a
         --  unique variable name string to store in it.
         ------------------------------------------------------
         Variable_Count.Next (Count);
         New_Obj := Tash.To_Tcl_Obj ("array" &
            Ada.Strings.Fixed.Trim (Natural'Image (Count), Ada.Strings.Left));
         TArray := (Ada.Finalization.Controlled with
                    Obj  => New_Obj);
      end if;

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Set_Option;
      Objv (3) := TArray.Obj;
      Objv (4) := Tash.Tash_Object (List).Obj;

      --  Set the array elements
      -------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Elements;

   procedure Delete_Element (
      TArray : in Tash_Array;
      Index  : in String) is

      Objc    : constant Interfaces.C.int := 4;
      Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
      Interp  : Tcl.Tcl_Interp;
      Result  : Interfaces.C.int;

      use type Tcl.Tcl_Obj;

   begin -- Delete_Element

      if TArray.Obj = null then
         return;
      end if;

      --  Create object parameters for Tcl_ArrayObjCmd call
      ----------------------------------------------------
      Objv (1) := Array_Cmd;
      Objv (2) := Unset_Option;
      Objv (3) := TArray.Obj;
      Objv (4) := Tash.To_Tcl_Obj (Index);

      --  Use Tcl_ArrayObjCmd to delete the
      --  element indexed by Index
      ------------------------------------
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result  := Tcl_ArrayObjCmd (
         dummy  => System.Null_Address,
         interp => Interp,
         objc   => Objc,
         objv   => Objv);
      Tcl.Tcl_DecrRefCount (Objv (4));
      Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Delete_Element;

   package body Generic_Integer_Arrays is

      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj;

      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj is
         New_Obj : Tcl.Tcl_Obj;
      begin -- To_Tcl_Obj
         New_Obj := Tcl.Tcl_NewIntObj (Interfaces.C.int (Num));
         Tcl.Tcl_IncrRefCount (New_Obj);
         return New_Obj;
      end To_Tcl_Obj;

      function To_Tash_Array (
         Index : in String;
         Num   : in Item) return Tash_Array is

         Count   : Natural;
         New_Obj : Tcl.Tcl_Obj;
         TArray  : Tash_Array;
         Objc    : constant Interfaces.C.int := 4;
         Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
         Listc   : constant Interfaces.C.int := 2;
         Listv   : Tcl.Tcl_Obj_Array (1 .. Listc);
         ListObj : Tcl.Tcl_Obj;
         Result  : Interfaces.C.int;
         Interp  : Tcl.Tcl_Interp;

      begin -- To_Tash_Array

         --  A Tash array is merely a unique variable name for a
         --  Tcl array.  We will now create a Tcl object and a
         --  unique variable name string to store in it.
         ------------------------------------------------------
         Variable_Count.Next (Count);
         New_Obj := Tash.To_Tcl_Obj ("array" &
            Ada.Strings.Fixed.Trim (Natural'Image (Count), Ada.Strings.Left));
         TArray := (Ada.Finalization.Controlled with
                    Obj  => New_Obj);

         --  Create a Tcl list to hold the index and value
         ------------------------------------------------
         Listv (1) := Tash.To_Tcl_Obj (Index);
         Listv (2) := To_Tcl_Obj (Num);
         ListObj  := Tcl.Tcl_NewListObj (objc => Listc, objv => Listv);
         Tcl.Tcl_IncrRefCount (ListObj);
         Tcl.Tcl_DecrRefCount (Listv (1));

         --  Create object parameters for Tcl_ArrayObjCmd call
         ----------------------------------------------------
         Objv (1) := Array_Cmd;
         Objv (2) := Set_Option;
         Objv (3) := TArray.Obj;
         Objv (4) := ListObj;

         --  Set the array element
         ------------------------
         Tash_Interp.Get (Interp);
         Tcl.Tcl_ResetResult (Interp);
         Result  := Tcl_ArrayObjCmd (
            dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
         Tcl.Tcl_DecrRefCount (ListObj);
         Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return TArray;

      end To_Tash_Array;

      function Get_Element (
         TArray : in Tash_Array;
         Index  : in String) return Item is
      --
         Interp  : Tcl.Tcl_Interp;
      begin -- Get_Element
         Tash_Interp.Get (Interp);
         declare
            Obj : constant Tcl.Tcl_Obj :=
              Get_Element (Interp, TArray.Obj, Index);
            Value  : aliased Interfaces.C.int;
            Result : Interfaces.C.int;
            pragma Unreferenced (Result);  -- XXX this doesn't seem right!
         begin
            if Type_Of_Array_Element (Obj) /= "int" then
               Tash_Interp.Raise_Exception (
                  Interp  => Interp,
                  E       => Constraint_Error'Identity,
                  Message => "Element is not an integer");
            end if;
            Result := Tcl.Tcl_GetIntFromObj (
               interp => Interp,
               objPtr => Obj,
               intPtr => Value'Access);
            Tash_Interp.Release (Interp);
            return Item (Value);
         end;
      end Get_Element;

      function Element_Is_Integer (
         TArray : in Tash_Array;
         Index  : in String) return Boolean is
      --
         Interp : Tcl.Tcl_Interp;
         Result : Boolean;
      begin -- Element_Is_Integer
         Tash_Interp.Get (Interp);
         Result := Type_Of_Array_Element (
            Get_Element (Interp, TArray.Obj, Index)) = "int";
         Tash_Interp.Release (Interp);
         return Result;
      exception
         when others => return False;
      end Element_Is_Integer;

      procedure Set_Element (
         TArray : in out Tash_Array;
         Index  : in     String;
         Value  : in     Item) is

         Count   : Natural;
         New_Obj : Tcl.Tcl_Obj;
         Objc    : constant Interfaces.C.int := 4;
         Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
         Listc   : constant Interfaces.C.int := 2;
         Listv   : Tcl.Tcl_Obj_Array (1 .. Listc);
         ListObj : Tcl.Tcl_Obj;
         Result  : Interfaces.C.int;
         Interp  : Tcl.Tcl_Interp;

         use type Tcl.Tcl_Obj;

      begin -- Set_Element

         if TArray.Obj = null then
            --  A Tash array is merely a unique variable name for a
            --  Tcl array.  We will now create a Tcl object and a
            --  unique variable name string to store in it.
            ------------------------------------------------------
            Variable_Count.Next (Count);
            New_Obj := Tash.To_Tcl_Obj
              ("array" &
               Ada.Strings.Fixed.Trim (Natural'Image (Count),
                                       Ada.Strings.Left));
            TArray := (Ada.Finalization.Controlled with
                       Obj  => New_Obj);
         end if;

         --  Create a Tcl list to hold the index and value
         ------------------------------------------------
         Listv (1) := Tash.To_Tcl_Obj (Index);
         Listv (2) := To_Tcl_Obj (Value);
         ListObj  := Tcl.Tcl_NewListObj (objc => Listc, objv => Listv);
         Tcl.Tcl_IncrRefCount (ListObj);
         Tcl.Tcl_DecrRefCount (Listv (1));

         --  Create object parameters for Tcl_ArrayObjCmd call
         ----------------------------------------------------
         Objv (1) := Array_Cmd;
         Objv (2) := Set_Option;
         Objv (3) := TArray.Obj;
         Objv (4) := ListObj;

         --  Set the array element
         ------------------------
         Tash_Interp.Get (Interp);
         Tcl.Tcl_ResetResult (Interp);
         Result  := Tcl_ArrayObjCmd (
            dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
         Tcl.Tcl_DecrRefCount (ListObj);
         Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);

      end Set_Element;

   end Generic_Integer_Arrays;

   package body Generic_Float_Arrays is

      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj;
      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj is
         New_Obj : Tcl.Tcl_Obj;
      begin -- To_Tcl_Obj
         New_Obj := Tcl.Tcl_NewDoubleObj (Interfaces.C.double (Num));
         Tcl.Tcl_IncrRefCount (New_Obj);
         return New_Obj;
      end To_Tcl_Obj;

      function To_Tash_Array (
         Index : in String;
         Num   : in Item) return Tash_Array is

         Count   : Natural;
         New_Obj : Tcl.Tcl_Obj;
         TArray  : Tash_Array;
         Objc    : constant Interfaces.C.int := 4;
         Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
         Listc   : constant Interfaces.C.int := 2;
         Listv   : Tcl.Tcl_Obj_Array (1 .. Listc);
         ListObj : Tcl.Tcl_Obj;
         Result  : Interfaces.C.int;
         Interp  : Tcl.Tcl_Interp;

      begin -- To_Tash_Array

         --  A Tash array is merely a unique variable name for a
         --  Tcl array.  We will now create a Tcl object and a
         --  unique variable name string to store in it.
         ------------------------------------------------------
         Variable_Count.Next (Count);
         New_Obj := Tash.To_Tcl_Obj ("array" &
            Ada.Strings.Fixed.Trim (Natural'Image (Count), Ada.Strings.Left));
         TArray := (Ada.Finalization.Controlled with
                    Obj  => New_Obj);

         --  Create a Tcl list to hold the index and value
         ------------------------------------------------
         Listv (1) := Tash.To_Tcl_Obj (Index);
         Listv (2) := To_Tcl_Obj (Num);
         ListObj  := Tcl.Tcl_NewListObj (objc => Listc, objv => Listv);
         Tcl.Tcl_IncrRefCount (ListObj);
         Tcl.Tcl_DecrRefCount (Listv (1));

         --  Create object parameters for Tcl_ArrayObjCmd call
         ----------------------------------------------------
         Objv (1) := Array_Cmd;
         Objv (2) := Set_Option;
         Objv (3) := TArray.Obj;
         Objv (4) := ListObj;

         --  Set the array element
         ------------------------
         Tash_Interp.Get (Interp);
         Tcl.Tcl_ResetResult (Interp);
         Result  := Tcl_ArrayObjCmd (
            dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
         Tcl.Tcl_DecrRefCount (ListObj);
         Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return TArray;

      end To_Tash_Array;

      function Get_Element (
         TArray : in Tash_Array;
         Index  : in String) return Item is
      --
         Interp  : Tcl.Tcl_Interp;
      begin -- Get_Element
         Tash_Interp.Get (Interp);
         declare
            Obj : constant Tcl.Tcl_Obj :=
              Get_Element (Interp, TArray.Obj, Index);
            Value  : aliased Interfaces.C.double;
            Result : Interfaces.C.int;
            pragma Unreferenced (Result);  -- XXX this doesn't seem right!
         begin
            if Type_Of_Array_Element (Obj) /= "double" then
               Tash_Interp.Raise_Exception (
                  Interp  => Interp,
                  E       => Constraint_Error'Identity,
                  Message => "Element is not an float");
            end if;
            Result := Tcl.Tcl_GetDoubleFromObj (
               interp    => Interp,
               objPtr    => Obj,
               doublePtr => Value'Access);
            Tash_Interp.Release (Interp);
            return Item (Value);
         end;
      end Get_Element;

      function Element_Is_Float (
         TArray : in Tash_Array;
         Index  : in String) return Boolean is
      --
         Interp : Tcl.Tcl_Interp;
         Result : Boolean;
      begin -- Element_Is_Float
         Tash_Interp.Get (Interp);
         Result := Type_Of_Array_Element (
            Get_Element (Interp, TArray.Obj, Index)) = "double";
         Tash_Interp.Release (Interp);
         return Result;
      exception
         when others => return False;
      end Element_Is_Float;

      procedure Set_Element (
         TArray : in out Tash_Array;
         Index  : in     String;
         Value  : in     Item) is

         Count   : Natural;
         New_Obj : Tcl.Tcl_Obj;
         Objc    : constant Interfaces.C.int := 4;
         Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
         Listc   : constant Interfaces.C.int := 2;
         Listv   : Tcl.Tcl_Obj_Array (1 .. Listc);
         ListObj : Tcl.Tcl_Obj;
         Result  : Interfaces.C.int;
         Interp  : Tcl.Tcl_Interp;

         use type Tcl.Tcl_Obj;

      begin -- Set_Element

         if TArray.Obj = null then
            --  A Tash array is merely a unique variable name for a
            --  Tcl array.  We will now create a Tcl object and a
            --  unique variable name string to store in it.
            ------------------------------------------------------
            Variable_Count.Next (Count);
            New_Obj := Tash.To_Tcl_Obj
              ("array" &
               Ada.Strings.Fixed.Trim (Natural'Image (Count),
                                       Ada.Strings.Left));
            TArray := (Ada.Finalization.Controlled with
                       Obj  => New_Obj);
         end if;

         --  Create a Tcl list to hold the index and value
         ------------------------------------------------
         Listv (1) := Tash.To_Tcl_Obj (Index);
         Listv (2) := To_Tcl_Obj (Value);
         ListObj  := Tcl.Tcl_NewListObj (objc => Listc, objv => Listv);
         Tcl.Tcl_IncrRefCount (ListObj);
         Tcl.Tcl_DecrRefCount (Listv (1));

         --  Create object parameters for Tcl_ArrayObjCmd call
         ----------------------------------------------------
         Objv (1) := Array_Cmd;
         Objv (2) := Set_Option;
         Objv (3) := TArray.Obj;
         Objv (4) := ListObj;

         --  Set the array element
         ------------------------
         Tash_Interp.Get (Interp);
         Tcl.Tcl_ResetResult (Interp);
         Result  := Tcl_ArrayObjCmd (
            dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
         Tcl.Tcl_DecrRefCount (ListObj);
         Tash_Interp.Assert (Interp, Result, Array_Error'Identity);
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);

      end Set_Element;

   end Generic_Float_Arrays;

   function Internal_Rep (
      TArray : in Tash_Array) return String is

      Name          : constant String := Internal_Name (TArray);
      Num_Elements  : Integer;
      Indices       : Tash.Lists.Tash_List;
      Max_Index_Len : Natural := 0;
      Image         : Ada.Strings.Unbounded.Unbounded_String;
      Interp        : Tcl.Tcl_Interp;
      Elem          : Tcl.Tcl_Obj;
      Elements      : Tash.Lists.Tash_List;
      pragma Unreferenced (Elements);  -- XXX why do we get it then?

   begin -- Internal_Rep

      --  Get the internal representation of the array itself
      -----------------------------------------------------
      Image := Ada.Strings.Unbounded.To_Unbounded_String (
         Tash.Internal_Rep (Tash_Object (TArray)));

      Elements := Get_Elements (TArray);
      if True then
         return Ada.Strings.Unbounded.To_String (Image);
      end if;

      --  Find max length of the indices
      ---------------------------------
      Num_Elements := Tash.Lists.Length (Indices);
      for I in 1 .. Num_Elements loop
         declare
            Index  : constant String := Tash.Lists.Get_Element (Indices, I);
         begin
            if Index'Length > Max_Index_Len then
               Max_Index_Len := Index'Length;
            end if;
         end;
      end loop;

      --  Append index and internal rep of each element.
      --  Note that we use the low-level internal rep of the
      --  Tcl object, not the Tash Object.  This is because
      --  if we declare Elem as Tash_Object'Class, we'll
      --  increment the reference count and show a confusingly
      --  high reference count.
      ------------------------------------------------
      for I in 1 .. Num_Elements loop
         declare
            Index : constant String := Tash.Lists.Get_Element (Indices, I);
         begin
            Tash_Interp.Get (Interp);
            Elem := Get_Element (Interp, TArray.Obj, Index);
            Tash_Interp.Release (Interp);
            Ada.Text_IO.Put_Line
              ("TA.Internal_Rep: i=" & Integer'Image (I) &
               " index=" & Index &
               " value=" & Tash.Image (Elem) &
               " count=" & Interfaces.C.int'Image (Tcl.Tcl_GetRefCount
                                                     (Elem)));
            Ada.Strings.Unbounded.Append (Image,
               ASCII.LF & "   " & Name & "(" & Index & ")" &
               (Index'Length + 1 .. Max_Index_Len => ' ') &
               "=" & Tash.Internal_Rep (Elem));
            --  Tcl.Tcl_DecrRefCount (Elem);
         end;
      end loop;

      return Ada.Strings.Unbounded.To_String (Image);

   end Internal_Rep;

   function Internal_Name (
      TArray : in Tash_Array) return String is
   --
      Length : aliased Interfaces.C.int;
   begin -- Internal_Name
      if Is_Null (TArray) then
         return "";
      else
         return CHelper.Value (Tcl.Tcl_GetStringFromObj (
            TArray.Obj, Length'Access));
      end if;
   end Internal_Name;

   procedure Finalize (
      TArray : in out Tash_Array) is

      Ref_Count : Interfaces.C.int;
      Indices   : Tash.Lists.Tash_List;

      use type Tcl.Tcl_Obj;

   begin -- Finalize

      if TArray.Obj = null then
         return;
      end if;
      Ref_Count := Tcl.Tcl_GetRefCount (TArray.Obj);
      if Ref_Count = 1  then
         --  array is now being de-referenced;
         --  decrement count of each element
         ------------------------------------
         Indices := Get_Indices (TArray);
         for I in 1 .. Tash.Lists.Length (Indices) loop
            declare
               Index   : constant String :=
                  Tash.Lists.Get_Element (Indices, I);
               Element : Tash.Tash_Object'Class :=
                  Get_Element (TArray, Index);
            begin
               Tash.Finalize (Element);
            end;
         end loop;
         Tcl.Tcl_DecrRefCount (TArray.Obj);
      else
         Tcl.Tcl_DecrRefCount (TArray.Obj);
      end if;
   end Finalize;

end Tash.Arrays;
