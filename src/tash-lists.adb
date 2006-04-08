--------------------------------------------------------------------
--
-- Unit Name:    Tash.Lists body
--
-- File Name:    tash-lists.adb
--
-- Purpose:      Defines the Tash list type which may
--               contain any Tash object, including lists.
--
-- Copyright (c) 1999 Terry J. Westley
--
-- Tash is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free
-- Software Foundation; either version 2, or (at your option) any later
-- version. Tash is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. You should have received a copy of the GNU General
-- Public License distributed with Tash; see file COPYING. If not, write to
--
--          Software Foundation
--          59 Temple Place - Suite 330
--          Boston, MA 02111-1307, USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Tash is maintained by Terry Westley (http://www.adatcl.com).
--
--------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Tags;
with Ada.Integer_Text_IO;
with CArgv;
with CHelper;
with Interfaces.C.Strings;
with System;
with Tash.Floats;
with Tash.Integers;
with Tash.Test;
with Tash.Strings;

package body Tash.Lists is

   use type Interfaces.C.Int;
   use type Interfaces.C.Strings.Chars_Ptr;
   use type Ada.Strings.Direction;
   use type Ada.Tags.Tag;

   Int_Type_Name    : constant String := "int";
   List_Type_Name   : constant String := "list";
   Double_Type_Name : constant String := "double";

   function Is_Empty (
      List : in Tash_List) return Boolean is
   begin -- Is_Empty
      return Is_Null (List) or else Length (List) = 0;
   end Is_Empty;
   pragma Inline (Is_Empty);

   -- Get length of a Tcl list object; return 0 if null
   ----------------------------------------------------
   function Length (
      Interp : in Tcl.Tcl_Interp;
      Obj    : in Tcl.Tcl_Obj) return Interfaces.C.Int is
   --
      Result : Interfaces.C.Int;
      IntPtr : aliased Interfaces.C.Int;
   begin -- Length
      if Tcl.Is_Null (Obj) then
         return 0;
      else
         Result := Tcl.Tcl_ListObjLength (
            Interp  => Interp,
            ListPtr => Obj,
            IntPtr  => IntPtr'Access);
         Tash_Interp.Assert (Interp, Result, List_Error'identity);
         return IntPtr;
      end if;
   end Length;

   function Strcmp (
      S1 : in Interfaces.C.Strings.Chars_Ptr;
      S2 : in Interfaces.C.Strings.Chars_Ptr) return Interfaces.C.Int;
   pragma Import (C, Strcmp, "strcmp");

   -- Convert a Tcl object to the Tash tagged type which matches
   -- the Tcl object's internal data type.  This is used to
   -- convert list elements to correct Tash data type.
   -------------------------------------------------------------
   function Convert_To_Tagged_Type (
      Obj : in Tcl.Tcl_Obj_Ptr) return Tash.Tash_Object'Class is
   --
      use type Tcl.Tcl_Obj_Ptr;
      Object_Type : Interfaces.C.Strings.Chars_Ptr;
   begin -- Convert_To_Tagged_Type
      if Obj = Tcl.Null_Tcl_Obj_Ptr then
         return Tash.Strings.Null_Tash_String;
      end if;
      if Tcl.Is_Null (Obj.all) then
         return Tash.Strings.Null_Tash_String;
      end if;
      --Tcl.Tcl_IncrRefCount (Obj.all);
      Object_Type := Tcl.Tcl_GetObjTypeName (Obj.all);
      if CHelper."=" (Object_Type, Int_Type_Name) then
         return Tash.Integers.Tash_Integer'(
            (Ada.Finalization.Controlled with Obj => Obj.all));
      elsif CHelper."=" (Object_Type, Double_Type_Name) then
         return Tash.Floats.Tash_Float'(
            (Ada.Finalization.Controlled with Obj => Obj.all));
      elsif CHelper."=" (Object_Type, List_Type_Name) then
         return Tash.Lists.Tash_List'(
            (Ada.Finalization.Controlled with Obj => Obj.all));
      else
         return Tash.Strings.Tash_String'(
            (Ada.Finalization.Controlled with Obj => Obj.all));
      end if;
   end Convert_To_Tagged_Type;

   -- Get an element of a Tcl list.
   -------------------------------
   function Element (
      Interp : in Tcl.Tcl_Interp;
      List   : in Tcl.Tcl_Obj;
      Index  : in Positive) return Tcl.Tcl_Obj_Ptr is
   --
      Result     : Interfaces.C.Int;
      Length     : Interfaces.C.Int := Tash.Lists.Length (Interp, List);
      New_Object : Tcl.Tcl_Obj_Ptr;
   begin -- Element
      if Tcl.Is_Null (List) then
         return Tcl.Null_Tcl_Obj_Ptr;
      elsif Length < Interfaces.C.Int (Index) then
         return Tcl.Null_Tcl_Obj_Ptr;
      else
         New_Object := new Tcl.Tcl_Obj;
         Result := Tcl.Tcl_ListObjIndex (
            interp    => Interp,
            listPtr   => List,
            index     => Interfaces.C.Int (Index-1),
            objPtrPtr => New_Object);
         Tash_Interp.Assert (Interp, Result, List_Error'identity);
         return New_Object;
      end if;
   end Element;

   -- Create a Tash list from a single Tash object
   -----------------------------------------------
   function To_Tash_List (
      Element : in Tash.Tash_Object'Class;
      Expand  : in Boolean := False) return Tash_List is

      New_List : Tash_List;
      Interp   : Tcl.Tcl_Interp;

   begin -- To_Tash_List

      if Is_Null (Element) then
         -- There's nothing from which to create a new list,
         -- so we just return an empty list.
         ---------------------------------------------------
         return New_List;
      end if;

      if Expand then

         Tash_Interp.Get (Interp);

         -- Create a new list containing each of the
         -- elements in the list Element.
         -------------------------------------------
         declare
            Objc       : Interfaces.C.Int := Length (Interp, Element.Obj);
            Objv       : Tcl.Tcl_Obj_Array (1..Objc);
            Obj_Ptr    : Tcl.Tcl_Obj_Ptr := new Tcl.Tcl_Obj;
            New_Object : Tcl.Tcl_Obj;
         begin

            -- Create an array of Tcl objects from list Element
            ---------------------------------------------------
            for I in Objv'Range loop
               Obj_Ptr := Tash.Lists.Element (Interp => Interp,
                                              List   => Element.Obj,
                                              Index  => Integer (I));
               Objv(I) := Obj_Ptr.all;
            end loop;

            -- Create new Tcl object and increment its reference count
            ----------------------------------------------------------
            New_Object := Tcl.Tcl_NewListObj (Objc => Objc, Objv => Objv);
            Tcl.Tcl_IncrRefCount (New_Object);

            -- Now, create the Tash object.
            ------------------------------------------------------------
            -- Warning: Don't combine these into one command as in:
            --
            -- New_List := (Ada.Finalization.Controlled with
            --    Obj => Tcl.Tcl_NewListObj (Objc => Objc, Objv => Objv));
            --
            -- because then Tcl gets a SIGSEGV while updating the
            -- string representation during Finalization.
            ------------------------------------------------------------
            New_List := (Ada.Finalization.Controlled with Obj => New_Object);
            Tash_Interp.Release (Interp);
            return New_List;

         end;
      end if;

      -- Create a new list containing the one element
      -----------------------------------------------
      declare
         Objv       : Tcl.Tcl_Obj_Array (1..1);
         New_Object : Tcl.Tcl_Obj;
      begin

         -- Create new Tcl object and increment its reference count
         ----------------------------------------------------------
         Objv(1)    := Element.Obj;
         New_Object := Tcl.Tcl_NewListObj (Objc => 1, Objv => Objv);
         Tcl.Tcl_IncrRefCount (New_Object);

         -- Now, create the Tash object.
         ------------------------------------------------------------
         -- Warning: Don't combine these into one command as in:
         --
         -- New_List := (Ada.Finalization.Controlled with
         --    Obj => Tcl.Tcl_NewListObj (Objc => 1, Objv => Objv));
         --
         -- because then Tcl gets a SIGSEGV while updating the
         -- string representation during Finalization.
         ------------------------------------------------------------
         New_List := (Ada.Finalization.Controlled with Obj => New_Object);
         return New_List;

      end;

   end To_Tash_List;

   function To_Tash_List (
      Str : in String) return Tash_List is

      Argc      : aliased Interfaces.C.Int;
      Argv      : aliased CArgv.Chars_Ptr_Ptr;
      Save_Argv : aliased CArgv.Chars_Ptr_Ptr;
      C_Str     : aliased Interfaces.C.Char_Array := Interfaces.C.To_C (Str);
      New_List  : Tash_List;
      New_Obj   : Tcl.Tcl_Obj;
      Objv      : Tcl.Tcl_Obj_Array (1..1);
      Result    : Interfaces.C.Int;
      Interp    : Tcl.Tcl_Interp;

      -- Tcl_Free is define for use as described
      -- in the Tcl_SplitList man page.
      ------------------------------------------
      procedure Tcl_Free (
         Ptr : in CArgv.Chars_Ptr_Ptr);
      pragma Import (C, Tcl_Free, "Tcl_Free");

   begin -- To_Tash_List

      if Str = "" then
         return New_List;
      end if;

      -- Tcl_SplitList creates a properly formed Tcl string
      -- including backslash substitution and braces.
      -----------------------------------------------------
      Tash_Interp.Get (Interp);
      Result := Tcl.Tcl_SplitList (
         Interp  => Interp,
         ListStr => Interfaces.C.Strings.To_Chars_Ptr (C_Str'Unchecked_Access),
         ArgcPtr => Argc'Unchecked_Access,
         ArgvPtr => Argv'Unchecked_Access);
      Tash_Interp.Assert (Interp, Result, List_Error'identity);

      -- Initialize New_List as an empty list
      ---------------------------------------
      if Argc > 0 then
         New_List.Obj := Tcl.Tcl_NewListObj (Objc => 0, Objv => Objv);
         Tcl.Tcl_IncrRefCount (New_List.Obj);
      end if;

      -- Append each string element created by Tcl_SplitList to New_List
      ------------------------------------------------------------------
      Save_Argv := Argv;
      for I in 1..Argc loop
         New_Obj := Tcl.Tcl_NewStringObj (
            Bytes  => Argv.all,
            Length => CHelper.Length (Argv.all));
         Result := Tcl.Tcl_ListObjAppendElement (
            Interp  => Interp,
            ListPtr => New_List.Obj,
            ObjPtr  => New_Obj);
         Tash_Interp.Assert (Interp, Result, List_Error'identity);
         CArgv.Argv_Pointer.Increment (Argv);
      end loop;

      -- Free space allocated for Argv by Tcl_SplitList
      -------------------------------------------------
      Tcl_Free (Save_Argv);

      Tash_Interp.Release (Interp);

      return New_List;

   end To_Tash_List;

   function To_String (
      TList : in Tash_List) return String is
   --
      Length : aliased Interfaces.C.Int;
   begin -- To_String
      if Is_Null (TList) then
         return "";
      else
         return CHelper.Value (
            Tcl.Tcl_GetStringFromObj (TList.Obj, Length'Access));
      end if;
   end To_String;

   function To_Tash_List (
      Element1 : in Tash.Tash_Object'Class;
      Element2 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element3 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element4 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element5 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element6 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element7 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element8 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element9 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Expand   : in Boolean                := False)
         return Tash_List is

      New_List : Tash_List;

   begin -- To_Tash_List
      New_List := To_Tash_List (Element1, Expand);
      Append (New_List, Element2, Expand);
      Append (New_List, Element3, Expand);
      Append (New_List, Element4, Expand);
      Append (New_List, Element5, Expand);
      Append (New_List, Element6, Expand);
      Append (New_List, Element7, Expand);
      Append (New_List, Element8, Expand);
      Append (New_List, Element9, Expand);
      return New_List;
   end To_Tash_List;

   function "+" (
      Element : in Tash.Tash_Object'Class) return Tash_List is
   begin -- "+"
      return To_Tash_List (Element => Element,
                           Expand  => False);
   end "+";

   function Tcl_SplitObjCmd (
      dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.Int;
      objv   : in Tcl.Tcl_Obj_Array
   ) return Interfaces.C.Int;
   pragma Import (C, Tcl_SplitObjCmd, "Tcl_SplitObjCmd");

   function Split (
      Str      : in String;
      Split_At : in String := " " & ASCII.CR & ASCII.HT & ASCII.LF)
         return Tash_List is
   --
      Objc     : Interfaces.C.Int := 3;
      Objv     : Tcl.Tcl_Obj_Array (1..Objc);
      Result   : Interfaces.C.Int;
      New_List : Tash_List;
      Interp   : Tcl.Tcl_Interp;
   begin -- Split
      Objv(1) := Tash.To_Tcl_Obj ("split");
      Objv(2) := Tash.To_Tcl_Obj (Str);
      Objv(3) := Tash.To_Tcl_Obj (Split_At);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result := Tcl_SplitObjCmd (
         Dummy  => System.Null_Address,
         Interp => Interp,
         Objc   => Objc,
         Objv   => Objv);
      for I in Objv(1..3)'Range loop
         Tcl.Tcl_DecrRefCount (Objv(I));
      end loop;
      Tash_Interp.Assert (Interp, Result, List_Error'identity);
      New_List.Obj := Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (New_List.Obj);
      return New_List;
   end Split;

   function Split (
      TObject  : in Tash.Tash_object'class;
      Split_At : in String := " " & ASCII.CR & ASCII.HT & ASCII.LF)
         return Tash_List is
   --
      Objc     : Interfaces.C.Int := 3;
      Objv     : Tcl.Tcl_Obj_Array (1..Objc);
      Result   : Interfaces.C.Int;
      New_List : Tash_List;
      Interp   : Tcl.Tcl_Interp;
   begin -- Split
      Objv(1) := Tash.To_Tcl_Obj ("split");
      Objv(2) := Tash.To_Tcl_Obj (Tash.To_String (TObject));
      Objv(3) := Tash.To_Tcl_Obj (Split_At);
      Tash_Interp.Get (Interp);
      Result  := Tcl_SplitObjCmd (
         Dummy  => System.Null_Address,
         Interp => Interp,
         Objc   => Objc,
         Objv   => Objv);
      for I in Objv(1..3)'Range loop
         Tcl.Tcl_DecrRefCount (Objv(I));
      end loop;
      Tash_Interp.Assert (Interp, Result, List_Error'identity);
      New_List.Obj := Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (New_List.Obj);
      return New_List;
   end Split;

   function Slice (
      List  : in Tash_List;
      First : in Positive;
      Last  : in Natural) return Tash_List is
   --
      New_List : Tash_List;
   begin -- Slice
      for I in First..Last loop
         Append (
            List    => New_List,
            Element => Element_Obj (List, I),
            Expand  => False);
      end loop;
      return New_List;
   end Slice;

   function Length (
      List   : in Tash_List) return Natural is
   --
      Len    : Natural;
      Interp : Tcl.Tcl_Interp;
   begin -- Length
      if Is_Null (List) then
         return 0;
      else
         Tash_Interp.Get (Interp);
         Len := Natural (Length (Interp, List.Obj));
         Tash_Interp.Release (Interp);
         return Len;
      end if;
   end Length;

   function Element (
      List  : in Tash_List;
      Index : in Positive) return Tash.Floats.Tash_Float is
   --
      Interp : Tcl.Tcl_Interp;
   begin -- Element
      if Is_Null (List) or else Index > Length (List) then
         return Tash.Floats.Null_Tash_Float;
      else
         Tash_Interp.Get (Interp);
         declare
            Obj         : Tcl.Tcl_Obj_Ptr := Element (Interp, List.Obj, Index);
            Object_Type : Interfaces.C.Strings.Chars_Ptr;
         begin
            Object_Type := Tcl.Tcl_GetObjTypeName (Obj.all);
            Tash_Interp.Release (Interp);
            if CHelper."=" (Object_Type, Double_Type_Name) then
               Tcl.Tcl_IncrRefCount (Obj.all);
               return Tash.Floats.Tash_Float'(
                  (Ada.Finalization.Controlled with Obj => Obj.all));
            else
               raise List_Error;
            end if;
         end;
      end if;
   end Element;

   function Element (
      List  : in Tash_List;
      Index : in Positive) return Tash.Integers.Tash_Integer is
   --
      Interp : Tcl.Tcl_Interp;
   begin -- Element
      if Is_Null (List) or else Index > Length (List) then
         return Tash.Integers.Null_Tash_Integer;
      else
         Tash_Interp.Get (Interp);
         declare
            Obj         : Tcl.Tcl_Obj_Ptr := Element (Interp, List.Obj, Index);
            Object_Type : Interfaces.C.Strings.Chars_Ptr;
         begin
            Object_Type := Tcl.Tcl_GetObjTypeName (Obj.all);
            Tash_Interp.Release (Interp);
            if CHelper."=" (Object_Type, Int_Type_Name) then
               Tcl.Tcl_IncrRefCount (Obj.all);
               return Tash.Integers.Tash_Integer'(
                  (Ada.Finalization.Controlled with Obj => Obj.all));
            else
               raise List_Error;
            end if;
         end;
      end if;
   end Element;

   function Element (
      List  : in Tash_List;
      Index : in Positive) return Tash.Strings.Tash_String is
   --
      Interp : Tcl.Tcl_Interp;
   begin -- Element
      if Is_Null (List) or else Index > Length (List) then
         return Tash.Strings.Null_Tash_String;
      else
         Tash_Interp.Get (Interp);
         declare
            Obj : Tcl.Tcl_Obj_Ptr := Element (Interp, List.Obj, Index);
         begin
            Tash_Interp.Release (Interp);
            Tcl.Tcl_IncrRefCount (Obj.all);
            return Tash.Strings.Tash_String'(
               (Ada.Finalization.Controlled with Obj => Obj.all));
         end;
      end if;
   end Element;

   function Element_Obj (
      List  : in Tash_List;
      Index : in Positive) return Tash.Tash_Object'Class is
   --
      Interp : Tcl.Tcl_Interp;
   begin -- Element_Obj
      if Is_Null (List) or else Index > Length (List) then
         return Tash.Strings.Null_Tash_String;
      else
         Tash_Interp.Get (Interp);
         declare
            Result : Tash.Tash_Object'Class :=
              Convert_To_Tagged_Type (Element (Interp, List.Obj, Index));
         begin
            Tash_Interp.Release (Interp);
            return Result;
         end;
      end if;
   end Element_Obj;

   function Head (
      List : in Tash_List) return Tash.Integers.Tash_Integer is
   begin -- Head
      return Element (List, 1);
   end Head;

   function Head (
      List : in Tash_List) return Tash.Floats.Tash_Float is
   begin -- Head
      return Element (List, 1);
   end Head;

   function Head (
      List : in Tash_List) return Tash.Strings.Tash_String is
   begin -- Head
      return Element (List, 1);
   end Head;

   function Head_Obj (
      List : in Tash_List) return Tash.Tash_Object'Class is
   begin -- Head_Obj
      return Element_Obj (List, 1);
   end Head_Obj;

   function Tail (
      List : in Tash_List) return Tash.Integers.Tash_Integer is
   --
      Length : Natural := Tash.Lists.Length (List);
   begin -- Tail
      if Length = 0 then
         return Tash.Integers.Null_Tash_Integer;
      else
         return Element (List, Length);
      end if;
   end Tail;

   function Tail (
      List : in Tash_List) return Tash.Floats.Tash_Float is
   --
      Length : Natural := Tash.Lists.Length (List);
   begin -- Tail
      if Length = 0 then
         return Tash.Floats.Null_Tash_Float;
      else
         return Element (List, Length);
      end if;
   end Tail;

   function Tail (
      List : in Tash_List) return Tash.Strings.Tash_String is
   --
      Length : Natural := Tash.Lists.Length (List);
   begin -- Tail
      if Length = 0 then
         return Tash.Strings.Null_Tash_String;
      else
         return Element (List, Length);
      end if;
   end Tail;

   function Tail_Obj (
      List : in Tash_List) return Tash.Tash_Object'Class is
   --
      Length : Natural := Tash.Lists.Length (List);
   begin -- Tail_Obj
      if Length = 0 then
         return Tash.Strings.Null_Tash_String;
      else
         return Element_Obj (List, Length);
      end if;
   end Tail_Obj;

   procedure Append (
      List    : in out Tash_List;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False) is
   --
      Result : Interfaces.C.Int;
      Interp : Tcl.Tcl_Interp;
   begin -- Append

      if Is_Null (Element) then
         -- Element is null, so we do not append anything
         ------------------------------------------------
         return;
      end if;

      if Is_Null (List) then
         -- List is null, so create a new list
         -------------------------------------
         List := To_Tash_List (Element => Element,
                               Expand  => Expand);
         return;
      end if;

      Tash_Interp.Get (Interp);

      if Expand then

         -- append Element as a list
         ---------------------------
         Result := Tcl.Tcl_ListObjAppendList (
            Interp      => Interp,
            ListPtr     => List.Obj,
            ElemListPtr => Element.Obj);
         Tash_Interp.Assert (Interp, Result, List_Error'identity);

      else

         -- append Element as an element
         -------------------------------
         if Tcl.Tcl_IsShared (List.Obj) = 1 then
            Tash_Interp.Raise_Exception (
               Interp  => Interp,
               E       => List_Error'Identity,
               Message => "List is shared");
         else
            Result := Tcl.Tcl_ListObjAppendElement (
               Interp  => Interp,
               ListPtr => List.Obj,
               ObjPtr  => Element.Obj);
            Tash_Interp.Assert (Interp, Result, List_Error'identity);
         end if;
      end if;
      Tash_Interp.Release (Interp);

   end Append;

   procedure Replace_Element (
      List    : in out Tash_List;
      Index   : in     Positive;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False) is
   begin -- Replace_Element
      Replace_Slice (List    => List,
                     From    => Index,
                     To      => Index,
                     Element => Element,
                     Expand  => Expand);
   end Replace_Element;

   procedure Replace_Slice (
      List    : in out Tash_List;
      From    : in     Positive;
      To      : in     Natural;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False) is

      Result  : Interfaces.C.Int;
      Interp  : Tcl.Tcl_Interp;

   begin -- Replace_Slice

      if Is_Null (Element) then
         -- Element is null, so this is the same
         -- as deleting From..To elements
         ---------------------------------------
         Delete_Slice (List  => List,
                       From  => From,
                       To    => To);
         return;
      end if;

      if Is_Null (List) then
         -- List is null, so create a new list
         -------------------------------------
         List := To_Tash_List (Element => Element,
                               Expand  => Expand);
         return;
      end if;

      Tash_Interp.Get (Interp);

      if Expand then
         -- replace From..To elements with each
         -- member of the list Element
         --------------------------------------
         declare
            Objc    : Interfaces.C.Int := Length (Interp, Element.Obj);
            Objv    : Tcl.Tcl_Obj_Array (1..Objc);
            Obj_Ptr : Tcl.Tcl_Obj_Ptr := new Tcl.Tcl_Obj;
         begin

            -- Create an array of Tcl objects from list Element
            ---------------------------------------------------
            for I in Objv'Range loop
               Obj_Ptr := Tash.Lists.Element (Interp => Interp,
                                              List   => Element.Obj,
                                              Index  => Integer (I));
               Objv(I) := Obj_Ptr.all;
            end loop;

            -- Replace the indicated elements with the array of objects
            -----------------------------------------------------------
            Result := Tcl.Tcl_ListObjReplace (
               Interp  => Interp,
               ListPtr => List.Obj,
               First   => Interfaces.C.Int (From) - 1,
               Count   => Interfaces.C.Int (To - From) + 1,
               Objc    => Objc,
               Objv    => Objv);
            Tash_Interp.Assert (Interp, Result, List_Error'identity);
         end;
         Tash_Interp.Release (Interp);
         return;
      end if;

      -- replace From..To elements with the list
      -- Element as a single element
      ------------------------------------------
      declare
         Objv : Tcl.Tcl_Obj_Array (1..1);
      begin
         Objv(1) := Element.Obj;
         Result := Tcl.Tcl_ListObjReplace (
            Interp  => Interp,
            ListPtr => List.Obj,
            First   => Interfaces.C.Int (From) - 1,
            Count   => Interfaces.C.Int (To - From) + 1,
            Objc    => 1,
            Objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'identity);
      end;

      Tash_Interp.Release (Interp);

   end Replace_Slice;

   procedure Insert (
      List    : in out Tash_List;
      Index   : in     Positive;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False) is
   begin -- Insert
      Replace_Slice (List    => List,
                     From    => Index,
                     To      => Index - 1,
                     Element => Element,
                     Expand  => Expand);
   end Insert;

   procedure Delete_Element (
      List  : in out Tash_List;
      Index : in     Positive) is
   --
      Result : Interfaces.C.Int;
      Objv   : Tcl.Tcl_Obj_Array (1..0);
      Interp : Tcl.Tcl_Interp;
   begin -- Delete_Element
      if Is_Null (List) then
         return;
      else
         Tash_Interp.Get (Interp);
         Result := Tcl.Tcl_ListObjReplace (
            Interp  => Interp,
            ListPtr => List.Obj,
            First   => Interfaces.C.Int (Index) - 1,
            Count   => 1,
            Objc    => Interfaces.C.Int (0),
            Objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'identity);
         Tash_Interp.Release (Interp);
      end if;
   end Delete_Element;

   procedure Delete_Slice  (
      List : in out Tash_List;
      From : in     Positive;
      To   : in     Natural) is
   --
      Result : Interfaces.C.Int;
      Objv   : Tcl.Tcl_Obj_Array (1..0);
      Interp : Tcl.Tcl_Interp;
   begin -- Delete_Slice
      if Is_Null (List) then
         return;
      else
         Tash_Interp.Get (Interp);
         Result := Tcl.Tcl_ListObjReplace (
            Interp  => Interp,
            ListPtr => List.Obj,
            First   => Interfaces.C.Int (From) - 1,
            Count   => Interfaces.C.Int (To - From) + 1,
            Objc    => Interfaces.C.Int (0),
            Objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'identity);
         Tash_Interp.Release (Interp);
      end if;
   end Delete_Slice;

   function Tcl_LsortObjCmd (
      dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.Int;
      objv   : in Tcl.Tcl_Obj_Array
   ) return Interfaces.C.Int;
   pragma Import (C, Tcl_LsortObjCmd, "Tcl_LsortObjCmd");

   function Sort (
      List  : in Tash_List;
      Mode  : in Sort_Mode := SM_ASCII;
      Order : in Ordering  := Increasing) return Tash_List is
   --
      Objc     : Interfaces.C.Int := 4;
      Objv     : Tcl.Tcl_Obj_Array (1..Objc);
      Result   : Interfaces.C.Int;
      New_List : Tash_List;
      Interp   : Tcl.Tcl_Interp;
   begin -- Sort
      Objv(1) := Tash.To_Tcl_Obj ("lsort");
      case Mode is
         when SM_ASCII =>
            Objv(2) := Tash.To_Tcl_Obj ("-ascii");
         when SM_Dictionary =>
            Objv(2) := Tash.To_Tcl_Obj ("-dictionary");
         when SM_Integer =>
            Objv(2) := Tash.To_Tcl_Obj ("-integer");
         when SM_Real =>
            Objv(2) := Tash.To_Tcl_Obj ("-real");
      end case;
      case Order is
         when Increasing =>
            Objv(3) := Tash.To_Tcl_Obj ("-increasing");
         when Decreasing =>
            Objv(3) := Tash.To_Tcl_Obj ("-decreasing");
      end case;
      Objv(4) := List.Obj;
      Tash_Interp.Get (Interp);
      Result  := Tcl_LsortObjCmd (
         Dummy  => System.Null_Address,
         Interp => Interp,
         Objc   => Objc,
         Objv   => Objv);
--      for I in Objv(1..4)'Range loop
--         Tcl.Tcl_DecrRefCount (Objv(I));
--      end loop;
      Tash_Interp.Assert (Interp, Result, List_Error'identity);
      New_List.Obj := Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (New_List.Obj);
      return New_List;
   end Sort;

   procedure Push (
      List    : in out Tash_List;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False) is
   begin -- Push
      Replace_Slice (List => List,
                     From => 1,
                     To   => 0,
                     Element => Element,
                     Expand  => Expand);
   end Push;

   procedure Pop (
      List : in out Tash_List) is
   begin -- Pop
      if Is_Null (List) then
         return;
      end if;
      Delete_Element (List  => List,
                      Index => 1);
   end Pop;

   function "="  (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean is
   begin -- "="
      return To_String (Left) = To_String (Right);
   end "=";

   function "<"  (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean is
   begin -- "<"
      return To_String (Left) < To_String (Right);
   end "<";

   function "<=" (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean is
   begin -- "<="
      return To_String (Left) <= To_String (Right);
   end "<=";

   function ">"  (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean is
   begin -- ">"
      return To_String (Left) > To_String (Right);
   end ">";

   function ">=" (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean is
   begin -- ">="
      return To_String (Left) >= To_String (Right);
   end ">=";

   function Tcl_FormatObjCmd (
     Dummy  : in Tcl.ClientData;
     Interp : in Tcl.Tcl_Interp;
     objc   : in Interfaces.C.Int;
     objv   : in Tcl.Tcl_Obj_Array) return Interfaces.C.Int;
   pragma Import (C, Tcl_FormatObjCmd, "Tcl_FormatObjCmd");

   function Format (
      FormatString : in String;
      Values       : in Tash_List) return String is

      Result : Interfaces.C.Int;
      Interp : Tcl.Tcl_Interp;

   begin -- Format

      Tash_Interp.Get (Interp);

      declare
         Objc    : aliased Interfaces.C.Int := Length (Interp, Values.Obj)+2;
         Objv    : Tcl.Tcl_Obj_Array (1..Objc);
         Obj_Ptr : Tcl.Tcl_Obj_Ptr := new Tcl.Tcl_Obj;
      begin
         Objv(1) := Tash.To_Tcl_Obj ("format");
         Objv(2) := Tash.To_Tcl_Obj (FormatString);

         -- Create an array of Tcl objects from list Values
         --------------------------------------------------
         for I in 1..Length (Interp, Values.Obj) loop
            Obj_Ptr := Tash.Lists.Element (Interp => Interp,
                                           List   => Values.Obj,
                                           Index  => Integer (I));
            Objv(I+2) := Obj_Ptr.all;
         end loop;

         -- Call Tcl Format command
         --------------------------
         begin
            Tcl.Tcl_ResetResult (Interp);
            Result := Tcl_FormatObjCmd (
               Dummy  => System.Null_Address,
               Interp => Interp,
               objc   => Objc,
               objv   => Objv);
         exception
            when others =>
               for I in Objv(1..2)'Range loop
                  Tcl.Tcl_DecrRefCount (Objv(I));
               end loop;
               Tash_Interp.Assert (Interp, Tcl.TCL_ERROR,
                  Format_Error'identity);
         end;

         -- Decrement the reference counts of Objv(1..2) because this
         -- is how we tell Tcl we don't need this memory any more.
         ------------------------------------------------------------
         for I in Objv(1..2)'Range loop
            Tcl.Tcl_DecrRefCount (Objv(I));
         end loop;

         Tash_Interp.Assert (Interp, Result, Format_Error'identity);

         -- Get result from interpreter and return it
         --------------------------------------------
         declare
            StringResult : constant String :=
              CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            Tash_Interp.Release (Interp);
            return StringResult;
         end;

      end;

   end Format;

   function Internal_Rep (
      TList : in Tash_List) return String is

      Image  : Ada.Strings.Unbounded.Unbounded_String;
      Interp : Tcl.Tcl_Interp;

   begin -- Internal_Rep

      -- Get the internal representation of the list itself
      -----------------------------------------------------
      Image := Ada.Strings.Unbounded.To_Unbounded_String (
         Tash.Internal_Rep (Tash_Object (TList)));

      -- Append the internal representation of each of the
      -- elements of the list.  Note that we use the low-level
      -- internal rep of the Tcl object, not the Tash Object.
      -- This is because if we declare Elem as Tash_Object'Class,
      -- we'll increment the reference count and show a
      -- confusingly high reference count.
      -----------------------------------------------------------
      Tash_Interp.Get (Interp);
      for I in 1..Integer(Length (Interp, TList.Obj)) loop
         declare
            I_Image : String (1..8);
            Elem    : Tcl.Tcl_Obj_Ptr :=
               Tash.Lists.Element (Interp, TList.Obj, I);
         begin
            Ada.Integer_Text_IO.Put (I_Image, I);
            Ada.Strings.Unbounded.Append (Image,
               ASCII.LF & I_Image & ": " & Tash.Internal_Rep (Elem.all));
         end;
      end loop;
      Tash_Interp.Release (Interp);

      return Ada.Strings.Unbounded.To_String (Image);

   end Internal_Rep;

end Tash.Lists;

