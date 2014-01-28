-------------------------------------------------------------------
--
--  Unit Name:    Tash.Lists body
--
--  File Name:    tash-lists.adb
--
--  Purpose:      Defines the Tash list type and operations on it.
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

with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Tags;
with CArgv;
with CHelper;
with Interfaces.C.Strings;
with System;
with Tcl.Ada;

package body Tash.Lists is

   function Length
     (Interp : in Tcl.Tcl_Interp;
      Obj    : in Tcl.Tcl_Obj) return Interfaces.C.int;
   function Get_Element
     (Interp : in Tcl.Tcl_Interp;
      List   : in Tcl.Tcl_Obj;
      Index  : in Positive) return Tcl.Tcl_Obj;
   function Type_Of_List_Element
     (Obj : in Tcl.Tcl_Obj) return String;

   use type Interfaces.C.int;
   use type Interfaces.C.Strings.chars_ptr;
   use type Ada.Strings.Direction;
   use type Ada.Tags.Tag;

   --  Tcl_Free is define for use as described
   --  in the Tcl_SplitList man page.
   ------------------------------------------
   procedure Tcl_Free (Ptr : in CArgv.Chars_Ptr_Ptr);
   pragma Import (C, Tcl_Free, "Tcl_Free");

   function Is_Empty (List : in Tash_List) return Boolean is
   begin --  Is_Empty
      return Is_Null (List) or else Length (List) = 0;
   end Is_Empty;
   pragma Inline (Is_Empty);

   --  Get length of a Tcl list object; return 0 if null
   ----------------------------------------------------
   function Length
     (Interp : in Tcl.Tcl_Interp;
      Obj    : in Tcl.Tcl_Obj)
      return   Interfaces.C.int
   is
      --
      Result : Interfaces.C.int;
      IntPtr : aliased Interfaces.C.int;
      use type Tcl.Tcl_Obj;
   begin --  Length
      if Obj = null then
         return 0;
      else
         Result :=
            Tcl.Tcl_ListObjLength
              (interp  => Interp,
               listPtr => Obj,
               intPtr  => IntPtr'Access);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         return IntPtr;
      end if;
   end Length;

   function Strcmp
     (S1   : in Interfaces.C.Strings.chars_ptr;
      S2   : in Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;
   pragma Import (C, Strcmp, "strcmp");
   pragma Unreferenced (Strcmp);  -- XXX why is it here?

   --  Get an element of a Tcl list.
   -------------------------------
   function Get_Element
     (Interp : in Tcl.Tcl_Interp;
      List   : in Tcl.Tcl_Obj;
      Index  : in Positive)
      return   Tcl.Tcl_Obj
   is
      --
      Result     : Interfaces.C.int;
      Length     : constant Interfaces.C.int :=
        Tash.Lists.Length (Interp, List);
      Object : aliased Tcl.Tcl_Obj;
      use type Tcl.Tcl_Obj;
   begin --  Get_Element
      if List = null then
         raise List_Error;
      elsif Length < Interfaces.C.int (Index) then
         raise List_Error;
      else
         Result     :=
            Tcl.Tcl_ListObjIndex
              (interp    => Interp,
               listPtr   => List,
               index     => Interfaces.C.int (Index - 1),
               objPtrPtr => Object'Unchecked_Access);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         return Object;
      end if;
   end Get_Element;

   function To_Tash_List (Str : in String) return Tash_List is
      Argc      : aliased Interfaces.C.int;
      Argv      : aliased CArgv.Chars_Ptr_Ptr;
      Save_Argv : aliased CArgv.Chars_Ptr_Ptr;
      C_Str     : aliased Interfaces.C.char_array := Interfaces.C.To_C (Str);
      New_List  : Tash_List;
      New_Obj   : Tcl.Tcl_Obj;
      Objv      : Tcl.Tcl_Obj_Array (1 .. 1);
      Result    : Interfaces.C.int;
      Interp    : Tcl.Tcl_Interp;

   begin --  To_Tash_List

      if Str = "" then
         return New_List;
      end if;

      --  Tcl_SplitList creates a properly formed Tcl string
      --  including backslash substitution and braces.
      -----------------------------------------------------
      Tash_Interp.Get (Interp);
      Result :=
         Tcl.Tcl_SplitList
           (interp  => Interp,
            listStr =>
               Interfaces.C.Strings.To_Chars_Ptr (C_Str'Unchecked_Access),
            argcPtr => Argc'Unchecked_Access,
            argvPtr => Argv'Unchecked_Access);
      Tash_Interp.Assert (Interp, Result, List_Error'Identity);

      --  Initialize New_List as an empty list
      ---------------------------------------
      if Argc > 0 then
         New_List.Obj := Tcl.Tcl_NewListObj (objc => 0, objv => Objv);
         Tcl.Tcl_IncrRefCount (New_List.Obj);
      end if;

      --  Append each string element created by Tcl_SplitList to New_List
      ------------------------------------------------------------------
      Save_Argv := Argv;
      for I in  1 .. Argc loop
         New_Obj :=
            Tcl.Tcl_NewStringObj
              (bytes  => Argv.all,
               length => CHelper.Length (Argv.all));
         Result  :=
            Tcl.Tcl_ListObjAppendElement
              (interp  => Interp,
               listPtr => New_List.Obj,
               objPtr  => New_Obj);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         CArgv.Argv_Pointer.Increment (Argv);
      end loop;

      --  Free space allocated for Argv by Tcl_SplitList
      -------------------------------------------------
      Tcl_Free (Save_Argv);

      Tash_Interp.Release (Interp);

      return New_List;

   end To_Tash_List;

   function "+" (Str : in String) return Tash_List is
   begin --  "+"
      return To_Tash_List (Str);
   end "+";
   pragma Inline ("+");

   function "&"
     (Left  : in Tash_List;
      Right : in Tash_List)
      return  Tash_List
   is
      --
      New_List : Tash_List;
   begin --  "&"
      New_List := Copy (Left);
      Append_Elements (List => New_List, Elements => Right);
      return New_List;
   end "&";

   function "&" (Left : in String; Right : in Tash_List) return Tash_List is
      --
      New_List : Tash_List;
   begin --  "&"
      New_List := To_Tash_List (Left);
      Append_Elements (List => New_List, Elements => Right);
      return New_List;
   end "&";

   function "&" (Left : in Tash_List; Right : in String) return Tash_List is
      --
      New_List : Tash_List;
   begin --  "&"
      New_List := Copy (Left);
      Append (List => New_List, Element => Right);
      return New_List;
   end "&";

   function Copy (List : in Tash_List) return Tash_List is
      Len      : Natural;
      Objv     : Tcl.Tcl_Obj_Array (1 .. 1);
      Interp   : Tcl.Tcl_Interp;
      Obj      : Tcl.Tcl_Obj;
      New_List : Tash_List;
      Result   : Interfaces.C.int;

   begin --  Copy

      Len := Length (List);
      if Len = 0 then
         return New_List;
      end if;

      --  Initialize New_List as an empty list
      ---------------------------------------
      New_List.Obj := Tcl.Tcl_NewListObj (objc => 0, objv => Objv);
      Tcl.Tcl_IncrRefCount (New_List.Obj);

      --  Get and append each element
      ------------------------------
      Tash_Interp.Get (Interp);
      for I in  1 .. Len loop
         Obj :=
            Tash.Lists.Get_Element
              (Interp => Interp,
               List   => List.Obj,
               Index  => I);
         Result  :=
            Tcl.Tcl_ListObjAppendElement
              (interp  => Interp,
               listPtr => New_List.Obj,
               objPtr  => Obj);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      end loop;
      Tash_Interp.Release (Interp);

      return New_List;

   end Copy;

   function Duplicate (List : in Tash_List) return Tash_List is
      Len      : Natural;
      Objv     : Tcl.Tcl_Obj_Array (1 .. 1);
      Interp   : Tcl.Tcl_Interp;
      Obj      : Tcl.Tcl_Obj;
      New_List : Tash_List;
      Result   : Interfaces.C.int;

   begin --  Duplicate

      Len := Length (List);
      if Len = 0 then
         return New_List;
      end if;

      --  Initialize New_List as an empty list
      ---------------------------------------
      New_List.Obj := Tcl.Tcl_NewListObj (objc => 0, objv => Objv);
      Tcl.Tcl_IncrRefCount (New_List.Obj);

      --  Get and append duplicate of each element
      -------------------------------------------
      Tash_Interp.Get (Interp);
      for I in  1 .. Len loop
         Obj :=
            Tash.Lists.Get_Element
              (Interp => Interp,
               List   => List.Obj,
               Index  => I);
         Result  :=
            Tcl.Tcl_ListObjAppendElement
              (interp  => Interp,
               listPtr => New_List.Obj,
               objPtr  => Tcl.Tcl_DuplicateObj (Obj));
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      end loop;
      Tash_Interp.Release (Interp);

      return New_List;

   end Duplicate;

   function To_String (List : in Tash_List) return String is
      --
      Length : aliased Interfaces.C.int;
   begin --  To_String
      if Is_Null (List) then
         return "";
      else
         return CHelper.Value
                  (Tcl.Tcl_GetStringFromObj (List.Obj, Length'Access));
      end if;
   end To_String;

   function Tcl_SplitObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_SplitObjCmd, "Tcl_SplitObjCmd");

   function Split
     (Str      : in String;
      Split_At : in String := " " & ASCII.CR & ASCII.HT & ASCII.LF)
      return     Tash_List
   is
      --
      Objc     : constant Interfaces.C.int := 3;
      Objv     : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result   : Interfaces.C.int;
      New_List : Tash_List;
      Interp   : Tcl.Tcl_Interp;
      Interp_Result : Tcl.Tcl_Obj;
   begin --  Split
      Objv (1) := Tash.To_Tcl_Obj ("split");
      Objv (2) := Tash.To_Tcl_Obj (Str);
      Objv (3) := Tash.To_Tcl_Obj (Split_At);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_SplitObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv (1 .. 3)'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;
      Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      Interp_Result := Tcl.Tcl_GetObjResult (Interp);
      New_List.Obj := Tcl.Tcl_DuplicateObj (Interp_Result);
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (New_List.Obj);
      return New_List;
   end Split;

   function Tcl_JoinObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_JoinObjCmd, "Tcl_JoinObjCmd");

   function Join
     (List       : in Tash_List;
      JoinString : in String := " ")
      return       String
   is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Join

      --  Use Tcl_JoinObjCmd to join the list
      --------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("join");
      Objv (2) := List.Obj;
      Objv (3) := Tash.To_Tcl_Obj (JoinString);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_JoinObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv (1 .. 3)'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;
      Tash_Interp.Assert (Interp, Result, List_Error'Identity);

      --  Get result from interpreter and return it
      --------------------------------------------
      declare
         JoinedResult : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return JoinedResult;
      end;

   end Join;

   function Slice
     (List  : in Tash_List;
      First : in Positive;
      Last  : in Natural)
      return  Tash_List
   is
      --
      Len      : Natural;
      New_List : Tash_List;
   begin --  Slice
      Len := Length (List);
      for I in  First .. Last loop
         exit when I > Len;
         Append (List => New_List, Element => Get_Element (List, I));
      end loop;
      return New_List;
   end Slice;

   function Tcl_LsortObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_LsortObjCmd, "Tcl_LsortObjCmd");

   function Sort
     (List  : in Tash_List;
      Mode  : in Sort_Mode := SM_ASCII;
      Order : in Ordering  := Increasing)
      return  Tash_List
   is
      --
      Objc     : constant Interfaces.C.int := 4;
      Objv     : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result   : Interfaces.C.int;
      New_List : Tash_List;
      Interp   : Tcl.Tcl_Interp;
   begin --  Sort
      if Length (List) <= 1 then
         return List;
      end if;
      Objv (1) := Tash.To_Tcl_Obj ("lsort");
      case Mode is
         when SM_ASCII =>
            Objv (2) := Tash.To_Tcl_Obj ("-ascii");
         when SM_Dictionary =>
            Objv (2) := Tash.To_Tcl_Obj ("-dictionary");
         when SM_Integer =>
            Objv (2) := Tash.To_Tcl_Obj ("-integer");
         when SM_Real =>
            Objv (2) := Tash.To_Tcl_Obj ("-real");
      end case;
      case Order is
         when Increasing =>
            Objv (3) := Tash.To_Tcl_Obj ("-increasing");
         when Decreasing =>
            Objv (3) := Tash.To_Tcl_Obj ("-decreasing");
      end case;
      Objv (4) := List.Obj;
      Tash_Interp.Get (Interp);
      Result :=
         Tcl_LsortObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      New_List.Obj := Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (New_List.Obj);
      return New_List;
   end Sort;

   function Length (List : in Tash_List) return Natural is
      --
      Len    : Natural;
      Interp : Tcl.Tcl_Interp;
   begin --  Length
      if Is_Null (List) then
         return 0;
      else
         Tash_Interp.Get (Interp);
         Len := Natural (Length (Interp, List.Obj));
         Tash_Interp.Release (Interp);
         return Len;
      end if;
   end Length;

   function Get_Element
     (List  : in Tash_List;
      Index : in Positive)
      return  String
   is
      --
      Interp : Tcl.Tcl_Interp;
   begin --  Get_Element
      if Is_Null (List) or else Index > Length (List) then
         raise List_Error;
      else
         Tash_Interp.Get (Interp);
         declare
            Obj : constant Tcl.Tcl_Obj :=
              Get_Element (Interp, List.Obj, Index);
            Len : aliased Interfaces.C.int;
         begin
            Tash_Interp.Release (Interp);
            return Tcl.Ada.Tcl_GetStringFromObj (Obj, Len'Access);
         end;
      end if;
   end Get_Element;

   function Get_Element
     (List  : in Tash_List;
      Index : in Positive)
      return  Tash_List
   is
      --
      Interp : Tcl.Tcl_Interp;
   begin --  Get_Element
      if Is_Null (List) or else Index > Length (List) then
         raise List_Error;
      else
         Tash_Interp.Get (Interp);
         declare
            Obj : constant Tcl.Tcl_Obj :=
              Get_Element (Interp, List.Obj, Index);
         begin
            Tash_Interp.Release (Interp);
            Tcl.Tcl_IncrRefCount (Obj);
            return (Ada.Finalization.Controlled with Obj => Obj);
         end;
      end if;
   end Get_Element;

   --  Determine type of list element
   ---------------------------------
   function Type_Of_List_Element
     (Obj : in Tcl.Tcl_Obj)
      return   String
   is
      --
      use type Tcl.Tcl_Obj;
   begin --  Type_Of_List_Element (
      if Obj = null then
         return "null";
      end if;
      if Obj = null then
         return "null";
      end if;
      return CHelper.Value (Tcl.Tcl_GetObjTypeName (Obj));
   end Type_Of_List_Element;

   function Element_Is_String
     (List  : in Tash_List;
      Index : in Positive)
      return  Boolean
   is
      --
      Interp : Tcl.Tcl_Interp;
      Result : Boolean;
   begin --  Element_Is_String
      Tash_Interp.Get (Interp);
      declare
         Type_Of_Element : constant String :=
            Type_Of_List_Element (Get_Element (Interp, List.Obj, Index));
      begin
         Result := Type_Of_Element /= "list" and
                   Type_Of_Element /= "int" and
                   Type_Of_Element /= "double";
      end;
      Tash_Interp.Release (Interp);
      return Result;
   exception
      when others =>
         return False;
   end Element_Is_String;

   function Element_Is_List
     (List  : in Tash_List;
      Index : in Positive)
      return  Boolean
   is
      --
      Interp : Tcl.Tcl_Interp;
      Result : Boolean;
   begin --  Element_Is_List
      Tash_Interp.Get (Interp);
      Result := Type_Of_List_Element (Get_Element (Interp, List.Obj, Index)) =
                "list";
      Tash_Interp.Release (Interp);
      return Result;
   exception
      when others =>
         return False;
   end Element_Is_List;

   function Head (List : in Tash_List) return String is
   begin --  Head
      return Get_Element (List, 1);
   end Head;

   function Head (List : in Tash_List) return Tash_List is
   begin --  Head
      return Get_Element (List, 1);
   end Head;

   function Tail (List : in Tash_List) return String is
      --
      Length : constant Natural := Tash.Lists.Length (List);
   begin --  Tail
      if Length = 0 then
         return "";
      else
         return Get_Element (List, Length);
      end if;
   end Tail;

   function Tail (List : in Tash_List) return Tash_List is
      --
      Length : constant Natural := Tash.Lists.Length (List);
   begin --  Tail
      if Length = 0 then
         raise List_Error;
      else
         return Get_Element (List, Length);
      end if;
   end Tail;

   procedure Append (List : in out Tash_List; Element : in String) is
      --
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;
   begin --  Append

      if Is_Null (List) then
         --  List is null, so create a new list
         -------------------------------------
         List := To_Tash_List ("{" & Element & "}");
         return;
      end if;

      Tash_Interp.Get (Interp);

      --  append Element
      -----------------
      if Tcl.Tcl_IsShared (List.Obj) = 1 then
         Tash_Interp.Raise_Exception
           (Interp  => Interp,
            E       => List_Error'Identity,
            Message => "List is shared");
      else
         Result :=
            Tcl.Tcl_ListObjAppendElement
              (interp  => Interp,
               listPtr => List.Obj,
               objPtr  => Tash.To_Tcl_Obj (Element));
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      end if;

      Tash_Interp.Release (Interp);

   end Append;

   procedure Append_Elements
     (List     : in out Tash_List;
      Elements : in Tash_List)
   is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Append_Elements

      if Is_Null (Elements) then
         --  Element is null, so we do not append anything
         ------------------------------------------------
         return;
      end if;

      if Is_Null (List) then
         --  List is null, so create a new list
         -------------------------------------
         List := Copy (Elements);
         return;
      end if;

      Tash_Interp.Get (Interp);

      Result :=
         Tcl.Tcl_ListObjAppendList
           (interp      => Interp,
            listPtr     => List.Obj,
            elemListPtr => Elements.Obj);
      Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      Tash_Interp.Release (Interp);

   end Append_Elements;

   procedure Append_List (List : in out Tash_List; Element : in Tash_List) is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Append_List

      if Is_Null (Element) then
         --  Element is null, so we do not append anything
         ------------------------------------------------
         return;
      end if;

      if Is_Null (List) then
         --  List is null, so create a new list
         -------------------------------------
         Append_List (List, Element);
         return;
      end if;

      Tash_Interp.Get (Interp);

      if Tcl.Tcl_IsShared (List.Obj) = 1 then
         Tash_Interp.Raise_Exception
           (Interp  => Interp,
            E       => List_Error'Identity,
            Message => "List is shared");
      else
         Result :=
            Tcl.Tcl_ListObjAppendElement
              (interp  => Interp,
               listPtr => List.Obj,
               objPtr  => Element.Obj);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      end if;
      Tash_Interp.Release (Interp);

   end Append_List;

   procedure Replace_Element
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in String)
   is
   begin --  Replace_Element
      Replace_Slice
        (List    => List,
         From    => Index,
         To      => Index,
         Element => Element);
   end Replace_Element;

   procedure Replace_Element_With_List
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in Tash_List)
   is
   begin --  Replace_Element_With_List
      Replace_Slice_With_List
        (List    => List,
         From    => Index,
         To      => Index,
         Element => Element);
   end Replace_Element_With_List;

   procedure Replace_Element_With_Elements
     (List     : in out Tash_List;
      Index    : in Positive;
      Elements : in Tash_List)
   is
   begin --  Replace_Element_With_Elements
      Replace_Slice_With_Elements
        (List     => List,
         From     => Index,
         To       => Index,
         Elements => Elements);
   end Replace_Element_With_Elements;

   procedure Replace_Slice
     (List    : in out Tash_List;
      From    : in Positive;
      To      : in Natural;
      Element : in String)
   is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Replace_Slice

      if Is_Null (List) then
         --  List is null, so create a new list
         -------------------------------------
         List := To_Tash_List ("{" & Element & "}");
         return;
      end if;

      Tash_Interp.Get (Interp);

      --  replace From..To elements with the string element
      ----------------------------------------------------
      declare
         Objv : Tcl.Tcl_Obj_Array (1 .. 1);
      begin
         Objv (1) := Tash.To_Tcl_Obj (Element);
         Result   :=
            Tcl.Tcl_ListObjReplace
              (interp  => Interp,
               listPtr => List.Obj,
               first   => Interfaces.C.int (From) - 1,
               count   => Interfaces.C.int (To - From) + 1,
               objc    => 1,
               objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      end;

      Tash_Interp.Release (Interp);

   end Replace_Slice;

   procedure Replace_Slice_With_List
     (List    : in out Tash_List;
      From    : in Positive;
      To      : in Natural;
      Element : in Tash_List)
   is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Replace_Slice_With_List

      if Is_Null (Element) then
         --  Element is null, so this is the same
         --  as deleting From..To elements
         ---------------------------------------
         Delete_Slice (List => List, From => From, To => To);
         return;
      end if;

      if Is_Null (List) then
         --  Append the list Element as a single element
         ----------------------------------------------
         Append_List (List, Element);
         return;
      end if;

      Tash_Interp.Get (Interp);

      --  replace From..To elements with the list
      --  Element as a single element
      ------------------------------------------
      declare
         Objv : Tcl.Tcl_Obj_Array (1 .. 1);
      begin
         Objv (1) := Element.Obj;
         Result   :=
            Tcl.Tcl_ListObjReplace
              (interp  => Interp,
               listPtr => List.Obj,
               first   => Interfaces.C.int (From) - 1,
               count   => Interfaces.C.int (To - From) + 1,
               objc    => 1,
               objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      end;

      Tash_Interp.Release (Interp);

   end Replace_Slice_With_List;

   procedure Replace_Slice_With_Elements
     (List     : in out Tash_List;
      From     : in Positive;
      To       : in Natural;
      Elements : in Tash_List)
   is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Replace_Slice_With_Elements

      if Is_Null (Elements) then
         --  Element is null, so this is the same
         --  as deleting From..To elements
         ---------------------------------------
         Delete_Slice (List => List, From => From, To => To);
         return;
      end if;

      if Is_Null (List) then
         --  Append each element of the list Elements
         -------------------------------------------
         Append_Elements (List, Elements);
         return;
      end if;

      Tash_Interp.Get (Interp);

      --  replace From..To elements with each
      --  member of the list Elements
      --------------------------------------
      declare
         Objc    : constant Interfaces.C.int := Length (Interp, Elements.Obj);
         Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
         Obj     : Tcl.Tcl_Obj;
      begin

         --  Create an array of Tcl objects from list Elements
         ----------------------------------------------------
         for I in  Objv'Range loop
            Obj :=
               Tash.Lists.Get_Element
                 (Interp => Interp,
                  List   => Elements.Obj,
                  Index  => Integer (I));
            Objv (I) := Obj;
         end loop;

         --  Replace the indicated elements with the array of objects
         -----------------------------------------------------------
         Result :=
            Tcl.Tcl_ListObjReplace
              (interp  => Interp,
               listPtr => List.Obj,
               first   => Interfaces.C.int (From) - 1,
               count   => Interfaces.C.int (To - From) + 1,
               objc    => Objc,
               objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
      end;

      Tash_Interp.Release (Interp);

   end Replace_Slice_With_Elements;

   procedure Insert
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in String)
   is
   begin --  Insert
      Replace_Slice
        (List    => List,
         From    => Index,
         To      => Index - 1,
         Element => Element);
   end Insert;

   procedure Insert_List
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in Tash_List)
   is
   begin --  Insert_List
      Replace_Slice_With_List
        (List    => List,
         From    => Index,
         To      => Index - 1,
         Element => Element);
   end Insert_List;

   procedure Insert_Elements
     (List     : in out Tash_List;
      Index    : in Positive;
      Elements : in Tash_List)
   is
   begin --  Insert_Elements
      Replace_Slice_With_Elements
        (List     => List,
         From     => Index,
         To       => Index - 1,
         Elements => Elements);
   end Insert_Elements;

   procedure Delete_Element (List : in out Tash_List; Index : in Positive) is
      --
      Result : Interfaces.C.int;
      Objv   : Tcl.Tcl_Obj_Array (1 .. 0);
      Interp : Tcl.Tcl_Interp;
   begin --  Delete_Element
      if Is_Null (List) then
         return;
      elsif Index > Length (List) then
         return;
      else
         Tash_Interp.Get (Interp);
         Result :=
            Tcl.Tcl_ListObjReplace
              (interp  => Interp,
               listPtr => List.Obj,
               first   => Interfaces.C.int (Index) - 1,
               count   => 1,
               objc    => Interfaces.C.int (0),
               objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         Tash_Interp.Release (Interp);
      end if;
   end Delete_Element;

   procedure Delete_Slice
     (List : in out Tash_List;
      From : in Positive;
      To   : in Natural)
   is
      --
      Length : Natural;
      Last   : Natural := To;
      Result : Interfaces.C.int;
      Objv   : Tcl.Tcl_Obj_Array (1 .. 0);
      Interp : Tcl.Tcl_Interp;
   begin --  Delete_Slice
      if Is_Null (List) then
         return;
      end if;
      Length := Tash.Lists.Length (List);
      if From > Length then
         return;
      elsif From > Last then
         return;
      else
         if Last > Length then
            Last := Length;
         end if;
         Tash_Interp.Get (Interp);
         Result :=
            Tcl.Tcl_ListObjReplace
              (interp  => Interp,
               listPtr => List.Obj,
               first   => Interfaces.C.int (From) - 1,
               count   => Interfaces.C.int (Last - From) + 1,
               objc    => Interfaces.C.int (0),
               objv    => Objv);
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         Tash_Interp.Release (Interp);
      end if;
   end Delete_Slice;

   procedure Push (List : in out Tash_List; Element : in String) is
   begin --  Push
      Replace_Slice (List => List, From => 1, To => 0, Element => Element);
   end Push;

   procedure Push_List (List : in out Tash_List; Element : in Tash_List) is
   begin --  Push_List
      Replace_Slice_With_List
        (List    => List,
         From    => 1,
         To      => 0,
         Element => Element);
   end Push_List;

   procedure Push_Elements
     (List     : in out Tash_List;
      Elements : in Tash_List)
   is
   begin --  Push_Elements
      Replace_Slice_With_Elements
        (List     => List,
         From     => 1,
         To       => 0,
         Elements => Elements);
   end Push_Elements;

   procedure Pop (List : in out Tash_List) is
   begin --  Pop
      if Is_Null (List) then
         return;
      end if;
      Delete_Element (List => List, Index => 1);
   end Pop;

   package body Generic_Integer_Lists is

      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj;
      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj is
         New_Obj : Tcl.Tcl_Obj;
      begin --  To_Tcl_Obj
         New_Obj := Tcl.Tcl_NewIntObj (Interfaces.C.int (Num));
         Tcl.Tcl_IncrRefCount (New_Obj);
         return New_Obj;
      end To_Tcl_Obj;

      function To_Tash_List (Num : in Item) return Tash_List is
         New_List : Tash_List;
         Objv     : Tcl.Tcl_Obj_Array (1 .. 1);
         Result   : Interfaces.C.int;
         Interp   : Tcl.Tcl_Interp;

      begin --  To_Tash_List

         --  Initialize New_List as an empty list
         ---------------------------------------
         New_List.Obj := Tcl.Tcl_NewListObj (objc => 0, objv => Objv);
         Tcl.Tcl_IncrRefCount (New_List.Obj);

         --  Append the integer element to New_List
         -----------------------------------------
         Tash_Interp.Get (Interp);
         Result :=
            Tcl.Tcl_ListObjAppendElement
              (interp  => Interp,
               listPtr => New_List.Obj,
               objPtr  => To_Tcl_Obj (Num));
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         Tash_Interp.Release (Interp);

         return New_List;

      end To_Tash_List;

      function "+" (Num : in Item) return Tash_List is
      begin --  "+"
         return To_Tash_List (Num);
      end "+";
      pragma Inline ("+");

      function "-" (Num : in Item) return Tash_List is
      begin --  "-"
         return To_Tash_List (-Num);
      end "-";
      pragma Inline ("-");

      function "&" (Left : in Item; Right : in Tash_List) return Tash_List is
         --
         New_List : Tash_List;
      begin --  "&"
         New_List := To_Tash_List (Left);
         Append_Elements (List => New_List, Elements => Right);
         return New_List;
      end "&";

      function "&" (Left : in Tash_List; Right : in Item) return Tash_List is
         --
         New_List : Tash_List;
      begin --  "&"
         New_List := Copy (Left);
         Append (List => New_List, Element => Right);
         return New_List;
      end "&";

      function Get_Element
        (List  : in Tash_List;
         Index : in Positive)
         return  Item
      is
         --
         Interp : Tcl.Tcl_Interp;
      begin --  Get_Element
         if Is_Null (List) or else Index > Length (List) then
            raise List_Error;
         else
            Tash_Interp.Get (Interp);
            declare
               Obj    : constant Tcl.Tcl_Obj :=
                  Get_Element (Interp, List.Obj, Index);
               Value  : aliased Interfaces.C.int;
               Result : Interfaces.C.int;
               pragma Unreferenced (Result);  -- XXX why not?
            begin
               if Type_Of_List_Element (Obj) /= "int" then
                  Tash_Interp.Raise_Exception
                    (Interp  => Interp,
                     E       => Constraint_Error'Identity,
                     Message => "Element is not an integer");
               end if;
               Result :=
                  Tcl.Tcl_GetIntFromObj
                    (interp => Interp,
                     objPtr => Obj,
                     intPtr => Value'Access);
               Tash_Interp.Release (Interp);
               return Item (Value);
            end;
         end if;
      end Get_Element;

      function Element_Is_Integer
        (List  : in Tash_List;
         Index : in Positive)
         return  Boolean
      is
         --
         Interp : Tcl.Tcl_Interp;
         Result : Boolean;
      begin --  Element_Is_Integer
         Tash_Interp.Get (Interp);
         Result :=
           Type_Of_List_Element (Get_Element (Interp, List.Obj, Index)) =
           "int";
         Tash_Interp.Release (Interp);
         return Result;
      exception
         when others =>
            return False;
      end Element_Is_Integer;

      function Head (List : in Tash_List) return Item is
      begin --  Head
         return Get_Element (List, 1);
      end Head;

      function Tail (List : in Tash_List) return Item is
         --
         Length : constant Natural := Tash.Lists.Length (List);
      begin --  Tail
         if Length = 0 then
            return 0;
         else
            return Get_Element (List, Length);
         end if;
      end Tail;

      procedure Append (List : in out Tash_List; Element : in Item) is
         --
         Result : Interfaces.C.int;
         Interp : Tcl.Tcl_Interp;
      begin --  Append

         if Is_Null (List) then
            --  List is null, so create a new list
            -------------------------------------
            List := To_Tash_List (Element);
            return;
         end if;

         Tash_Interp.Get (Interp);

         --  append Element
         -----------------
         if Tcl.Tcl_IsShared (List.Obj) = 1 then
            Tash_Interp.Raise_Exception
              (Interp  => Interp,
               E       => List_Error'Identity,
               Message => "List is shared");
         else
            Result :=
               Tcl.Tcl_ListObjAppendElement
                 (interp  => Interp,
                  listPtr => List.Obj,
                  objPtr  => To_Tcl_Obj (Element));
            Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         end if;

         Tash_Interp.Release (Interp);

      end Append;

      procedure Replace_Element
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item)
      is
      begin --  Replace_Element
         Replace_Slice
           (List    => List,
            From    => Index,
            To      => Index,
            Element => Element);
      end Replace_Element;

      procedure Replace_Slice
        (List    : in out Tash_List;
         From    : in Positive;
         To      : in Natural;
         Element : in Item)
      is

         Result : Interfaces.C.int;
         Interp : Tcl.Tcl_Interp;

      begin --  Replace_Slice

         if Is_Null (List) then
            --  List is null, so create a new list
            -------------------------------------
            List := To_Tash_List (Element);
            return;
         end if;

         Tash_Interp.Get (Interp);

         --  replace From..To elements with the integer element
         -----------------------------------------------------
         declare
            Objv : Tcl.Tcl_Obj_Array (1 .. 1);
         begin
            Objv (1) := To_Tcl_Obj (Element);
            Result   :=
               Tcl.Tcl_ListObjReplace
                 (interp  => Interp,
                  listPtr => List.Obj,
                  first   => Interfaces.C.int (From) - 1,
                  count   => Interfaces.C.int (To - From) + 1,
                  objc    => 1,
                  objv    => Objv);
            Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         end;

         Tash_Interp.Release (Interp);

      end Replace_Slice;

      procedure Insert
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item)
      is
      begin --  Insert
         Replace_Slice
           (List    => List,
            From    => Index,
            To      => Index - 1,
            Element => Element);
      end Insert;

      procedure Push (List : in out Tash_List; Element : in Item) is
      begin --  Push
         Replace_Slice (List => List, From => 1, To => 0, Element => Element);
      end Push;

   end Generic_Integer_Lists;

   package body Generic_Float_Lists is

      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj;
      function To_Tcl_Obj (Num : in Item) return Tcl.Tcl_Obj is
         New_Obj : Tcl.Tcl_Obj;
      begin --  To_Tcl_Obj
         New_Obj := Tcl.Tcl_NewDoubleObj (Interfaces.C.double (Num));
         Tcl.Tcl_IncrRefCount (New_Obj);
         return New_Obj;
      end To_Tcl_Obj;

      function To_Tash_List (Num : in Item) return Tash_List is
         New_List : Tash_List;
         Objv     : Tcl.Tcl_Obj_Array (1 .. 1);
         Result   : Interfaces.C.int;
         Interp   : Tcl.Tcl_Interp;

      begin --  To_Tash_List

         --  Initialize New_List as an empty list
         ---------------------------------------
         New_List.Obj := Tcl.Tcl_NewListObj (objc => 0, objv => Objv);
         Tcl.Tcl_IncrRefCount (New_List.Obj);

         --  Append the float element to New_List
         -----------------------------------------
         Tash_Interp.Get (Interp);
         Result :=
            Tcl.Tcl_ListObjAppendElement
              (interp  => Interp,
               listPtr => New_List.Obj,
               objPtr  => To_Tcl_Obj (Num));
         Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         Tash_Interp.Release (Interp);

         return New_List;

      end To_Tash_List;

      function "+" (Num : in Item) return Tash_List is
      begin --  "+"
         return To_Tash_List (Num);
      end "+";
      pragma Inline ("+");

      function "-" (Num : in Item) return Tash_List is
      begin --  "-"
         return To_Tash_List (-Num);
      end "-";
      pragma Inline ("-");

      function "&" (Left : in Item; Right : in Tash_List) return Tash_List is
         --
         New_List : Tash_List;
      begin --  "&"
         New_List := To_Tash_List (Left);
         Append_Elements (List => New_List, Elements => Right);
         return New_List;
      end "&";

      function "&" (Left : in Tash_List; Right : in Item) return Tash_List is
         --
         New_List : Tash_List;
      begin --  "&"
         New_List := Copy (Left);
         Append (List => New_List, Element => Right);
         return New_List;
      end "&";

      function Get_Element
        (List  : in Tash_List;
         Index : in Positive)
         return  Item
      is
         --
         Interp : Tcl.Tcl_Interp;
      begin --  Get_Element
         if Is_Null (List) or else Index > Length (List) then
            raise List_Error;
         else
            Tash_Interp.Get (Interp);
            declare
               Obj    : constant Tcl.Tcl_Obj :=
                  Get_Element (Interp, List.Obj, Index);
               Value  : aliased Interfaces.C.double;
               Result : Interfaces.C.int;
               pragma Unreferenced (Result);  --  XXX why not?
            begin
               if Type_Of_List_Element (Obj) /= "double" then
                  Tash_Interp.Raise_Exception
                    (Interp  => Interp,
                     E       => Constraint_Error'Identity,
                     Message => "Element is not a float");
               end if;
               Result :=
                  Tcl.Tcl_GetDoubleFromObj
                    (interp    => Interp,
                     objPtr    => Obj,
                     doublePtr => Value'Access);
               Tash_Interp.Release (Interp);
               return Item (Value);
            end;
         end if;
      end Get_Element;

      function Element_Is_Float
        (List  : in Tash_List;
         Index : in Positive)
         return  Boolean
      is
         --
         Interp : Tcl.Tcl_Interp;
         Result : Boolean;
      begin --  Element_Is_Float
         Tash_Interp.Get (Interp);
         Result :=
           Type_Of_List_Element (Get_Element (Interp, List.Obj, Index)) =
           "double";
         Tash_Interp.Release (Interp);
         return Result;
      exception
         when others =>
            return False;
      end Element_Is_Float;

      function Head (List : in Tash_List) return Item is
      begin --  Head
         return Get_Element (List, 1);
      end Head;

      function Tail (List : in Tash_List) return Item is
         --
         Length : constant Natural := Tash.Lists.Length (List);
      begin --  Tail
         if Length = 0 then
            return 0.0;
         else
            return Get_Element (List, Length);
         end if;
      end Tail;

      procedure Append (List : in out Tash_List; Element : in Item) is
         --
         Result : Interfaces.C.int;
         Interp : Tcl.Tcl_Interp;
      begin --  Append

         if Is_Null (List) then
            --  List is null, so create a new list
            -------------------------------------
            List := To_Tash_List (Element);
            return;
         end if;

         Tash_Interp.Get (Interp);

         --  append Element
         -----------------
         if Tcl.Tcl_IsShared (List.Obj) = 1 then
            Tash_Interp.Raise_Exception
              (Interp  => Interp,
               E       => List_Error'Identity,
               Message => "List is shared");
         else
            Result :=
               Tcl.Tcl_ListObjAppendElement
                 (interp  => Interp,
                  listPtr => List.Obj,
                  objPtr  => To_Tcl_Obj (Element));
            Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         end if;

         Tash_Interp.Release (Interp);

      end Append;

      procedure Replace_Element
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item)
      is
      begin --  Replace_Element
         Replace_Slice
           (List    => List,
            From    => Index,
            To      => Index,
            Element => Element);
      end Replace_Element;

      procedure Replace_Slice
        (List    : in out Tash_List;
         From    : in Positive;
         To      : in Natural;
         Element : in Item)
      is

         Result : Interfaces.C.int;
         Interp : Tcl.Tcl_Interp;

      begin --  Replace_Slice

         if Is_Null (List) then
            --  List is null, so create a new list
            -------------------------------------
            List := To_Tash_List (Element);
            return;
         end if;

         Tash_Interp.Get (Interp);

         --  replace From..To elements with the float element
         -----------------------------------------------------
         declare
            Objv : Tcl.Tcl_Obj_Array (1 .. 1);
         begin
            Objv (1) := To_Tcl_Obj (Element);
            Result   :=
               Tcl.Tcl_ListObjReplace
                 (interp  => Interp,
                  listPtr => List.Obj,
                  first   => Interfaces.C.int (From) - 1,
                  count   => Interfaces.C.int (To - From) + 1,
                  objc    => 1,
                  objv    => Objv);
            Tash_Interp.Assert (Interp, Result, List_Error'Identity);
         end;

         Tash_Interp.Release (Interp);

      end Replace_Slice;

      procedure Insert
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item)
      is
      begin --  Insert
         Replace_Slice
           (List    => List,
            From    => Index,
            To      => Index - 1,
            Element => Element);
      end Insert;

      procedure Push (List : in out Tash_List; Element : in Item) is
      begin --  Push
         Replace_Slice (List => List, From => 1, To => 0, Element => Element);
      end Push;

   end Generic_Float_Lists;

   function "=" (Left : in Tash_List; Right : in Tash_List) return Boolean is
   begin --  "="
      return To_String (Left) = To_String (Right);
   end "=";

   function "<" (Left : in Tash_List; Right : in Tash_List) return Boolean is
   begin --  "<"
      return To_String (Left) < To_String (Right);
   end "<";

   function "<=" (Left : in Tash_List; Right : in Tash_List) return Boolean is
   begin --  "<="
      return To_String (Left) <= To_String (Right);
   end "<=";

   function ">" (Left : in Tash_List; Right : in Tash_List) return Boolean is
   begin --  ">"
      return To_String (Left) > To_String (Right);
   end ">";

   function ">=" (Left : in Tash_List; Right : in Tash_List) return Boolean is
   begin --  ">="
      return To_String (Left) >= To_String (Right);
   end ">=";

   function Tcl_FormatObjCmd
     (Dummy  : in Tcl.ClientData;
      Interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_FormatObjCmd, "Tcl_FormatObjCmd");

   function Format
     (FormatString : in String;
      Values       : in Tash_List)
      return         String
   is
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Format

      Tash_Interp.Get (Interp);

      declare
         Objc    : aliased constant Interfaces.C.int :=
            Length (Interp, Values.Obj) + 2;
         Objv    : Tcl.Tcl_Obj_Array (1 .. Objc);
         Obj     : Tcl.Tcl_Obj;
      begin
         Objv (1) := Tash.To_Tcl_Obj ("format");
         Objv (2) := Tash.To_Tcl_Obj (FormatString);

         --  Create an array of Tcl objects from list Values
         --------------------------------------------------
         for I in  1 .. Length (Interp, Values.Obj) loop
            Obj :=
               Tash.Lists.Get_Element
                 (Interp => Interp,
                  List   => Values.Obj,
                  Index  => Integer (I));
            Objv (I + 2) := Obj;
         end loop;

      --  Call Tcl Format command
      --------------------------
         begin
            Tcl.Tcl_ResetResult (Interp);
            Result :=
               Tcl_FormatObjCmd
                 (Dummy  => System.Null_Address,
                  Interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
         exception
            when others =>
               for I in  Objv (1 .. 2)'Range loop
                  Tcl.Tcl_DecrRefCount (Objv (I));
               end loop;
               Tash_Interp.Assert
                 (Interp,
                  Tcl.TCL_ERROR,
                  Format_Error'Identity);
         end;

         --  Decrement the reference counts of Objv(1..2) because this
         --  is how we tell Tcl we don't need this memory any more.
         ------------------------------------------------------------
         for I in  Objv (1 .. 2)'Range loop
            Tcl.Tcl_DecrRefCount (Objv (I));
         end loop;

         Tash_Interp.Assert (Interp, Result, Format_Error'Identity);

         --  Get result from interpreter and return it
         --------------------------------------------
         declare
            StringResult : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            Tcl.Tcl_ResetResult (Interp);
            Tash_Interp.Release (Interp);
            return StringResult;
         end;

      end;

   end Format;

   function Internal_Rep (List : in Tash_List) return String is
      Image  : Ada.Strings.Unbounded.Unbounded_String;
      Interp : Tcl.Tcl_Interp;

   begin --  Internal_Rep

      --  Get the internal representation of the list itself
      -----------------------------------------------------
      Image :=
         Ada.Strings.Unbounded.To_Unbounded_String
           (Tash.Internal_Rep (Tash_Object (List)));

      --  Append the internal representation of each of the
      --  elements of the list.  Note that we use the low-level
      --  internal rep of the Tcl object, not the Tash Object.
      --  This is because if we declare Elem as Tash_Object'Class,
      --  we'll increment the reference count and show a
      --  confusingly high reference count.
      -----------------------------------------------------------
      Tash_Interp.Get (Interp);
      for I in  1 .. Integer (Length (Interp, List.Obj)) loop
         declare
            I_Image : String (1 .. 8);
            Elem    : constant Tcl.Tcl_Obj :=
               Tash.Lists.Get_Element (Interp, List.Obj, I);
         begin
            Ada.Integer_Text_IO.Put (I_Image, I);
            Ada.Strings.Unbounded.Append
              (Image,
               ASCII.LF & I_Image & ": " & Tash.Internal_Rep (Elem));
         end;
      end loop;
      Tash_Interp.Release (Interp);

      return Ada.Strings.Unbounded.To_String (Image);

   end Internal_Rep;

end Tash.Lists;
