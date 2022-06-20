--------------------------------------------------------------------
--
-- tcl.adb --
--
--  Copyright (c) 1995-2000 Terry J. Westley
--  Copyright (c) 2008 O Kellogg
--  Copyright (c) 2006, 2008, 2009, 2014, 2019, 2021, 2022
--     Simon Wright <simon@pushface.org>
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

with Ada.Text_IO;

package body Tcl is

   --  Some subprograms here correspond to C macros. C functions are
   --  provided to invoke the macros. However, only Ada units can form
   --  part of the public interface of the library, and what where
   --  global symbols in the other units (including C units) are
   --  converted to local symbols and hence not visible to callers.
   --
   --  So we have to call the C functions from Ada code, rather than
   --  having pragma Import in the package spec.

   procedure Tcl_IncrRefCount (objPtr : not null Tcl_Obj) is
      procedure Tcl_CallIncrRefCount (objPtr : not null Tcl_Obj);
      pragma Import (C, Tcl_CallIncrRefCount, "Tcl_CallIncrRefCount");
   begin
      Tcl_CallIncrRefCount (objPtr);
   end Tcl_IncrRefCount;

   procedure Tcl_DecrRefCount (objPtr : not null Tcl_Obj) is
      procedure Tcl_CallDecrRefCount (objPtr : not null Tcl_Obj);
      pragma Import (C, Tcl_CallDecrRefCount, "Tcl_CallDecrRefCount");
   begin
      Tcl_CallDecrRefCount (objPtr);
   end Tcl_DecrRefCount;

   function Tcl_IsShared (objPtr : not null Tcl_Obj) return C.int is
      function Tcl_CallIsShared (objPtr : not null Tcl_Obj) return C.int;
      pragma Import (C, Tcl_CallIsShared, "Tcl_CallIsShared");
   begin
      return Tcl_CallIsShared (objPtr);
   end Tcl_IsShared;

   function Tcl_GetHashValue
     (HashEntry : not null Tcl_HashEntry)
     return       ClientData is
      function Tcl_CallGetHashValue
        (HashEntry : not null Tcl_HashEntry)
        return      ClientData;
      pragma Import (C, Tcl_CallGetHashValue, "Tcl_CallGetHashValue");
   begin
      return Tcl_CallGetHashValue (HashEntry);
   end Tcl_GetHashValue;

   procedure Tcl_SetHashValue
     (HashEntry : not null Tcl_HashEntry;
      value     : ClientData) is
      procedure Tcl_CallSetHashValue
        (HashEntry : not null Tcl_HashEntry;
         value     : ClientData);
      pragma Import (C, Tcl_CallSetHashValue, "Tcl_CallSetHashValue");
   begin
      Tcl_CallSetHashValue (HashEntry, value);
   end Tcl_SetHashValue;

   function Tcl_GetHashKey
     (HashTable : not null Tcl_HashTable;
      HashEntry : not null Tcl_HashEntry)
     return      C.Strings.chars_ptr is
      function Tcl_CallGetHashKey
        (HashTable : not null Tcl_HashTable;
         HashEntry : not null Tcl_HashEntry)
        return      C.Strings.chars_ptr;
      pragma Import (C, Tcl_CallGetHashKey, "Tcl_CallGetHashKey");
   begin
      return Tcl_CallGetHashKey (HashTable, HashEntry);
   end Tcl_GetHashKey;

   function Tcl_FindHashEntry
     (HashTable : not null Tcl_HashTable;
      key       : C.Strings.chars_ptr)
     return      Tcl_HashEntry is
      function Tcl_CallFindHashEntry
        (HashTable : not null Tcl_HashTable;
         key       : C.Strings.chars_ptr)
        return      Tcl_HashEntry;
      pragma Import (C, Tcl_CallFindHashEntry, "Tcl_CallFindHashEntry");
   begin
      return Tcl_CallFindHashEntry (HashTable, key);
   end Tcl_FindHashEntry;

   function Tcl_CreateHashEntry
     (HashTable : not null Tcl_HashTable;
      key       : C.Strings.chars_ptr;
      newPtr    : not null access C.int)
     return      Tcl_HashEntry is
      function Tcl_CallCreateHashEntry
        (HashTable : not null Tcl_HashTable;
         key       : C.Strings.chars_ptr;
         newPtr    : not null access C.int)
        return      Tcl_HashEntry;
      pragma Import (C, Tcl_CallCreateHashEntry, "Tcl_CallCreateHashEntry");
   begin
      return Tcl_CallCreateHashEntry (HashTable, key, newPtr);
   end Tcl_CreateHashEntry;

   --  End of C macro interfaces.

   procedure AppendStringsToObj
     (objPtr    : Tcl_Obj;
      String1   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : C.Strings.chars_ptr := C.Strings.Null_Ptr)
   with
     Import,
     Convention => C_Variadic_1,
     External_Name => "Tcl_AppendStringsToObj";

   procedure Tcl_AppendStringsToObj
     (objPtr  : not null Tcl_Obj;
      String1 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr)
   is
   begin --  Tcl_AppendStringsToObj
      AppendStringsToObj
        (objPtr,
         String1,
         String2,
         String3,
         String4,
         String5,
         String6,
         String7,
         String8,
         String9);
   end Tcl_AppendStringsToObj;

   procedure AppendResult
     (interp    : Tcl_Interp;
      String1   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : C.Strings.chars_ptr := C.Strings.Null_Ptr)
   with
     Import,
     Convention => C_Variadic_1,
     External_Name => "Tcl_AppendResult";

   procedure Tcl_AppendResult
     (interp  : not null Tcl_Interp;
      String1 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr)
   is
   begin --  Tcl_AppendResult
      AppendResult
        (interp,
         String1,
         String2,
         String3,
         String4,
         String5,
         String6,
         String7,
         String8,
         String9);
   end Tcl_AppendResult;

   procedure SetErrorCode
     (interp    : Tcl_Interp;
      String1   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : C.Strings.chars_ptr := C.Strings.Null_Ptr)
   with
     Import,
     Convention => C_Variadic_1,
     External_Name => "Tcl_SetErrorCode";

   procedure Tcl_SetErrorCode
     (interp  : not null Tcl_Interp;
      String1 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr)
   is
   begin --  Tcl_SetErrorCode
      SetErrorCode
        (interp,
         String1,
         String2,
         String3,
         String4,
         String5,
         String6,
         String7,
         String8,
         String9);
   end Tcl_SetErrorCode;

   function VarEval
     (interp    : Tcl_Interp;
      String1   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : C.Strings.chars_ptr := C.Strings.Null_Ptr)
     return      C.int
   with
     Import,
     Convention => C_Variadic_1,
     External_Name => "Tcl_VarEval";

   function Tcl_VarEval
     (interp  : not null Tcl_Interp;
      String1 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr)
      return    C.int
   is
   begin --  Tcl_VarEval
      return VarEval
               (interp,
                String1,
                String2,
                String3,
                String4,
                String5,
                String6,
                String7,
                String8,
                String9);
   end Tcl_VarEval;

   function Tcl_GetObjTypeName
     (objPtr : Tcl_Obj) return C.Strings.chars_ptr is
   begin
      if objPtr = null or else objPtr.typePtr = null then
         return C.Strings.Null_Ptr;
      end if;
      return objPtr.typePtr.name;
   end Tcl_GetObjTypeName;

   function Tcl_GetRefCount (objPtr : Tcl_Obj) return C.int is
   begin
      if objPtr = null then
         return 0;
      end if;
      return objPtr.refCount;
   end Tcl_GetRefCount;

   procedure Tcl_PrintObj (Ptr : Tcl_Obj) is
   begin
      if Ptr = null then
         Ada.Text_IO.Put ("NULL");
      else
         declare
            Len : aliased C.int;
            StringFromValue : constant C.Strings.chars_ptr
              := Tcl_GetStringFromObj (Ptr, Len'Access);
            ObjTypeName : constant C.Strings.chars_ptr
              := Tcl_GetObjTypeName (Ptr);
            RefCount : constant C.int := Tcl_GetRefCount (Ptr);
         begin
            Ada.Text_IO.Put
              ("s=""" & C.Strings.Value (StringFromValue) & """ " &
                 "t=" & C.Strings.Value (ObjTypeName) & " " &
                 "c=" & C.int'Image (RefCount));
         end;
      end if;
   end Tcl_PrintObj;

end Tcl;
