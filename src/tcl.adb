--------------------------------------------------------------------
--
-- tcl.adb --
--
--  Copyright (c) 1995-2000 Terry J. Westley
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

with Unchecked_Conversion;
with Ada.Text_IO;

package body Tcl is

   function Is_Null (Ptr : in Tcl_Interp) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_AsyncHandler) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Channel) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Command) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Condition) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_EncodingState) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Encoding) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Event) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Mutex) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Pid) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_RegExp) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_ThreadDataKey) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_ThreadId) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_TimerToken) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Trace) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Var) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_RegExpInfo) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in stat) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Value) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Obj) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_ObjType) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_SavedResult) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Namespace) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_CallFrame) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_CmdInfo) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_DString) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_HashEntry) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_HashTable) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_HashSearch) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Time) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_ChannelType) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_ChannelTypeVersion) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_LoadHandle) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_NotifierProcs) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_EncodingType) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Token) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_Parse) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tcl_StatBuf) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   procedure AppendStringsToObj
     (objPtr    : in Tcl_Obj;
      String1   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      ForceNull : in C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Import (C, AppendStringsToObj, "Tcl_AppendStringsToObj");

   procedure Tcl_AppendStringsToObj
     (objPtr  : in Tcl_Obj;
      String1 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr)
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
     (interp    : in Tcl_Interp;
      String1   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      ForceNull : in C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Import (C, AppendResult, "Tcl_AppendResult");

   procedure Tcl_AppendResult
     (interp  : in Tcl_Interp;
      String1 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr)
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
     (interp    : in Tcl_Interp;
      String1   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      ForceNull : in C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Import (C, SetErrorCode, "Tcl_SetErrorCode");

   procedure Tcl_SetErrorCode
     (interp  : in Tcl_Interp;
      String1 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr)
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
     (interp    : in Tcl_Interp;
      String1   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9   : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      ForceNull : in C.Strings.chars_ptr := C.Strings.Null_Ptr)
      return      C.int;
   pragma Import (C, VarEval, "Tcl_VarEval");

   function Tcl_VarEval
     (interp  : in Tcl_Interp;
      String1 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr)
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
     (objPtr : in Tcl_Obj) return C.Strings.chars_ptr is
   begin
      if objPtr = null or else objPtr.typePtr = null then
         return C.Strings.Null_Ptr;
      end if;
      return objPtr.typePtr.name;
   end Tcl_GetObjTypeName;

   function Tcl_GetRefCount (objPtr : in Tcl_Obj) return C.int is
   begin
      if objPtr = null then
         return 0;
      end if;
      return objPtr.refCount;
   end Tcl_GetRefCount;

   procedure Tcl_PrintObj (Ptr : in Tcl_Obj) is
      Len : aliased C.int;
      StringFromValue : constant C.Strings.chars_ptr
         := Tcl_GetStringFromObj (Ptr, Len'Access);
      ObjTypeName : constant C.Strings.chars_ptr
         := Tcl_GetObjTypeName (Ptr);
      RefCount : constant C.int := Tcl_GetRefCount (Ptr);
   begin
      if Ptr = null then
         Ada.Text_IO.Put ("NULL");
      else
         Ada.Text_IO.Put
           ("s=""" & C.Strings.Value (StringFromValue) & """ " &
            "t=" & C.Strings.Value (ObjTypeName) & " " &
            "c=" & C.int'Image (RefCount));
      end if;
   end Tcl_PrintObj;

end Tcl;
