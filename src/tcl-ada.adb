--------------------------------------------------------------------
--
-- tcl-ada.adb --
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

with Ada.Exceptions;
with CHelper;

package body Tcl.Ada is

--     use type C.int;

   procedure Assert (Interp : in Tcl_Interp; Return_Code : in C.int) is
   --  Raises Tcl_Error_Exception if Return_Code = TCL_ERROR
   begin --  Assert
      if Return_Code = TCL_ERROR then
         Standard.Ada.Exceptions.Raise_Exception
           (Tcl_Error_Exception'Identity,
            Tcl_GetResult (Interp));
      end if;
   end Assert;

   package body Generic_AssocData is

      function Tcl_GetAssocData
        (interp  : in Tcl_Interp;
         name    : in String;
         procPtr : in Tcl_InterpDeleteProc)
         return    ClientData
      is
         C_name : aliased C.char_array := C.To_C (name);
      begin --  Tcl_GetAssocData
         return Tcl_GetAssocData
                  (interp,
                   C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                   procPtr);
      end Tcl_GetAssocData;

      procedure Tcl_SetAssocData
        (interp : in Tcl_Interp;
         name   : in String;
         proc   : in Tcl_InterpDeleteProc;
         data   : in ClientData)
      is
         C_name : aliased C.char_array := C.To_C (name);
      begin --  Tcl_SetAssocData
         Tcl_SetAssocData
           (interp,
            C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
            proc,
            data);
      end Tcl_SetAssocData;

   end Generic_AssocData;

   package body Generic_AsyncEvents is

   end Generic_AsyncEvents;

   package body Generic_CallWhenDeleted is

   end Generic_CallWhenDeleted;

   package body Generic_Channel is

   end Generic_Channel;

   package body Generic_ChannelDriver is

      function Tcl_CreateChannel
        (typePtr      : in Tcl_ChannelType;
         chanName     : in String;
         instancedata : in ClientData;
         mask         : in C.int)
         return         Tcl_Channel
      is
         C_chanName : aliased C.char_array := C.To_C (chanName);
      begin --  Tcl_CreateChannel
         return Tcl_CreateChannel
                  (typePtr,
                   C.Strings.To_Chars_Ptr (C_chanName'Unchecked_Access),
                   instancedata,
                   mask);
      end Tcl_CreateChannel;

   end Generic_ChannelDriver;

   package body Generic_ChannelHandler is

   end Generic_ChannelHandler;

   package body Generic_CloseHandler is

   end Generic_CloseHandler;

   package body Generic_Command is

      function Tcl_CreateCommand
        (interp     : in Tcl_Interp;
         cmdName    : in String;
         proc       : in Tcl_CmdProc;
         data       : in ClientData;
         deleteProc : in Tcl_CmdDeleteProc)
         return       Tcl_Command
      is
         C_cmdName : aliased C.char_array := C.To_C (cmdName);
      begin --  Tcl_CreateCommand
         return Tcl_CreateCommand
                  (interp,
                   C.Strings.To_Chars_Ptr (C_cmdName'Unchecked_Access),
                   proc,
                   data,
                   deleteProc);
      end Tcl_CreateCommand;

   end Generic_Command;

   package body Generic_EventQueueAndNotifier is

   end Generic_EventQueueAndNotifier;

   package body Generic_ExitHandler is

   end Generic_ExitHandler;

   package body Generic_FileHandler is

   end Generic_FileHandler;

   package body Generic_GetOpenFile is

      function Tcl_GetOpenFile
        (interp     : in Tcl_Interp;
         str        : in String;
         forWriting : in C.int;
         checkUsage : in C.int;
         fileptr    : in ClientData)
         return       C.int
      is
         C_str : aliased C.char_array := C.To_C (str);
      begin --  Tcl_GetOpenFile
         return Tcl_GetOpenFile
                  (interp,
                   C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                   forWriting,
                   checkUsage,
                   fileptr);
      end Tcl_GetOpenFile;

      procedure Tcl_GetOpenFile
        (interp     : in Tcl_Interp;
         str        : in String;
         forWriting : in C.int;
         checkUsage : in C.int;
         fileptr    : in ClientData)
      is
         C_str : aliased C.char_array := C.To_C (str);
      begin --  Tcl_GetOpenFile
         Assert
           (interp,
            Tcl_GetOpenFile
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                forWriting,
                checkUsage,
                fileptr));
      end Tcl_GetOpenFile;

   end Generic_GetOpenFile;

   package body Generic_Hash is

   end Generic_Hash;

   package body Generic_Idle is

   end Generic_Idle;

   package body Generic_ManageStorage is

   end Generic_ManageStorage;

   package body Generic_MathFunc is

      procedure Tcl_CreateMathFunc
        (interp   : in Tcl_Interp;
         name     : in String;
         numArgs  : in C.int;
         argTypes : in Tcl_ValueType;
         proc     : in Tcl_MathProc;
         data     : in ClientData)
      is
         C_name : aliased C.char_array := C.To_C (name);
      begin --  Tcl_CreateMathFunc
         Tcl_CreateMathFunc
           (interp,
            C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
            numArgs,
            argTypes,
            proc,
            data);
      end Tcl_CreateMathFunc;

   end Generic_MathFunc;

   package body Generic_ObjCommand is

      function Tcl_CreateObjCommand
        (interp     : in Tcl_Interp;
         cmdName    : in String;
         proc       : in Tcl_ObjCmdProc;
         data       : in ClientData;
         deleteProc : in Tcl_CmdDeleteProc)
         return       Tcl_Command
      is
         C_cmdName : aliased C.char_array := C.To_C (cmdName);
      begin --  Tcl_CreateObjCommand
         return Tcl_CreateObjCommand
                  (interp,
                   C.Strings.To_Chars_Ptr (C_cmdName'Unchecked_Access),
                   proc,
                   data,
                   deleteProc);
      end Tcl_CreateObjCommand;

   end Generic_ObjCommand;

   package body Generic_PkgRequire is

      function Tcl_PkgPresentEx
        (interp        : in Tcl_Interp;
         name          : in String;
         version       : in String;
         exact         : in C.int;
         clientdataptr : in ClientData)
         return          String
      is
         C_name    : aliased C.char_array := C.To_C (name);
         C_version : aliased C.char_array := C.To_C (version);
      begin --  Tcl_PkgPresentEx
         return CHelper.Value
                  (Tcl_PkgPresentEx
                      (interp,
                       C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                       C.Strings.To_Chars_Ptr (C_version'Unchecked_Access),
                       exact,
                       clientdataptr));
      end Tcl_PkgPresentEx;

      function Tcl_PkgProvideEx
        (interp  : in Tcl_Interp;
         name    : in String;
         version : in String;
         data    : in ClientData)
         return    C.int
      is
         C_name    : aliased C.char_array := C.To_C (name);
         C_version : aliased C.char_array := C.To_C (version);
      begin --  Tcl_PkgProvideEx
         return Tcl_PkgProvideEx
                  (interp,
                   C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                   C.Strings.To_Chars_Ptr (C_version'Unchecked_Access),
                   data);
      end Tcl_PkgProvideEx;

      procedure Tcl_PkgProvideEx
        (interp  : in Tcl_Interp;
         name    : in String;
         version : in String;
         data    : in ClientData)
      is
         C_name    : aliased C.char_array := C.To_C (name);
         C_version : aliased C.char_array := C.To_C (version);
      begin --  Tcl_PkgProvideEx
         Assert
           (interp,
            Tcl_PkgProvideEx
               (interp,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_version'Unchecked_Access),
                data));
      end Tcl_PkgProvideEx;

      function Tcl_PkgRequireEx
        (interp        : in Tcl_Interp;
         name          : in String;
         version       : in String;
         exact         : in C.int;
         clientdataptr : in ClientData)
         return          String
      is
         C_name    : aliased C.char_array := C.To_C (name);
         C_version : aliased C.char_array := C.To_C (version);
      begin --  Tcl_PkgRequireEx
         return CHelper.Value
                  (Tcl_PkgRequireEx
                      (interp,
                       C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                       C.Strings.To_Chars_Ptr (C_version'Unchecked_Access),
                       exact,
                       clientdataptr));
      end Tcl_PkgRequireEx;

   end Generic_PkgRequire;

   package body Generic_StackChannel is

   end Generic_StackChannel;

   package body Generic_TcpChannel is

      function Tcl_OpenTcpServer
        (interp       : in Tcl_Interp;
         port         : in C.int;
         host         : in String;
         acceptProc   : in Tcl_TcpAcceptProc;
         callbackdata : in ClientData)
         return         Tcl_Channel
      is
         C_host : aliased C.char_array := C.To_C (host);
      begin --  Tcl_OpenTcpServer
         return Tcl_OpenTcpServer
                  (interp,
                   port,
                   C.Strings.To_Chars_Ptr (C_host'Unchecked_Access),
                   acceptProc,
                   callbackdata);
      end Tcl_OpenTcpServer;

   end Generic_TcpChannel;

   package body Generic_TimerHandler is

   end Generic_TimerHandler;

   package body Generic_Trace is

   end Generic_Trace;

   package body Generic_TraceVar is

      function Tcl_TraceVar
        (interp  : in Tcl_Interp;
         varName : in String;
         flags   : in C.int;
         proc    : in Tcl_VarTraceProc;
         data    : in ClientData)
         return    C.int
      is
         C_varName : aliased C.char_array := C.To_C (varName);
      begin --  Tcl_TraceVar
         return Tcl_TraceVar
                  (interp,
                   C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                   flags,
                   proc,
                   data);
      end Tcl_TraceVar;

      procedure Tcl_TraceVar
        (interp  : in Tcl_Interp;
         varName : in String;
         flags   : in C.int;
         proc    : in Tcl_VarTraceProc;
         data    : in ClientData)
      is
         C_varName : aliased C.char_array := C.To_C (varName);
      begin --  Tcl_TraceVar
         Assert
           (interp,
            Tcl_TraceVar
               (interp,
                C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                flags,
                proc,
                data));
      end Tcl_TraceVar;

      function Tcl_TraceVar2
        (interp : in Tcl_Interp;
         part1  : in String;
         part2  : in String;
         flags  : in C.int;
         proc   : in Tcl_VarTraceProc;
         data   : in ClientData)
         return   C.int
      is
         C_part1 : aliased C.char_array := C.To_C (part1);
         C_part2 : aliased C.char_array := C.To_C (part2);
      begin --  Tcl_TraceVar2
         return Tcl_TraceVar2
                  (interp,
                   C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                   C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                   flags,
                   proc,
                   data);
      end Tcl_TraceVar2;

      procedure Tcl_TraceVar2
        (interp : in Tcl_Interp;
         part1  : in String;
         part2  : in String;
         flags  : in C.int;
         proc   : in Tcl_VarTraceProc;
         data   : in ClientData)
      is
         C_part1 : aliased C.char_array := C.To_C (part1);
         C_part2 : aliased C.char_array := C.To_C (part2);
      begin --  Tcl_TraceVar2
         Assert
           (interp,
            Tcl_TraceVar2
               (interp,
                C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                flags,
                proc,
                data));
      end Tcl_TraceVar2;

      procedure Tcl_UntraceVar
        (interp  : in Tcl_Interp;
         varName : in String;
         flags   : in C.int;
         proc    : in Tcl_VarTraceProc;
         data    : in ClientData)
      is
         C_varName : aliased C.char_array := C.To_C (varName);
      begin --  Tcl_UntraceVar
         Tcl_UntraceVar
           (interp,
            C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
            flags,
            proc,
            data);
      end Tcl_UntraceVar;

      procedure Tcl_UntraceVar2
        (interp : in Tcl_Interp;
         part1  : in String;
         part2  : in String;
         flags  : in C.int;
         proc   : in Tcl_VarTraceProc;
         data   : in ClientData)
      is
         C_part1 : aliased C.char_array := C.To_C (part1);
         C_part2 : aliased C.char_array := C.To_C (part2);
      begin --  Tcl_UntraceVar2
         Tcl_UntraceVar2
           (interp,
            C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
            C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
            flags,
            proc,
            data);
      end Tcl_UntraceVar2;

      function Tcl_VarTraceInfo
        (interp         : in Tcl_Interp;
         varName        : in String;
         flags          : in C.int;
         procPtr        : in Tcl_VarTraceProc;
         prevclientdata : in ClientData)
         return           ClientData
      is
         C_varName : aliased C.char_array := C.To_C (varName);
      begin --  Tcl_VarTraceInfo
         return Tcl_VarTraceInfo
                  (interp,
                   C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                   flags,
                   procPtr,
                   prevclientdata);
      end Tcl_VarTraceInfo;

      function Tcl_VarTraceInfo2
        (interp         : in Tcl_Interp;
         part1          : in String;
         part2          : in String;
         flags          : in C.int;
         procPtr        : in Tcl_VarTraceProc;
         prevclientdata : in ClientData)
         return           ClientData
      is
         C_part1 : aliased C.char_array := C.To_C (part1);
         C_part2 : aliased C.char_array := C.To_C (part2);
      begin --  Tcl_VarTraceInfo2
         return Tcl_VarTraceInfo2
                  (interp,
                   C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                   C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                   flags,
                   procPtr,
                   prevclientdata);
      end Tcl_VarTraceInfo2;

   end Generic_TraceVar;

   function Tcl_DStringValue (dsPtr : in Tcl_DString) return String is
   begin --  Tcl_DStringValue
      return CHelper.Value (Tcl.Tcl_DStringValue (dsPtr));
   end Tcl_DStringValue;

   function Tcl_DbCkalloc
     (size : in C.unsigned;
      file : in String;
      line : in C.int)
      return String
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbCkalloc
      return CHelper.Value
               (Tcl.Tcl_DbCkalloc
                   (size,
                    C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                    line));
   end Tcl_DbCkalloc;

   function Tcl_DbCkfree
     (ptr  : in String;
      file : in String;
      line : in C.int)
      return C.int
   is
      C_ptr  : aliased C.char_array := C.To_C (ptr);
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbCkfree
      return Tcl.Tcl_DbCkfree
               (C.Strings.To_Chars_Ptr (C_ptr'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbCkfree;

   function Tcl_DbCkrealloc
     (ptr  : in String;
      size : in C.unsigned;
      file : in String;
      line : in C.int)
      return String
   is
      C_ptr  : aliased C.char_array := C.To_C (ptr);
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbCkrealloc
      return CHelper.Value
               (Tcl.Tcl_DbCkrealloc
                   (C.Strings.To_Chars_Ptr (C_ptr'Unchecked_Access),
                    size,
                    C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                    line));
   end Tcl_DbCkrealloc;

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
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "")
   is
      C_String1 : aliased C.char_array := C.To_C (String1);
      C_String2 : aliased C.char_array := C.To_C (String2);
      C_String3 : aliased C.char_array := C.To_C (String3);
      C_String4 : aliased C.char_array := C.To_C (String4);
      C_String5 : aliased C.char_array := C.To_C (String5);
      C_String6 : aliased C.char_array := C.To_C (String6);
      C_String7 : aliased C.char_array := C.To_C (String7);
      C_String8 : aliased C.char_array := C.To_C (String8);
      C_String9 : aliased C.char_array := C.To_C (String9);
   begin --  Tcl_AppendStringsToObj
      AppendStringsToObj
        (objPtr,
         C.Strings.To_Chars_Ptr (C_String1'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String2'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String3'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String4'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String5'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String6'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String7'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String8'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String9'Unchecked_Access),
         C.Strings.Null_Ptr);
   end Tcl_AppendStringsToObj;

   procedure Tcl_AppendToObj
     (objPtr : in Tcl_Obj;
      bytes  : in String;
      length : in C.int)
   is
      C_bytes : aliased C.char_array := C.To_C (bytes);
   begin --  Tcl_AppendToObj
      Tcl.Tcl_AppendToObj
        (objPtr,
         C.Strings.To_Chars_Ptr (C_bytes'Unchecked_Access),
         length);
   end Tcl_AppendToObj;

   procedure Tcl_DbDecrRefCount
     (objPtr : in Tcl_Obj;
      file   : in String;
      line   : in C.int)
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbDecrRefCount
      Tcl.Tcl_DbDecrRefCount
        (objPtr,
         C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
         line);
   end Tcl_DbDecrRefCount;

   procedure Tcl_DbIncrRefCount
     (objPtr : in Tcl_Obj;
      file   : in String;
      line   : in C.int)
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbIncrRefCount
      Tcl.Tcl_DbIncrRefCount
        (objPtr,
         C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
         line);
   end Tcl_DbIncrRefCount;

   function Tcl_DbIsShared
     (objPtr : in Tcl_Obj;
      file   : in String;
      line   : in C.int)
      return   C.int
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbIsShared
      return Tcl.Tcl_DbIsShared
               (objPtr,
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbIsShared;

   function Tcl_DbNewBooleanObj
     (boolValue : in C.int;
      file      : in String;
      line      : in C.int)
      return      Tcl_Obj
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbNewBooleanObj
      return Tcl.Tcl_DbNewBooleanObj
               (boolValue,
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbNewBooleanObj;

   function Tcl_DbNewByteArrayObj
     (bytes  : in String;
      length : in C.int;
      file   : in String;
      line   : in C.int)
      return   Tcl_Obj
   is
      C_bytes : aliased C.char_array := C.To_C (bytes);
      C_file  : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbNewByteArrayObj
      return Tcl.Tcl_DbNewByteArrayObj
               (C.Strings.To_Chars_Ptr (C_bytes'Unchecked_Access),
                length,
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbNewByteArrayObj;

   function Tcl_DbNewDoubleObj
     (doubleValue : in C.double;
      file        : in String;
      line        : in C.int)
      return        Tcl_Obj
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbNewDoubleObj
      return Tcl.Tcl_DbNewDoubleObj
               (doubleValue,
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbNewDoubleObj;

   function Tcl_DbNewListObj
     (objc : in C.int;
      objv : in Tcl_Obj_Array;
      file : in String;
      line : in C.int)
      return Tcl_Obj
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbNewListObj
      return Tcl.Tcl_DbNewListObj
               (objc,
                objv,
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbNewListObj;

   function Tcl_DbNewLongObj
     (longValue : in C.long;
      file      : in String;
      line      : in C.int)
      return      Tcl_Obj
   is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbNewLongObj
      return Tcl.Tcl_DbNewLongObj
               (longValue,
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbNewLongObj;

   function Tcl_DbNewObj (file : in String; line : in C.int) return Tcl_Obj is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbNewObj
      return Tcl.Tcl_DbNewObj
               (C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbNewObj;

   function Tcl_DbNewStringObj
     (bytes  : in String;
      length : in C.int;
      file   : in String;
      line   : in C.int)
      return   Tcl_Obj
   is
      C_bytes : aliased C.char_array := C.To_C (bytes);
      C_file  : aliased C.char_array := C.To_C (file);
   begin --  Tcl_DbNewStringObj
      return Tcl.Tcl_DbNewStringObj
               (C.Strings.To_Chars_Ptr (C_bytes'Unchecked_Access),
                length,
                C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
                line);
   end Tcl_DbNewStringObj;

   function Tcl_GetBoolean
     (interp  : in Tcl_Interp;
      str     : in String;
      boolPtr : access C.int)
      return    C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_GetBoolean
      return Tcl.Tcl_GetBoolean
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                boolPtr);
   end Tcl_GetBoolean;

   procedure Tcl_GetBoolean
     (interp  : in Tcl_Interp;
      str     : in String;
      boolPtr : access C.int)
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_GetBoolean
      Assert
        (interp,
         Tcl.Tcl_GetBoolean
            (interp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             boolPtr));
   end Tcl_GetBoolean;

   function Tcl_GetByteArrayFromObj
     (objPtr    : in Tcl_Obj;
      lengthPtr : access C.int)
      return      String
   is
   begin --  Tcl_GetByteArrayFromObj
      return CHelper.Value (Tcl.Tcl_GetByteArrayFromObj (objPtr, lengthPtr));
   end Tcl_GetByteArrayFromObj;

   function Tcl_GetDouble
     (interp    : in Tcl_Interp;
      str       : in String;
      doublePtr : access C.double)
      return      C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_GetDouble
      return Tcl.Tcl_GetDouble
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                doublePtr);
   end Tcl_GetDouble;

   procedure Tcl_GetDouble
     (interp    : in Tcl_Interp;
      str       : in String;
      doublePtr : access C.double)
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_GetDouble
      Assert
        (interp,
         Tcl.Tcl_GetDouble
            (interp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             doublePtr));
   end Tcl_GetDouble;

   function Tcl_GetIndexFromObj
     (interp   : in Tcl_Interp;
      objPtr   : in Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      msg      : in String;
      flags    : in C.int;
      indexPtr : access C.int)
      return     C.int
   is
      C_msg : aliased C.char_array := C.To_C (msg);
   begin --  Tcl_GetIndexFromObj
      return Tcl.Tcl_GetIndexFromObj
               (interp,
                objPtr,
                tablePtr,
                C.Strings.To_Chars_Ptr (C_msg'Unchecked_Access),
                flags,
                indexPtr);
   end Tcl_GetIndexFromObj;

   procedure Tcl_GetIndexFromObj
     (interp   : in Tcl_Interp;
      objPtr   : in Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      msg      : in String;
      flags    : in C.int;
      indexPtr : access C.int)
   is
      C_msg : aliased C.char_array := C.To_C (msg);
   begin --  Tcl_GetIndexFromObj
      Assert
        (interp,
         Tcl.Tcl_GetIndexFromObj
            (interp,
             objPtr,
             tablePtr,
             C.Strings.To_Chars_Ptr (C_msg'Unchecked_Access),
             flags,
             indexPtr));
   end Tcl_GetIndexFromObj;

   function Tcl_GetInt
     (interp : in Tcl_Interp;
      str    : in String;
      intPtr : access C.int)
      return   C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_GetInt
      return Tcl.Tcl_GetInt
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                intPtr);
   end Tcl_GetInt;

   procedure Tcl_GetInt
     (interp : in Tcl_Interp;
      str    : in String;
      intPtr : access C.int)
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_GetInt
      Assert
        (interp,
         Tcl.Tcl_GetInt
            (interp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             intPtr));
   end Tcl_GetInt;

   function Tcl_GetObjType (typeName : in String) return Tcl_ObjType is
      C_typeName : aliased C.char_array := C.To_C (typeName);
   begin --  Tcl_GetObjType
      return Tcl.Tcl_GetObjType
               (C.Strings.To_Chars_Ptr (C_typeName'Unchecked_Access));
   end Tcl_GetObjType;

   function Tcl_GetObjTypeName (objPtr : in Tcl_Obj) return String is
   begin --  Tcl_GetObjTypeName
      return CHelper.Value (Tcl.Tcl_GetObjTypeName (objPtr));
   end Tcl_GetObjTypeName;

   function Tcl_GetStringFromObj
     (objPtr    : in Tcl_Obj;
      lengthPtr : access C.int)
      return      String
   is
   begin --  Tcl_GetStringFromObj
      return CHelper.Value (Tcl.Tcl_GetStringFromObj (objPtr, lengthPtr));
   end Tcl_GetStringFromObj;

   function Tcl_NewByteArrayObj
     (bytes  : in String;
      length : in C.int)
      return   Tcl_Obj
   is
      C_bytes : aliased C.char_array := C.To_C (bytes);
   begin --  Tcl_NewByteArrayObj
      return Tcl.Tcl_NewByteArrayObj
               (C.Strings.To_Chars_Ptr (C_bytes'Unchecked_Access),
                length);
   end Tcl_NewByteArrayObj;

   function Tcl_NewStringObj
     (bytes  : in String;
      length : in C.int)
      return   Tcl_Obj
   is
      C_bytes : aliased C.char_array := C.To_C (bytes);
   begin --  Tcl_NewStringObj
      return Tcl.Tcl_NewStringObj
               (C.Strings.To_Chars_Ptr (C_bytes'Unchecked_Access),
                length);
   end Tcl_NewStringObj;

   function Tcl_SetByteArrayLength
     (objPtr : in Tcl_Obj;
      length : in C.int)
      return   String
   is
   begin --  Tcl_SetByteArrayLength
      return CHelper.Value (Tcl.Tcl_SetByteArrayLength (objPtr, length));
   end Tcl_SetByteArrayLength;

   procedure Tcl_SetByteArrayObj
     (objPtr : in Tcl_Obj;
      bytes  : in String;
      length : in C.int)
   is
      C_bytes : aliased C.char_array := C.To_C (bytes);
   begin --  Tcl_SetByteArrayObj
      Tcl.Tcl_SetByteArrayObj
        (objPtr,
         C.Strings.To_Chars_Ptr (C_bytes'Unchecked_Access),
         length);
   end Tcl_SetByteArrayObj;

   procedure Tcl_SetStringObj
     (objPtr : in Tcl_Obj;
      bytes  : in String;
      length : in C.int)
   is
      C_bytes : aliased C.char_array := C.To_C (bytes);
   begin --  Tcl_SetStringObj
      Tcl.Tcl_SetStringObj
        (objPtr,
         C.Strings.To_Chars_Ptr (C_bytes'Unchecked_Access),
         length);
   end Tcl_SetStringObj;

   procedure Tcl_AddErrorInfo (interp : in Tcl_Interp; message : in String) is
      C_message : aliased C.char_array := C.To_C (message);
   begin --  Tcl_AddErrorInfo
      Tcl.Tcl_AddErrorInfo
        (interp,
         C.Strings.To_Chars_Ptr (C_message'Unchecked_Access));
   end Tcl_AddErrorInfo;

   procedure Tcl_AddObjErrorInfo
     (interp  : in Tcl_Interp;
      message : in String;
      length  : in C.int)
   is
      C_message : aliased C.char_array := C.To_C (message);
   begin --  Tcl_AddObjErrorInfo
      Tcl.Tcl_AddObjErrorInfo
        (interp,
         C.Strings.To_Chars_Ptr (C_message'Unchecked_Access),
         length);
   end Tcl_AddObjErrorInfo;

   procedure Tcl_AppendElement (interp : in Tcl_Interp; strng : in String) is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_AppendElement
      Tcl.Tcl_AppendElement
        (interp,
         C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access));
   end Tcl_AppendElement;

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
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "")
   is
      C_String1 : aliased C.char_array := C.To_C (String1);
      C_String2 : aliased C.char_array := C.To_C (String2);
      C_String3 : aliased C.char_array := C.To_C (String3);
      C_String4 : aliased C.char_array := C.To_C (String4);
      C_String5 : aliased C.char_array := C.To_C (String5);
      C_String6 : aliased C.char_array := C.To_C (String6);
      C_String7 : aliased C.char_array := C.To_C (String7);
      C_String8 : aliased C.char_array := C.To_C (String8);
      C_String9 : aliased C.char_array := C.To_C (String9);
   begin --  Tcl_AppendResult
      AppendResult
        (interp,
         C.Strings.To_Chars_Ptr (C_String1'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String2'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String3'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String4'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String5'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String6'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String7'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String8'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String9'Unchecked_Access),
         C.Strings.Null_Ptr);
   end Tcl_AppendResult;

   function Tcl_Backslash
     (src     : in String;
      readPtr : access C.int)
      return    C.char
   is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_Backslash
      return Tcl.Tcl_Backslash
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                readPtr);
   end Tcl_Backslash;

   function Tcl_CommandComplete (cmd : in String) return C.int is
      C_cmd : aliased C.char_array := C.To_C (cmd);
   begin --  Tcl_CommandComplete
      return Tcl.Tcl_CommandComplete
               (C.Strings.To_Chars_Ptr (C_cmd'Unchecked_Access));
   end Tcl_CommandComplete;

   function Tcl_Concat
     (argc : in C.int;
      argv : in CArgv.Chars_Ptr_Ptr)
      return String
   is
   begin --  Tcl_Concat
      return CHelper.Value (Tcl.Tcl_Concat (argc, argv));
   end Tcl_Concat;

   function Tcl_ConvertElement
     (src   : in String;
      dst   : in String;
      flags : in C.int)
      return  C.int
   is
      C_src : aliased C.char_array := C.To_C (src);
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_ConvertElement
      return Tcl.Tcl_ConvertElement
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access),
                flags);
   end Tcl_ConvertElement;

   function Tcl_ConvertCountedElement
     (src    : in String;
      length : in C.int;
      dst    : in String;
      flags  : in C.int)
      return   C.int
   is
      C_src : aliased C.char_array := C.To_C (src);
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_ConvertCountedElement
      return Tcl.Tcl_ConvertCountedElement
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                length,
                C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access),
                flags);
   end Tcl_ConvertCountedElement;

   function Tcl_CreateAlias
     (slave     : in Tcl_Interp;
      slaveCmd  : in String;
      target    : in Tcl_Interp;
      targetCmd : in String;
      argc      : in C.int;
      argv      : in CArgv.Chars_Ptr_Ptr)
      return      C.int
   is
      C_slaveCmd  : aliased C.char_array := C.To_C (slaveCmd);
      C_targetCmd : aliased C.char_array := C.To_C (targetCmd);
   begin --  Tcl_CreateAlias
      return Tcl.Tcl_CreateAlias
               (slave,
                C.Strings.To_Chars_Ptr (C_slaveCmd'Unchecked_Access),
                target,
                C.Strings.To_Chars_Ptr (C_targetCmd'Unchecked_Access),
                argc,
                argv);
   end Tcl_CreateAlias;

   function Tcl_CreateAliasObj
     (slave     : in Tcl_Interp;
      slaveCmd  : in String;
      target    : in Tcl_Interp;
      targetCmd : in String;
      objc      : in C.int;
      objv      : in Tcl_Obj_Array)
      return      C.int
   is
      C_slaveCmd  : aliased C.char_array := C.To_C (slaveCmd);
      C_targetCmd : aliased C.char_array := C.To_C (targetCmd);
   begin --  Tcl_CreateAliasObj
      return Tcl.Tcl_CreateAliasObj
               (slave,
                C.Strings.To_Chars_Ptr (C_slaveCmd'Unchecked_Access),
                target,
                C.Strings.To_Chars_Ptr (C_targetCmd'Unchecked_Access),
                objc,
                objv);
   end Tcl_CreateAliasObj;

   function Tcl_CreateSlave
     (interp    : in Tcl_Interp;
      slaveName : in String;
      isSafe    : in C.int)
      return      Tcl_Interp
   is
      C_slaveName : aliased C.char_array := C.To_C (slaveName);
   begin --  Tcl_CreateSlave
      return Tcl.Tcl_CreateSlave
               (interp,
                C.Strings.To_Chars_Ptr (C_slaveName'Unchecked_Access),
                isSafe);
   end Tcl_CreateSlave;

   function Tcl_DStringAppend
     (dsPtr  : in Tcl_DString;
      str    : in String;
      length : in C.int)
      return   String
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_DStringAppend
      return CHelper.Value
               (Tcl.Tcl_DStringAppend
                   (dsPtr,
                    C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                    length));
   end Tcl_DStringAppend;

   function Tcl_DStringAppendElement
     (dsPtr : in Tcl_DString;
      strng : in String)
      return  String
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_DStringAppendElement
      return CHelper.Value
               (Tcl.Tcl_DStringAppendElement
                   (dsPtr,
                    C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access)));
   end Tcl_DStringAppendElement;

   function Tcl_ErrnoId return C.Strings.chars_ptr is
   begin --  Tcl_ErrnoId
      return Tcl.Tcl_ErrnoId;
   end Tcl_ErrnoId;

   function Tcl_ErrnoMsg (err : in C.int) return String is
   begin --  Tcl_ErrnoMsg
      return CHelper.Value (Tcl.Tcl_ErrnoMsg (err));
   end Tcl_ErrnoMsg;

   function Tcl_Eval
     (interp : in Tcl_Interp;
      strng  : in String)
      return   C.int
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_Eval
      return Tcl.Tcl_Eval
               (interp,
                C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access));
   end Tcl_Eval;

   procedure Tcl_Eval (interp : in Tcl_Interp; strng : in String) is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_Eval
      Assert
        (interp,
         Tcl.Tcl_Eval
            (interp,
             C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access)));
   end Tcl_Eval;

   function Tcl_EvalFile
     (interp   : in Tcl_Interp;
      fileName : in String)
      return     C.int
   is
      C_fileName : aliased C.char_array := C.To_C (fileName);
   begin --  Tcl_EvalFile
      return Tcl.Tcl_EvalFile
               (interp,
                C.Strings.To_Chars_Ptr (C_fileName'Unchecked_Access));
   end Tcl_EvalFile;

   procedure Tcl_EvalFile (interp : in Tcl_Interp; fileName : in String) is
      C_fileName : aliased C.char_array := C.To_C (fileName);
   begin --  Tcl_EvalFile
      Assert
        (interp,
         Tcl.Tcl_EvalFile
            (interp,
             C.Strings.To_Chars_Ptr (C_fileName'Unchecked_Access)));
   end Tcl_EvalFile;

   function Tcl_ExposeCommand
     (interp         : in Tcl_Interp;
      hiddenCmdToken : in String;
      cmdName        : in String)
      return           C.int
   is
      C_hiddenCmdToken : aliased C.char_array := C.To_C (hiddenCmdToken);
      C_cmdName        : aliased C.char_array := C.To_C (cmdName);
   begin --  Tcl_ExposeCommand
      return Tcl.Tcl_ExposeCommand
               (interp,
                C.Strings.To_Chars_Ptr (C_hiddenCmdToken'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_cmdName'Unchecked_Access));
   end Tcl_ExposeCommand;

   procedure Tcl_ExposeCommand
     (interp         : in Tcl_Interp;
      hiddenCmdToken : in String;
      cmdName        : in String)
   is
      C_hiddenCmdToken : aliased C.char_array := C.To_C (hiddenCmdToken);
      C_cmdName        : aliased C.char_array := C.To_C (cmdName);
   begin --  Tcl_ExposeCommand
      Assert
        (interp,
         Tcl.Tcl_ExposeCommand
            (interp,
             C.Strings.To_Chars_Ptr (C_hiddenCmdToken'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_cmdName'Unchecked_Access)));
   end Tcl_ExposeCommand;

   function Tcl_ExprBoolean
     (interp : in Tcl_Interp;
      str    : in String)
      return   Boolean
   is
      C_str     : aliased C.char_array := C.To_C (str);
      local_ptr : aliased C.int;
   begin --  Tcl_ExprBoolean
      Assert
        (interp,
         Tcl.Tcl_ExprBoolean
            (interp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             local_ptr'Access));
      return Boolean'Val (local_ptr);
   end Tcl_ExprBoolean;

   function Tcl_ExprDouble
     (interp : in Tcl_Interp;
      str    : in String)
      return   C.double
   is
      C_str     : aliased C.char_array := C.To_C (str);
      local_ptr : aliased C.double;
   begin --  Tcl_ExprDouble
      Assert
        (interp,
         Tcl.Tcl_ExprDouble
            (interp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             local_ptr'Access));
      return local_ptr;
   end Tcl_ExprDouble;

   function Tcl_ExprLong
     (interp : in Tcl_Interp;
      str    : in String)
      return   C.long
   is
      C_str     : aliased C.char_array := C.To_C (str);
      local_ptr : aliased C.long;
   begin --  Tcl_ExprLong
      Assert
        (interp,
         Tcl.Tcl_ExprLong
            (interp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             local_ptr'Access));
      return local_ptr;
   end Tcl_ExprLong;

   function Tcl_ExprString
     (interp : in Tcl_Interp;
      strng  : in String)
      return   String
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ExprString
      Assert
        (interp,
         Tcl.Tcl_ExprString
            (interp,
             C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access)));
      return CHelper.Value (Tcl_GetResult (interp));
   end Tcl_ExprString;

   procedure Tcl_FindExecutable (argv0 : in String) is
      C_argv0 : aliased C.char_array := C.To_C (argv0);
   begin --  Tcl_FindExecutable
      Tcl.Tcl_FindExecutable
        (C.Strings.To_Chars_Ptr (C_argv0'Unchecked_Access));
   end Tcl_FindExecutable;

   function Tcl_GetAlias
     (interp          : in Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : in Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      argcPtr         : access C.int;
      argvPtr         : access CArgv.Chars_Ptr_Ptr)
      return            C.int
   is
      C_slaveCmd : aliased C.char_array := C.To_C (slaveCmd);
   begin --  Tcl_GetAlias
      return Tcl.Tcl_GetAlias
               (interp,
                C.Strings.To_Chars_Ptr (C_slaveCmd'Unchecked_Access),
                targetInterpPtr,
                targetCmdPtr,
                argcPtr,
                argvPtr);
   end Tcl_GetAlias;

   procedure Tcl_GetAlias
     (interp          : in Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : in Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      argcPtr         : access C.int;
      argvPtr         : access CArgv.Chars_Ptr_Ptr)
   is
      C_slaveCmd : aliased C.char_array := C.To_C (slaveCmd);
   begin --  Tcl_GetAlias
      Assert
        (interp,
         Tcl.Tcl_GetAlias
            (interp,
             C.Strings.To_Chars_Ptr (C_slaveCmd'Unchecked_Access),
             targetInterpPtr,
             targetCmdPtr,
             argcPtr,
             argvPtr));
   end Tcl_GetAlias;

   function Tcl_GetAliasObj
     (interp          : in Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : in Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      objcPtr         : access C.int;
      objv            : in Tcl_Obj_Array)
      return            C.int
   is
      C_slaveCmd : aliased C.char_array := C.To_C (slaveCmd);
   begin --  Tcl_GetAliasObj
      return Tcl.Tcl_GetAliasObj
               (interp,
                C.Strings.To_Chars_Ptr (C_slaveCmd'Unchecked_Access),
                targetInterpPtr,
                targetCmdPtr,
                objcPtr,
                objv);
   end Tcl_GetAliasObj;

   procedure Tcl_GetAliasObj
     (interp          : in Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : in Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      objcPtr         : access C.int;
      objv            : in Tcl_Obj_Array)
   is
      C_slaveCmd : aliased C.char_array := C.To_C (slaveCmd);
   begin --  Tcl_GetAliasObj
      Assert
        (interp,
         Tcl.Tcl_GetAliasObj
            (interp,
             C.Strings.To_Chars_Ptr (C_slaveCmd'Unchecked_Access),
             targetInterpPtr,
             targetCmdPtr,
             objcPtr,
             objv));
   end Tcl_GetAliasObj;

   function Tcl_GetHostName return C.Strings.chars_ptr is
   begin --  Tcl_GetHostName
      return Tcl.Tcl_GetHostName;
   end Tcl_GetHostName;

   function Tcl_GetNameOfExecutable return C.Strings.chars_ptr is
   begin --  Tcl_GetNameOfExecutable
      return Tcl.Tcl_GetNameOfExecutable;
   end Tcl_GetNameOfExecutable;

   function Tcl_GetPathType (path : in String) return Tcl_PathType is
      C_path : aliased C.char_array := C.To_C (path);
   begin --  Tcl_GetPathType
      return Tcl.Tcl_GetPathType
               (C.Strings.To_Chars_Ptr (C_path'Unchecked_Access));
   end Tcl_GetPathType;

   function Tcl_GetResult (interp : in Tcl_Interp) return String is
   begin --  Tcl_GetResult
      return CHelper.Value (Tcl.Tcl_GetResult (interp));
   end Tcl_GetResult;

   function Tcl_GetSlave
     (interp    : in Tcl_Interp;
      slaveName : in String)
      return      Tcl_Interp
   is
      C_slaveName : aliased C.char_array := C.To_C (slaveName);
   begin --  Tcl_GetSlave
      return Tcl.Tcl_GetSlave
               (interp,
                C.Strings.To_Chars_Ptr (C_slaveName'Unchecked_Access));
   end Tcl_GetSlave;

   function Tcl_GetStringResult (interp : in Tcl_Interp) return String is
   begin --  Tcl_GetStringResult
      return CHelper.Value (Tcl.Tcl_GetStringResult (interp));
   end Tcl_GetStringResult;

   function Tcl_GetVar
     (interp  : in Tcl_Interp;
      varName : in String;
      flags   : in C.int := TCL_GLOBAL_ONLY)
      return    String
   is
      C_varName : aliased C.char_array := C.To_C (varName);
   begin --  Tcl_GetVar
      return CHelper.Value
               (Tcl.Tcl_GetVar
                   (interp,
                    C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                    flags));
   end Tcl_GetVar;

   function Tcl_GetVar2
     (interp : in Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY)
      return   String
   is
      C_part1 : aliased C.char_array := C.To_C (part1);
      C_part2 : aliased C.char_array := C.To_C (part2);
   begin --  Tcl_GetVar2
      return CHelper.Value
               (Tcl.Tcl_GetVar2
                   (interp,
                    C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                    C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                    flags));
   end Tcl_GetVar2;

   function Tcl_GlobalEval
     (interp  : in Tcl_Interp;
      command : in String)
      return    C.int
   is
      C_command : aliased C.char_array := C.To_C (command);
   begin --  Tcl_GlobalEval
      return Tcl.Tcl_GlobalEval
               (interp,
                C.Strings.To_Chars_Ptr (C_command'Unchecked_Access));
   end Tcl_GlobalEval;

   procedure Tcl_GlobalEval (interp : in Tcl_Interp; command : in String) is
      C_command : aliased C.char_array := C.To_C (command);
   begin --  Tcl_GlobalEval
      Assert
        (interp,
         Tcl.Tcl_GlobalEval
            (interp,
             C.Strings.To_Chars_Ptr (C_command'Unchecked_Access)));
   end Tcl_GlobalEval;

   function Tcl_HideCommand
     (interp         : in Tcl_Interp;
      cmdName        : in String;
      hiddenCmdToken : in String)
      return           C.int
   is
      C_cmdName        : aliased C.char_array := C.To_C (cmdName);
      C_hiddenCmdToken : aliased C.char_array := C.To_C (hiddenCmdToken);
   begin --  Tcl_HideCommand
      return Tcl.Tcl_HideCommand
               (interp,
                C.Strings.To_Chars_Ptr (C_cmdName'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_hiddenCmdToken'Unchecked_Access));
   end Tcl_HideCommand;

   procedure Tcl_HideCommand
     (interp         : in Tcl_Interp;
      cmdName        : in String;
      hiddenCmdToken : in String)
   is
      C_cmdName        : aliased C.char_array := C.To_C (cmdName);
      C_hiddenCmdToken : aliased C.char_array := C.To_C (hiddenCmdToken);
   begin --  Tcl_HideCommand
      Assert
        (interp,
         Tcl.Tcl_HideCommand
            (interp,
             C.Strings.To_Chars_Ptr (C_cmdName'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_hiddenCmdToken'Unchecked_Access)));
   end Tcl_HideCommand;

   function Tcl_JoinPath
     (argc      : in C.int;
      argv      : in CArgv.Chars_Ptr_Ptr;
      resultPtr : in Tcl_DString)
      return      String
   is
   begin --  Tcl_JoinPath
      return CHelper.Value (Tcl.Tcl_JoinPath (argc, argv, resultPtr));
   end Tcl_JoinPath;

   function Tcl_LinkVar
     (interp  : in Tcl_Interp;
      varName : in String;
      addr    : in System.Address;
      typ     : in C.int)
      return    C.int
   is
      C_varName : aliased C.char_array := C.To_C (varName);
   begin --  Tcl_LinkVar
      return Tcl.Tcl_LinkVar
               (interp,
                C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                addr,
                typ);
   end Tcl_LinkVar;

   procedure Tcl_LinkVar
     (interp  : in Tcl_Interp;
      varName : in String;
      addr    : in System.Address;
      typ     : in C.int)
   is
      C_varName : aliased C.char_array := C.To_C (varName);
   begin --  Tcl_LinkVar
      Assert
        (interp,
         Tcl.Tcl_LinkVar
            (interp,
             C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
             addr,
             typ));
   end Tcl_LinkVar;

   function Tcl_Merge
     (argc : in C.int;
      argv : in CArgv.Chars_Ptr_Ptr)
      return String
   is
   begin --  Tcl_Merge
      return CHelper.Value (Tcl.Tcl_Merge (argc, argv));
   end Tcl_Merge;

   procedure Tcl_PrintDouble
     (interp : in Tcl_Interp;
      value  : in C.double;
      dst    : in String)
   is
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_PrintDouble
      Tcl.Tcl_PrintDouble
        (interp,
         value,
         C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access));
   end Tcl_PrintDouble;

   function Tcl_PutEnv (strng : in String) return C.int is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_PutEnv
      return Tcl.Tcl_PutEnv
               (C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access));
   end Tcl_PutEnv;

   function Tcl_PosixError (interp : in Tcl_Interp) return String is
   begin --  Tcl_PosixError
      return CHelper.Value (Tcl.Tcl_PosixError (interp));
   end Tcl_PosixError;

   function Tcl_RecordAndEval
     (interp : in Tcl_Interp;
      cmd    : in String;
      flags  : in C.int)
      return   C.int
   is
      C_cmd : aliased C.char_array := C.To_C (cmd);
   begin --  Tcl_RecordAndEval
      return Tcl.Tcl_RecordAndEval
               (interp,
                C.Strings.To_Chars_Ptr (C_cmd'Unchecked_Access),
                flags);
   end Tcl_RecordAndEval;

   procedure Tcl_RecordAndEval
     (interp : in Tcl_Interp;
      cmd    : in String;
      flags  : in C.int)
   is
      C_cmd : aliased C.char_array := C.To_C (cmd);
   begin --  Tcl_RecordAndEval
      Assert
        (interp,
         Tcl.Tcl_RecordAndEval
            (interp,
             C.Strings.To_Chars_Ptr (C_cmd'Unchecked_Access),
             flags));
   end Tcl_RecordAndEval;

   function Tcl_RegExpCompile
     (interp : in Tcl_Interp;
      strng  : in String)
      return   Tcl_RegExp
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_RegExpCompile
      return Tcl.Tcl_RegExpCompile
               (interp,
                C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access));
   end Tcl_RegExpCompile;

   function Tcl_RegExpExec
     (interp : in Tcl_Interp;
      regexp : in Tcl_RegExp;
      str    : in String;
      start  : in String)
      return   C.int
   is
      C_str   : aliased C.char_array := C.To_C (str);
      C_start : aliased C.char_array := C.To_C (start);
   begin --  Tcl_RegExpExec
      return Tcl.Tcl_RegExpExec
               (interp,
                regexp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_start'Unchecked_Access));
   end Tcl_RegExpExec;

   procedure Tcl_RegExpExec
     (interp : in Tcl_Interp;
      regexp : in Tcl_RegExp;
      str    : in String;
      start  : in String)
   is
      C_str   : aliased C.char_array := C.To_C (str);
      C_start : aliased C.char_array := C.To_C (start);
   begin --  Tcl_RegExpExec
      Assert
        (interp,
         Tcl.Tcl_RegExpExec
            (interp,
             regexp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_start'Unchecked_Access)));
   end Tcl_RegExpExec;

   function Tcl_RegExpMatch
     (interp  : in Tcl_Interp;
      str     : in String;
      pattern : in String)
      return    C.int
   is
      C_str     : aliased C.char_array := C.To_C (str);
      C_pattern : aliased C.char_array := C.To_C (pattern);
   begin --  Tcl_RegExpMatch
      return Tcl.Tcl_RegExpMatch
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_pattern'Unchecked_Access));
   end Tcl_RegExpMatch;

   procedure Tcl_RegExpMatch
     (interp  : in Tcl_Interp;
      str     : in String;
      pattern : in String)
   is
      C_str     : aliased C.char_array := C.To_C (str);
      C_pattern : aliased C.char_array := C.To_C (pattern);
   begin --  Tcl_RegExpMatch
      Assert
        (interp,
         Tcl.Tcl_RegExpMatch
            (interp,
             C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_pattern'Unchecked_Access)));
   end Tcl_RegExpMatch;

   function Tcl_ScanElement
     (str     : in String;
      flagPtr : access C.int)
      return    C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_ScanElement
      return Tcl.Tcl_ScanElement
               (C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                flagPtr);
   end Tcl_ScanElement;

   function Tcl_ScanCountedElement
     (str     : in String;
      length  : in C.int;
      flagPtr : access C.int)
      return    C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_ScanCountedElement
      return Tcl.Tcl_ScanCountedElement
               (C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                length,
                flagPtr);
   end Tcl_ScanCountedElement;

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
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "")
   is
      C_String1 : aliased C.char_array := C.To_C (String1);
      C_String2 : aliased C.char_array := C.To_C (String2);
      C_String3 : aliased C.char_array := C.To_C (String3);
      C_String4 : aliased C.char_array := C.To_C (String4);
      C_String5 : aliased C.char_array := C.To_C (String5);
      C_String6 : aliased C.char_array := C.To_C (String6);
      C_String7 : aliased C.char_array := C.To_C (String7);
      C_String8 : aliased C.char_array := C.To_C (String8);
      C_String9 : aliased C.char_array := C.To_C (String9);
   begin --  Tcl_SetErrorCode
      SetErrorCode
        (interp,
         C.Strings.To_Chars_Ptr (C_String1'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String2'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String3'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String4'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String5'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String6'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String7'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String8'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_String9'Unchecked_Access),
         C.Strings.Null_Ptr);
   end Tcl_SetErrorCode;

   procedure Tcl_SetResult (interp : in Tcl_Interp; str : in String) is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_SetResult
      Tcl.Tcl_SetResult
        (interp,
         C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
         C.int (TCL_VOLATILE));
   end Tcl_SetResult;

   function Tcl_SetVar
     (interp   : in Tcl_Interp;
      varName  : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY)
      return     String
   is
      C_varName  : aliased C.char_array := C.To_C (varName);
      C_newValue : aliased C.char_array := C.To_C (newValue);
   begin --  Tcl_SetVar
      return CHelper.Value
               (Tcl.Tcl_SetVar
                   (interp,
                    C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                    C.Strings.To_Chars_Ptr (C_newValue'Unchecked_Access),
                    flags));
   end Tcl_SetVar;

   procedure Tcl_SetVar
     (interp   : in Tcl_Interp;
      varName  : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY)
   is
      Result     : C.Strings.chars_ptr;
      C_varName  : aliased C.char_array := C.To_C (varName);
      C_newValue : aliased C.char_array := C.To_C (newValue);
      pragma Unreferenced (Result);  --  XXX why not?
   begin --  Tcl_SetVar
      Result :=
         Tcl.Tcl_SetVar
           (interp,
            C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
            C.Strings.To_Chars_Ptr (C_newValue'Unchecked_Access),
            flags);
   end Tcl_SetVar;

   function Tcl_SetVar2
     (interp   : in Tcl_Interp;
      part1    : in String;
      part2    : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY)
      return     String
   is
      C_part1    : aliased C.char_array := C.To_C (part1);
      C_part2    : aliased C.char_array := C.To_C (part2);
      C_newValue : aliased C.char_array := C.To_C (newValue);
   begin --  Tcl_SetVar2
      return CHelper.Value
               (Tcl.Tcl_SetVar2
                   (interp,
                    C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                    C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                    C.Strings.To_Chars_Ptr (C_newValue'Unchecked_Access),
                    flags));
   end Tcl_SetVar2;

   procedure Tcl_SetVar2
     (interp   : in Tcl_Interp;
      part1    : in String;
      part2    : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY)
   is
      Result     : C.Strings.chars_ptr;
      C_part1    : aliased C.char_array := C.To_C (part1);
      C_part2    : aliased C.char_array := C.To_C (part2);
      C_newValue : aliased C.char_array := C.To_C (newValue);
      pragma Unreferenced (Result);  --  XXX why not?
   begin --  Tcl_SetVar2
      Result :=
         Tcl.Tcl_SetVar2
           (interp,
            C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
            C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
            C.Strings.To_Chars_Ptr (C_newValue'Unchecked_Access),
            flags);
   end Tcl_SetVar2;

   function Tcl_SignalId (sig : in C.int) return String is
   begin --  Tcl_SignalId
      return CHelper.Value (Tcl.Tcl_SignalId (sig));
   end Tcl_SignalId;

   function Tcl_SignalMsg (sig : in C.int) return String is
   begin --  Tcl_SignalMsg
      return CHelper.Value (Tcl.Tcl_SignalMsg (sig));
   end Tcl_SignalMsg;

   function Tcl_SplitList
     (interp  : in Tcl_Interp;
      listStr : in String;
      argcPtr : access C.int;
      argvPtr : access CArgv.Chars_Ptr_Ptr)
      return    C.int
   is
      C_listStr : aliased C.char_array := C.To_C (listStr);
   begin --  Tcl_SplitList
      return Tcl.Tcl_SplitList
               (interp,
                C.Strings.To_Chars_Ptr (C_listStr'Unchecked_Access),
                argcPtr,
                argvPtr);
   end Tcl_SplitList;

   procedure Tcl_SplitList
     (interp  : in Tcl_Interp;
      listStr : in String;
      argcPtr : access C.int;
      argvPtr : access CArgv.Chars_Ptr_Ptr)
   is
      C_listStr : aliased C.char_array := C.To_C (listStr);
   begin --  Tcl_SplitList
      Assert
        (interp,
         Tcl.Tcl_SplitList
            (interp,
             C.Strings.To_Chars_Ptr (C_listStr'Unchecked_Access),
             argcPtr,
             argvPtr));
   end Tcl_SplitList;

   procedure Tcl_SplitPath
     (path    : in String;
      argcPtr : access C.int;
      argvPtr : access CArgv.Chars_Ptr_Ptr)
   is
      C_path : aliased C.char_array := C.To_C (path);
   begin --  Tcl_SplitPath
      Tcl.Tcl_SplitPath
        (C.Strings.To_Chars_Ptr (C_path'Unchecked_Access),
         argcPtr,
         argvPtr);
   end Tcl_SplitPath;

   procedure Tcl_StaticPackage
     (interp       : in Tcl_Interp;
      pkgName      : in String;
      initProc     : in Tcl_PackageInitProc;
      safeInitProc : in Tcl_PackageInitProc)
   is
      C_pkgName : aliased C.char_array := C.To_C (pkgName);
   begin --  Tcl_StaticPackage
      Tcl.Tcl_StaticPackage
        (interp,
         C.Strings.To_Chars_Ptr (C_pkgName'Unchecked_Access),
         initProc,
         safeInitProc);
   end Tcl_StaticPackage;

   function Tcl_StringMatch
     (str     : in String;
      pattern : in String)
      return    C.int
   is
      C_str     : aliased C.char_array := C.To_C (str);
      C_pattern : aliased C.char_array := C.To_C (pattern);
   begin --  Tcl_StringMatch
      return Tcl.Tcl_StringMatch
               (C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_pattern'Unchecked_Access));
   end Tcl_StringMatch;

   function Tcl_Ungets
     (chan   : in Tcl_Channel;
      str    : in String;
      len    : in C.int;
      atHead : in C.int)
      return   C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_Ungets
      return Tcl.Tcl_Ungets
               (chan,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                len,
                atHead);
   end Tcl_Ungets;

   procedure Tcl_UnlinkVar (interp : in Tcl_Interp; varName : in String) is
      C_varName : aliased C.char_array := C.To_C (varName);
   begin --  Tcl_UnlinkVar
      Tcl.Tcl_UnlinkVar
        (interp,
         C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access));
   end Tcl_UnlinkVar;

   function Tcl_UnsetVar
     (interp  : in Tcl_Interp;
      varName : in String;
      flags   : in C.int := TCL_GLOBAL_ONLY)
      return    C.int
   is
      C_varName : aliased C.char_array := C.To_C (varName);
   begin --  Tcl_UnsetVar
      return Tcl.Tcl_UnsetVar
               (interp,
                C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                flags);
   end Tcl_UnsetVar;

   procedure Tcl_UnsetVar
     (interp  : in Tcl_Interp;
      varName : in String;
      flags   : in C.int := TCL_GLOBAL_ONLY)
   is
      C_varName : aliased C.char_array := C.To_C (varName);
   begin --  Tcl_UnsetVar
      Assert
        (interp,
         Tcl.Tcl_UnsetVar
            (interp,
             C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
             flags));
   end Tcl_UnsetVar;

   function Tcl_UnsetVar2
     (interp : in Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY)
      return   C.int
   is
      C_part1 : aliased C.char_array := C.To_C (part1);
      C_part2 : aliased C.char_array := C.To_C (part2);
   begin --  Tcl_UnsetVar2
      return Tcl.Tcl_UnsetVar2
               (interp,
                C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                flags);
   end Tcl_UnsetVar2;

   procedure Tcl_UnsetVar2
     (interp : in Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY)
   is
      C_part1 : aliased C.char_array := C.To_C (part1);
      C_part2 : aliased C.char_array := C.To_C (part2);
   begin --  Tcl_UnsetVar2
      Assert
        (interp,
         Tcl.Tcl_UnsetVar2
            (interp,
             C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
             flags));
   end Tcl_UnsetVar2;

   procedure Tcl_UpdateLinkedVar
     (interp  : in Tcl_Interp;
      varName : in String)
   is
      C_varName : aliased C.char_array := C.To_C (varName);
   begin --  Tcl_UpdateLinkedVar
      Tcl.Tcl_UpdateLinkedVar
        (interp,
         C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access));
   end Tcl_UpdateLinkedVar;

   function Tcl_UpVar
     (interp    : in Tcl_Interp;
      frameName : in String;
      varName   : in String;
      localName : in String;
      flags     : in C.int)
      return      C.int
   is
      C_frameName : aliased C.char_array := C.To_C (frameName);
      C_varName   : aliased C.char_array := C.To_C (varName);
      C_localName : aliased C.char_array := C.To_C (localName);
   begin --  Tcl_UpVar
      return Tcl.Tcl_UpVar
               (interp,
                C.Strings.To_Chars_Ptr (C_frameName'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_localName'Unchecked_Access),
                flags);
   end Tcl_UpVar;

   procedure Tcl_UpVar
     (interp    : in Tcl_Interp;
      frameName : in String;
      varName   : in String;
      localName : in String;
      flags     : in C.int)
   is
      C_frameName : aliased C.char_array := C.To_C (frameName);
      C_varName   : aliased C.char_array := C.To_C (varName);
      C_localName : aliased C.char_array := C.To_C (localName);
   begin --  Tcl_UpVar
      Assert
        (interp,
         Tcl.Tcl_UpVar
            (interp,
             C.Strings.To_Chars_Ptr (C_frameName'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_varName'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_localName'Unchecked_Access),
             flags));
   end Tcl_UpVar;

   function Tcl_UpVar2
     (interp    : in Tcl_Interp;
      frameName : in String;
      part1     : in String;
      part2     : in String;
      localName : in String;
      flags     : in C.int)
      return      C.int
   is
      C_frameName : aliased C.char_array := C.To_C (frameName);
      C_part1     : aliased C.char_array := C.To_C (part1);
      C_part2     : aliased C.char_array := C.To_C (part2);
      C_localName : aliased C.char_array := C.To_C (localName);
   begin --  Tcl_UpVar2
      return Tcl.Tcl_UpVar2
               (interp,
                C.Strings.To_Chars_Ptr (C_frameName'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_localName'Unchecked_Access),
                flags);
   end Tcl_UpVar2;

   procedure Tcl_UpVar2
     (interp    : in Tcl_Interp;
      frameName : in String;
      part1     : in String;
      part2     : in String;
      localName : in String;
      flags     : in C.int)
   is
      C_frameName : aliased C.char_array := C.To_C (frameName);
      C_part1     : aliased C.char_array := C.To_C (part1);
      C_part2     : aliased C.char_array := C.To_C (part2);
      C_localName : aliased C.char_array := C.To_C (localName);
   begin --  Tcl_UpVar2
      Assert
        (interp,
         Tcl.Tcl_UpVar2
            (interp,
             C.Strings.To_Chars_Ptr (C_frameName'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_localName'Unchecked_Access),
             flags));
   end Tcl_UpVar2;

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
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "")
      return    C.int
   is
      C_String1 : aliased C.char_array := C.To_C (String1);
      C_String2 : aliased C.char_array := C.To_C (String2);
      C_String3 : aliased C.char_array := C.To_C (String3);
      C_String4 : aliased C.char_array := C.To_C (String4);
      C_String5 : aliased C.char_array := C.To_C (String5);
      C_String6 : aliased C.char_array := C.To_C (String6);
      C_String7 : aliased C.char_array := C.To_C (String7);
      C_String8 : aliased C.char_array := C.To_C (String8);
      C_String9 : aliased C.char_array := C.To_C (String9);
   begin --  Tcl_VarEval
      return VarEval
               (interp,
                C.Strings.To_Chars_Ptr (C_String1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String2'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String3'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String4'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String5'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String6'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String7'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String8'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_String9'Unchecked_Access),
                C.Strings.Null_Ptr);
   end Tcl_VarEval;

   procedure Tcl_VarEval
     (interp  : in Tcl_Interp;
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "")
   is
      C_String1 : aliased C.char_array := C.To_C (String1);
      C_String2 : aliased C.char_array := C.To_C (String2);
      C_String3 : aliased C.char_array := C.To_C (String3);
      C_String4 : aliased C.char_array := C.To_C (String4);
      C_String5 : aliased C.char_array := C.To_C (String5);
      C_String6 : aliased C.char_array := C.To_C (String6);
      C_String7 : aliased C.char_array := C.To_C (String7);
      C_String8 : aliased C.char_array := C.To_C (String8);
      C_String9 : aliased C.char_array := C.To_C (String9);
   begin --  Tcl_VarEval
      Assert
        (interp,
         VarEval
            (interp,
             C.Strings.To_Chars_Ptr (C_String1'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String2'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String3'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String4'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String5'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String6'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String7'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String8'Unchecked_Access),
             C.Strings.To_Chars_Ptr (C_String9'Unchecked_Access),
             C.Strings.Null_Ptr));
   end Tcl_VarEval;

   procedure Tcl_WrongNumArgs
     (interp  : in Tcl_Interp;
      objc    : in C.int;
      objv    : in Tcl_Obj_Array;
      message : in String)
   is
      C_message : aliased C.char_array := C.To_C (message);
   begin --  Tcl_WrongNumArgs
      Tcl.Tcl_WrongNumArgs
        (interp,
         objc,
         objv,
         C.Strings.To_Chars_Ptr (C_message'Unchecked_Access));
   end Tcl_WrongNumArgs;

   function Tcl_DumpActiveMemory (fileName : in String) return C.int is
      C_fileName : aliased C.char_array := C.To_C (fileName);
   begin --  Tcl_DumpActiveMemory
      return Tcl.Tcl_DumpActiveMemory
               (C.Strings.To_Chars_Ptr (C_fileName'Unchecked_Access));
   end Tcl_DumpActiveMemory;

   procedure Tcl_ValidateAllMemory (file : in String; line : in C.int) is
      C_file : aliased C.char_array := C.To_C (file);
   begin --  Tcl_ValidateAllMemory
      Tcl.Tcl_ValidateAllMemory
        (C.Strings.To_Chars_Ptr (C_file'Unchecked_Access),
         line);
   end Tcl_ValidateAllMemory;

   function Tcl_ParseVar
     (interp  : in Tcl_Interp;
      str     : in String;
      termPtr : in CArgv.Chars_Ptr_Ptr)
      return    String
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tcl_ParseVar
      return CHelper.Value
               (Tcl.Tcl_ParseVar
                   (interp,
                    C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                    termPtr));
   end Tcl_ParseVar;

   function Tcl_EvalEx
     (interp   : in Tcl_Interp;
      script   : in String;
      numBytes : in C.int;
      flags    : in C.int)
      return     C.int
   is
      C_script : aliased C.char_array := C.To_C (script);
   begin --  Tcl_EvalEx
      return Tcl.Tcl_EvalEx
               (interp,
                C.Strings.To_Chars_Ptr (C_script'Unchecked_Access),
                numBytes,
                flags);
   end Tcl_EvalEx;

   procedure Tcl_EvalEx
     (interp   : in Tcl_Interp;
      script   : in String;
      numBytes : in C.int;
      flags    : in C.int)
   is
      C_script : aliased C.char_array := C.To_C (script);
   begin --  Tcl_EvalEx
      Assert
        (interp,
         Tcl.Tcl_EvalEx
            (interp,
             C.Strings.To_Chars_Ptr (C_script'Unchecked_Access),
             numBytes,
             flags));
   end Tcl_EvalEx;

   function Tcl_ExternalToUtf
     (interp      : in Tcl_Interp;
      encoding    : in Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : in Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
      return        C.int
   is
      C_src : aliased C.char_array := C.To_C (src);
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_ExternalToUtf
      return Tcl.Tcl_ExternalToUtf
               (interp,
                encoding,
                C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                srcLen,
                flags,
                statePtr,
                C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access),
                dstLen,
                srcReadPtr,
                dstWrotePtr,
                dstCharsPtr);
   end Tcl_ExternalToUtf;

   procedure Tcl_ExternalToUtf
     (interp      : in Tcl_Interp;
      encoding    : in Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : in Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
   is
      C_src : aliased C.char_array := C.To_C (src);
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_ExternalToUtf
      Assert
        (interp,
         Tcl.Tcl_ExternalToUtf
            (interp,
             encoding,
             C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
             srcLen,
             flags,
             statePtr,
             C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access),
             dstLen,
             srcReadPtr,
             dstWrotePtr,
             dstCharsPtr));
   end Tcl_ExternalToUtf;

   function Tcl_ExternalToUtfDString
     (encoding : in Tcl_Encoding;
      src      : in String;
      srcLen   : in C.int;
      dsPtr    : in Tcl_DString)
      return     String
   is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_ExternalToUtfDString
      return CHelper.Value
               (Tcl.Tcl_ExternalToUtfDString
                   (encoding,
                    C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                    srcLen,
                    dsPtr));
   end Tcl_ExternalToUtfDString;

   function Tcl_GetEncoding
     (interp : in Tcl_Interp;
      name   : in String)
      return   Tcl_Encoding
   is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tcl_GetEncoding
      return Tcl.Tcl_GetEncoding
               (interp,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access));
   end Tcl_GetEncoding;

   function Tcl_GetEncodingName (encoding : in Tcl_Encoding) return String is
   begin --  Tcl_GetEncodingName
      return CHelper.Value (Tcl.Tcl_GetEncodingName (encoding));
   end Tcl_GetEncodingName;

   function Tcl_GetIndexFromObjStruct
     (interp   : in Tcl_Interp;
      objPtr   : in Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      offset   : in C.int;
      msg      : in String;
      flags    : in C.int;
      indexPtr : access C.int)
      return     C.int
   is
      C_msg : aliased C.char_array := C.To_C (msg);
   begin --  Tcl_GetIndexFromObjStruct
      return Tcl.Tcl_GetIndexFromObjStruct
               (interp,
                objPtr,
                tablePtr,
                offset,
                C.Strings.To_Chars_Ptr (C_msg'Unchecked_Access),
                flags,
                indexPtr);
   end Tcl_GetIndexFromObjStruct;

   procedure Tcl_GetIndexFromObjStruct
     (interp   : in Tcl_Interp;
      objPtr   : in Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      offset   : in C.int;
      msg      : in String;
      flags    : in C.int;
      indexPtr : access C.int)
   is
      C_msg : aliased C.char_array := C.To_C (msg);
   begin --  Tcl_GetIndexFromObjStruct
      Assert
        (interp,
         Tcl.Tcl_GetIndexFromObjStruct
            (interp,
             objPtr,
             tablePtr,
             offset,
             C.Strings.To_Chars_Ptr (C_msg'Unchecked_Access),
             flags,
             indexPtr));
   end Tcl_GetIndexFromObjStruct;

   function Tcl_GetVar2Ex
     (interp : in Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY)
      return   Tcl_Obj
   is
      C_part1 : aliased C.char_array := C.To_C (part1);
      C_part2 : aliased C.char_array := C.To_C (part2);
   begin --  Tcl_GetVar2Ex
      return Tcl.Tcl_GetVar2Ex
               (interp,
                C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                flags);
   end Tcl_GetVar2Ex;

   function Tcl_NumUtfChars (src : in String; len : in C.int) return C.int is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_NumUtfChars
      return Tcl.Tcl_NumUtfChars
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                len);
   end Tcl_NumUtfChars;

   function Tcl_SetSystemEncoding
     (interp : in Tcl_Interp;
      name   : in String)
      return   C.int
   is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tcl_SetSystemEncoding
      return Tcl.Tcl_SetSystemEncoding
               (interp,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access));
   end Tcl_SetSystemEncoding;

   procedure Tcl_SetSystemEncoding
     (interp : in Tcl_Interp;
      name   : in String)
   is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tcl_SetSystemEncoding
      Assert
        (interp,
         Tcl.Tcl_SetSystemEncoding
            (interp,
             C.Strings.To_Chars_Ptr (C_name'Unchecked_Access)));
   end Tcl_SetSystemEncoding;

   function Tcl_SetVar2Ex
     (interp      : in Tcl_Interp;
      part1       : in String;
      part2       : in String;
      newValuePtr : in Tcl_Obj;
      flags       : in C.int := TCL_GLOBAL_ONLY)
      return        Tcl_Obj
   is
      C_part1 : aliased C.char_array := C.To_C (part1);
      C_part2 : aliased C.char_array := C.To_C (part2);
   begin --  Tcl_SetVar2Ex
      return Tcl.Tcl_SetVar2Ex
               (interp,
                C.Strings.To_Chars_Ptr (C_part1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_part2'Unchecked_Access),
                newValuePtr,
                flags);
   end Tcl_SetVar2Ex;

   function Tcl_UniCharAtIndex
     (src   : in String;
      index : in C.int)
      return  Tcl_UniChar
   is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UniCharAtIndex
      return Tcl.Tcl_UniCharAtIndex
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                index);
   end Tcl_UniCharAtIndex;

   function Tcl_UniCharToUtf (ch : in C.int; buf : in String) return C.int is
      C_buf : aliased C.char_array := C.To_C (buf);
   begin --  Tcl_UniCharToUtf
      return Tcl.Tcl_UniCharToUtf
               (ch,
                C.Strings.To_Chars_Ptr (C_buf'Unchecked_Access));
   end Tcl_UniCharToUtf;

   function Tcl_UtfAtIndex
     (src   : in String;
      index : in C.int)
      return  String
   is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfAtIndex
      return CHelper.Value
               (Tcl.Tcl_UtfAtIndex
                   (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                    index));
   end Tcl_UtfAtIndex;

   function Tcl_UtfCharComplete
     (src  : in String;
      len  : in C.int)
      return C.int
   is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfCharComplete
      return Tcl.Tcl_UtfCharComplete
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                len);
   end Tcl_UtfCharComplete;

   function Tcl_UtfBackslash
     (src     : in String;
      readPtr : access C.int;
      dst     : in String)
      return    C.int
   is
      C_src : aliased C.char_array := C.To_C (src);
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_UtfBackslash
      return Tcl.Tcl_UtfBackslash
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                readPtr,
                C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access));
   end Tcl_UtfBackslash;

   function Tcl_UtfFindFirst (src : in String; ch : in C.int) return String is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfFindFirst
      return CHelper.Value
               (Tcl.Tcl_UtfFindFirst
                   (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                    ch));
   end Tcl_UtfFindFirst;

   function Tcl_UtfFindLast (src : in String; ch : in C.int) return String is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfFindLast
      return CHelper.Value
               (Tcl.Tcl_UtfFindLast
                   (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                    ch));
   end Tcl_UtfFindLast;

   function Tcl_UtfNext (src : in String) return String is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfNext
      return CHelper.Value
               (Tcl.Tcl_UtfNext
                   (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access)));
   end Tcl_UtfNext;

   function Tcl_UtfPrev (src : in String; start : in String) return String is
      C_src   : aliased C.char_array := C.To_C (src);
      C_start : aliased C.char_array := C.To_C (start);
   begin --  Tcl_UtfPrev
      return CHelper.Value
               (Tcl.Tcl_UtfPrev
                   (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                    C.Strings.To_Chars_Ptr (C_start'Unchecked_Access)));
   end Tcl_UtfPrev;

   function Tcl_UtfToExternal
     (interp      : in Tcl_Interp;
      encoding    : in Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : in Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
      return        C.int
   is
      C_src : aliased C.char_array := C.To_C (src);
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_UtfToExternal
      return Tcl.Tcl_UtfToExternal
               (interp,
                encoding,
                C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                srcLen,
                flags,
                statePtr,
                C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access),
                dstLen,
                srcReadPtr,
                dstWrotePtr,
                dstCharsPtr);
   end Tcl_UtfToExternal;

   procedure Tcl_UtfToExternal
     (interp      : in Tcl_Interp;
      encoding    : in Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : in Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
   is
      C_src : aliased C.char_array := C.To_C (src);
      C_dst : aliased C.char_array := C.To_C (dst);
   begin --  Tcl_UtfToExternal
      Assert
        (interp,
         Tcl.Tcl_UtfToExternal
            (interp,
             encoding,
             C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
             srcLen,
             flags,
             statePtr,
             C.Strings.To_Chars_Ptr (C_dst'Unchecked_Access),
             dstLen,
             srcReadPtr,
             dstWrotePtr,
             dstCharsPtr));
   end Tcl_UtfToExternal;

   function Tcl_UtfToExternalDString
     (encoding : in Tcl_Encoding;
      src      : in String;
      srcLen   : in C.int;
      dsPtr    : in Tcl_DString)
      return     String
   is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfToExternalDString
      return CHelper.Value
               (Tcl.Tcl_UtfToExternalDString
                   (encoding,
                    C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                    srcLen,
                    dsPtr));
   end Tcl_UtfToExternalDString;

   function Tcl_UtfToLower (src : in String) return C.int is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfToLower
      return Tcl.Tcl_UtfToLower
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access));
   end Tcl_UtfToLower;

   function Tcl_UtfToTitle (src : in String) return C.int is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfToTitle
      return Tcl.Tcl_UtfToTitle
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access));
   end Tcl_UtfToTitle;

   function Tcl_UtfToUniChar
     (src   : in String;
      chPtr : in Tcl_UniChar)
      return  C.int
   is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfToUniChar
      return Tcl.Tcl_UtfToUniChar
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access),
                chPtr);
   end Tcl_UtfToUniChar;

   function Tcl_UtfToUpper (src : in String) return C.int is
      C_src : aliased C.char_array := C.To_C (src);
   begin --  Tcl_UtfToUpper
      return Tcl.Tcl_UtfToUpper
               (C.Strings.To_Chars_Ptr (C_src'Unchecked_Access));
   end Tcl_UtfToUpper;

   function Tcl_GetString (objPtr : in Tcl_Obj) return String is
   begin --  Tcl_GetString
      return CHelper.Value (Tcl.Tcl_GetString (objPtr));
   end Tcl_GetString;

   function Tcl_GetDefaultEncodingDir return C.Strings.chars_ptr is
   begin --  Tcl_GetDefaultEncodingDir
      return Tcl.Tcl_GetDefaultEncodingDir;
   end Tcl_GetDefaultEncodingDir;

   procedure Tcl_SetDefaultEncodingDir (path : in String) is
      C_path : aliased C.char_array := C.To_C (path);
   begin --  Tcl_SetDefaultEncodingDir
      Tcl.Tcl_SetDefaultEncodingDir
        (C.Strings.To_Chars_Ptr (C_path'Unchecked_Access));
   end Tcl_SetDefaultEncodingDir;

   function Tcl_UniCharToUtfDString
     (strng    : in Tcl_UniChar;
      numChars : in C.int;
      dsPtr    : in Tcl_DString)
      return     String
   is
   begin --  Tcl_UniCharToUtfDString
      return CHelper.Value
               (Tcl.Tcl_UniCharToUtfDString (strng, numChars, dsPtr));
   end Tcl_UniCharToUtfDString;

   function Tcl_UtfToUniCharDString
     (strng  : in String;
      length : in C.int;
      dsPtr  : in Tcl_DString)
      return   Tcl_UniChar
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_UtfToUniCharDString
      return Tcl.Tcl_UtfToUniCharDString
               (C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
                length,
                dsPtr);
   end Tcl_UtfToUniCharDString;

   procedure Tcl_LogCommandInfo
     (interp  : in Tcl_Interp;
      script  : in String;
      command : in String;
      length  : in C.int)
   is
      C_script  : aliased C.char_array := C.To_C (script);
      C_command : aliased C.char_array := C.To_C (command);
   begin --  Tcl_LogCommandInfo
      Tcl.Tcl_LogCommandInfo
        (interp,
         C.Strings.To_Chars_Ptr (C_script'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_command'Unchecked_Access),
         length);
   end Tcl_LogCommandInfo;

   function Tcl_ParseBraces
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
      return     C.int
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseBraces
      return Tcl.Tcl_ParseBraces
               (interp,
                C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
                numBytes,
                parsePtr,
                append,
                termPtr);
   end Tcl_ParseBraces;

   procedure Tcl_ParseBraces
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseBraces
      Assert
        (interp,
         Tcl.Tcl_ParseBraces
            (interp,
             C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
             numBytes,
             parsePtr,
             append,
             termPtr));
   end Tcl_ParseBraces;

   function Tcl_ParseCommand
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      nested   : in C.int;
      parsePtr : in Tcl_Parse)
      return     C.int
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseCommand
      return Tcl.Tcl_ParseCommand
               (interp,
                C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
                numBytes,
                nested,
                parsePtr);
   end Tcl_ParseCommand;

   procedure Tcl_ParseCommand
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      nested   : in C.int;
      parsePtr : in Tcl_Parse)
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseCommand
      Assert
        (interp,
         Tcl.Tcl_ParseCommand
            (interp,
             C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
             numBytes,
             nested,
             parsePtr));
   end Tcl_ParseCommand;

   function Tcl_ParseExpr
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse)
      return     C.int
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseExpr
      return Tcl.Tcl_ParseExpr
               (interp,
                C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
                numBytes,
                parsePtr);
   end Tcl_ParseExpr;

   procedure Tcl_ParseExpr
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse)
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseExpr
      Assert
        (interp,
         Tcl.Tcl_ParseExpr
            (interp,
             C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
             numBytes,
             parsePtr));
   end Tcl_ParseExpr;

   function Tcl_ParseQuotedString
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
      return     C.int
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseQuotedString
      return Tcl.Tcl_ParseQuotedString
               (interp,
                C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
                numBytes,
                parsePtr,
                append,
                termPtr);
   end Tcl_ParseQuotedString;

   procedure Tcl_ParseQuotedString
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseQuotedString
      Assert
        (interp,
         Tcl.Tcl_ParseQuotedString
            (interp,
             C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
             numBytes,
             parsePtr,
             append,
             termPtr));
   end Tcl_ParseQuotedString;

   function Tcl_ParseVarName
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int)
      return     C.int
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseVarName
      return Tcl.Tcl_ParseVarName
               (interp,
                C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
                numBytes,
                parsePtr,
                append);
   end Tcl_ParseVarName;

   procedure Tcl_ParseVarName
     (interp   : in Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int)
   is
      C_strng : aliased C.char_array := C.To_C (strng);
   begin --  Tcl_ParseVarName
      Assert
        (interp,
         Tcl.Tcl_ParseVarName
            (interp,
             C.Strings.To_Chars_Ptr (C_strng'Unchecked_Access),
             numBytes,
             parsePtr,
             append));
   end Tcl_ParseVarName;

   function Tcl_Chdir (dirName : in String) return C.int is
      C_dirName : aliased C.char_array := C.To_C (dirName);
   begin --  Tcl_Chdir
      return Tcl.Tcl_Chdir
               (C.Strings.To_Chars_Ptr (C_dirName'Unchecked_Access));
   end Tcl_Chdir;

   function Tcl_Access (path : in String; mode : in C.int) return C.int is
      C_path : aliased C.char_array := C.To_C (path);
   begin --  Tcl_Access
      return Tcl.Tcl_Access
               (C.Strings.To_Chars_Ptr (C_path'Unchecked_Access),
                mode);
   end Tcl_Access;

   function Tcl_Stat (path : in String) return C.int is
      C_path : aliased C.char_array := C.To_C (path);
   begin --  Tcl_Stat
      return Tcl.Tcl_Stat (C.Strings.To_Chars_Ptr (C_path'Unchecked_Access));
   end Tcl_Stat;

   function Tcl_UtfNcmp
     (s1   : in String;
      s2   : in String;
      n    : in C.unsigned_long)
      return C.int
   is
      C_s1 : aliased C.char_array := C.To_C (s1);
      C_s2 : aliased C.char_array := C.To_C (s2);
   begin --  Tcl_UtfNcmp
      return Tcl.Tcl_UtfNcmp
               (C.Strings.To_Chars_Ptr (C_s1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_s2'Unchecked_Access),
                n);
   end Tcl_UtfNcmp;

   function Tcl_UtfNcasecmp
     (s1   : in String;
      s2   : in String;
      n    : in C.unsigned_long)
      return C.int
   is
      C_s1 : aliased C.char_array := C.To_C (s1);
      C_s2 : aliased C.char_array := C.To_C (s2);
   begin --  Tcl_UtfNcasecmp
      return Tcl.Tcl_UtfNcasecmp
               (C.Strings.To_Chars_Ptr (C_s1'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_s2'Unchecked_Access),
                n);
   end Tcl_UtfNcasecmp;

   function Tcl_StringCaseMatch
     (str     : in String;
      pattern : in String;
      nocase  : in C.int)
      return    C.int
   is
      C_str     : aliased C.char_array := C.To_C (str);
      C_pattern : aliased C.char_array := C.To_C (pattern);
   begin --  Tcl_StringCaseMatch
      return Tcl.Tcl_StringCaseMatch
               (C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_pattern'Unchecked_Access),
                nocase);
   end Tcl_StringCaseMatch;

end Tcl.Ada;
