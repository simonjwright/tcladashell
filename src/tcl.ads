--------------------------------------------------------------------
--
--  tcl.ads -- This package is the "thin" binding to Tcl.
--
--  Copyright (c) 1995-2000 Terry J. Westley
--  Copyright (c) 2008 O Kellogg
--  Copyright (c) 2006, 2008, 2009, 2011, 2014, 2019
--     Simon Wright <simon@pushface.org>
--
--  Tash is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. Tash is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with Tash; see file COPYING. If not, write to
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
--  This package was originally automatically generated from tcl.h at
--  patchlevel 8.4.7.  Note that some of the comments below, preserved
--  from tcl.h, do not apply to the Ada version.  Someday, these
--  comments may be customized better.
--
--------------------------------------------------------------------

with CArgv;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;
with Tcl_Record_Sizes;

package Tcl is

   package C renames Interfaces.C;

   use type C.int;
   use type C.size_t;

   subtype CNatural is CArgv.CNatural;

   --
   --  Special macro to define mutexes, that doesn't do anything
   --  if we are not using threads.
   --

   --  TCL_DECLARE_MUTEX

   --
   --  Macros that eliminate the overhead of the thread synchronization
   --  functions when compiling without thread support.
   --

   --  TCL_THREADS

   --
   --  Miscellaneous declarations.
   --

   subtype ClientData is System.Address;
   Null_ClientData : constant ClientData := System.Null_Address;

   type Tcl_WideInt  is new Interfaces.Integer_64;
   type Tcl_WideUInt is new Interfaces.Unsigned_64;

   --
   --  Data structures defined opaquely in this module. The
   --  definitions below just provide dummy types. A few fields are
   --  made visible in Tcl_Interp structures, namely those used for
   --  returning a string result from commands. Direct access to the
   --  result field is discouraged in Tcl 8.0.  The interpreter result
   --  is either an object or a string, and the two values are kept
   --  consistent unless some C code sets interp->result
   --  directly. Programmers should use either the procedure
   --  Tcl_GetObjResult {} or Tcl_GetStringResult {} to read the
   --  interpreter's result. See the SetResult man page for details.
   --

   --  Tcl_Interp is defined in tcl.h but as indicated above direct
   --  access is deprecated, so not supported here.

   type Tcl_Interp_Rec (<>) is private;
   type Tcl_Interp is access all Tcl_Interp_Rec;
   pragma Convention (C, Tcl_Interp);

   type Tcl_AsyncHandler_Rec (<>) is private;
   type Tcl_AsyncHandler is access all Tcl_AsyncHandler_Rec;
   pragma Convention (C, Tcl_AsyncHandler);

   type Tcl_Channel_Rec (<>) is private;
   type Tcl_Channel is access all Tcl_Channel_Rec;
   pragma Convention (C, Tcl_Channel);

   type Tcl_Command_Rec (<>) is private;
   type Tcl_Command is access all Tcl_Command_Rec;
   pragma Convention (C, Tcl_Command);

   type Tcl_Condition_Rec (<>) is private;
   type Tcl_Condition is access all Tcl_Condition_Rec;
   pragma Convention (C, Tcl_Condition);

   type Tcl_EncodingState_Rec (<>) is private;
   type Tcl_EncodingState is access all Tcl_EncodingState_Rec;
   pragma Convention (C, Tcl_EncodingState);

   type Tcl_Encoding_Rec (<>) is private;
   type Tcl_Encoding is access all Tcl_Encoding_Rec;
   pragma Convention (C, Tcl_Encoding);

   type Tcl_Event_Rec;
   type Tcl_Event is access all Tcl_Event_Rec;
   pragma Convention (C, Tcl_Event);

   type Tcl_EventProc is access function
     (evPtr : Tcl_Event;
      flags : C.int)
   return     C.int;
   pragma Convention (C, Tcl_EventProc);

   type Tcl_Event_Rec is record
      --  Function to call to service this event.
      proc : Tcl_EventProc;
      --  Next in list of pending events, or NULL.
      nextPtr : Tcl_Event;
   end record;
   pragma Convention (C, Tcl_Event_Rec);

   type Tcl_Mutex_Rec (<>) is private;
   type Tcl_Mutex is access all Tcl_Mutex_Rec;
   pragma Convention (C, Tcl_Mutex);

   type Tcl_Pid_Rec (<>) is private;
   type Tcl_Pid is access all Tcl_Pid_Rec;
   pragma Convention (C, Tcl_Pid);

   type Tcl_RegExp_Rec (<>) is private;
   type Tcl_RegExp is access all Tcl_RegExp_Rec;
   pragma Convention (C, Tcl_RegExp);

   type Tcl_ThreadDataKey_Rec (<>) is private;
   type Tcl_ThreadDataKey is access all Tcl_ThreadDataKey_Rec;
   pragma Convention (C, Tcl_ThreadDataKey);

   type Tcl_ThreadId_Rec (<>) is private;
   type Tcl_ThreadId is access all Tcl_ThreadId_Rec;
   pragma Convention (C, Tcl_ThreadId);

   type Tcl_TimerToken_Rec (<>) is private;
   type Tcl_TimerToken is access all Tcl_TimerToken_Rec;
   pragma Convention (C, Tcl_TimerToken);

   type Tcl_Trace_Rec (<>) is private;
   type Tcl_Trace is access all Tcl_Trace_Rec;
   pragma Convention (C, Tcl_Trace);

   type Tcl_Var_Rec (<>) is private;
   type Tcl_Var is access all Tcl_Var_Rec;
   pragma Convention (C, Tcl_Var);

   type Tcl_ChannelTypeVersion_Rec (<>) is private;
   type Tcl_ChannelTypeVersion is access all Tcl_ChannelTypeVersion_Rec;
   pragma Convention (C, Tcl_ChannelTypeVersion);

   type Tcl_LoadHandle_Rec (<>) is private;
   type Tcl_LoadHandle is access all Tcl_LoadHandle_Rec;
   pragma Convention (C, Tcl_LoadHandle);

   --
   --  Flag values passed to Tcl_GetRegExpFromObj.
   --

   TCL_REG_BASIC : constant := 8#000000#;
   --  BREs {convenience}

   TCL_REG_EXTENDED : constant := 8#000001#;
   --  EREs

   TCL_REG_ADVF : constant := 8#000002#;
   --  advanced features in EREs

   TCL_REG_ADVANCED : constant := 8#000003#;
   --  AREs {which are also EREs}

   TCL_REG_QUOTE : constant := 8#000004#;
   --  no special characters, none

   TCL_REG_NOCASE : constant := 8#000010#;
   --  ignore case

   TCL_REG_NOSUB : constant := 8#000020#;
   --  don't care about subexpressions

   TCL_REG_EXPANDED : constant := 8#000040#;
   --  expanded format, white space &
   --  comments

   TCL_REG_NLSTOP : constant := 8#000100#;
   --  \n doesn't match . or {^ }

   TCL_REG_NLANCH : constant := 8#000200#;
   --  ^ matches after \n, $ before

   TCL_REG_NEWLINE : constant := 8#000300#;
   --  newlines are line terminators

   TCL_REG_CANMATCH : constant := 8#001000#;
   --  report details on partial/limited
   --  matches

   --
   --  The following flag is experimental and only intended for use by
   --  Expect.  It will probably go away in a later release.
   --

   TCL_REG_BOSONLY : constant := 8#002000#;
   --  prepend \A to pattern so it only matches at the beginning of
   --  the string.

   --
   --  Flags values passed to Tcl_RegExpExecObj.
   --

   TCL_REG_NOTBOL : constant := 1;
   --  Beginning of string does not match ^.

   TCL_REG_NOTEOL : constant := 2;
   --  End of string does not match $.

   --
   --  Structures filled in by Tcl_RegExpInfo.  Note that all offset values are
   --  relative to the start of the match string, not the beginning of the
   --  entire string.
   --

   type Tcl_RegExpIndices_Rec is record
      Start : C.long;  --  character offset of first character in match
      E_n_d : C.long;  --  character offset of first character after the match.
   end record;
   pragma Convention (C, Tcl_RegExpIndices_Rec);

   type Tcl_RegExpIndices_Array is
     array (CNatural range <>) of aliased Tcl_RegExpIndices_Rec;

   package Tcl_RegExpIndices_Pointers is new C.Pointers
     (Index => CNatural,
      Element => Tcl_RegExpIndices_Rec,
      Element_Array => Tcl_RegExpIndices_Array,
      Default_Terminator => (0, 0));

   subtype Tcl_RegExpIndices is Tcl_RegExpIndices_Pointers.Pointer;

   type Tcl_RegExpInfo_Rec is record
      nsubs : C.int;                --  number of subexpressions in the
                                    --  compiled expression
      matches : Tcl_RegExpIndices;  --  array of nsubs match offset
                                    --  pairs
      extendStart : C.long;         --  The offset at which a subsequent
                                    --  match might begin.
      reserved : C.long;            --  Reserved for later use.
   end record;
   pragma Convention (C, Tcl_RegExpInfo_Rec);

   type Tcl_RegExpInfo is access all Tcl_RegExpInfo_Rec;
   pragma Convention (C, Tcl_RegExpInfo);

   --
   --  Picky compilers complain if this typdef doesn't appear before the
   --  struct's reference in tclDecls.h.
   --

   type Tcl_OldStat_Rec (<>) is private;
   type stat is access all Tcl_OldStat_Rec;
   pragma Convention (C, stat);

   type Tcl_StatBuf_Rec (<>) is private;
   type Tcl_StatBuf is access all Tcl_StatBuf_Rec;
   pragma Convention (C, Tcl_StatBuf);

   --
   --  When a TCL command returns, the interpreter contains a result from the
   --  command. Programmers are strongly encouraged to use one of the
   --  procedures Tcl_GetObjResult {} or Tcl_GetStringResult {} to read the
   --  interpreter's result. See the SetResult man page for details. Besides
   --  this result, the command procedure returns an integer code, which is
   --  one of the following:
   --
   --  TCL_OK           Command completed normally; the interpreter's
   --                   result contains the command's result.
   --  TCL_ERROR        The command couldn't be completed successfully;
   --                   the interpreter's result describes what went wrong.
   --  TCL_RETURN       The command requests that the current procedure
   --                   return; the interpreter's result contains the
   --                   procedure's return value.
   --  TCL_BREAK        The command requests that the innermost loop
   --                   be exited; the interpreter's result is meaningless.
   --  TCL_CONTINUE     Go on to the next iteration of the current loop;
   --                   the interpreter's result is meaningless.
   --

   TCL_OK          : constant := 0;
   TCL_ERROR       : constant := 1;
   TCL_RETURN      : constant := 2;
   TCL_BREAK       : constant := 3;
   TCL_CONTINUE    : constant := 4;
   TCL_RESULT_SIZE : constant := 200;

   --
   --  Flags to control what substitutions are performed by Tcl_SubstObj():
   --

   TCL_SUBST_COMMANDS    : constant := 8#001#;
   TCL_SUBST_VARIABLES   : constant := 8#002#;
   TCL_SUBST_BACKSLASHES : constant := 8#004#;
   TCL_SUBST_ALL         : constant := 8#007#;

   --
   --  Argument descriptors for math function callbacks in expressions:
   --

   type Tcl_ValueType is (TCL_INT, TCL_DOUBLE, TCL_EITHER, TCL_WIDE_INT);
   for Tcl_ValueType'Size use 32;

   type Tcl_Value_Rec is record
      typ : Tcl_ValueType;
      --  Indicates intValue or doubleValue is valid, or both; or
      --  wideValue.
      intValue : C.long;
      --  Integer value.
      doubleValue : C.double;
      --  Double-precision floating value.
      wideValue : Tcl_WideInt;
      --  Wide (min. 64-bit) integer value.
   end record;
   pragma Convention (C, Tcl_Value_Rec);
   type Tcl_Value is access all Tcl_Value_Rec;
   pragma Convention (C, Tcl_Value);

   --
   --  Tcl_Obj forward declaration (for use by subprogram types)
   --

   type Tcl_Obj;

   --
   --  Subprogram types defined by Tcl:
   --

   type Tcl_AppInitProc is access function
     (interp : Tcl_Interp)
     return      C.int;
   pragma Convention (C, Tcl_AppInitProc);

   type Tcl_AsyncProc is access function
     (data   : ClientData;
      interp : Tcl_Interp;
      code   : C.int)
     return      C.int;
   pragma Convention (C, Tcl_AsyncProc);

   type Tcl_ChannelProc is access procedure
     (data : ClientData;
      mask : C.int);
   pragma Convention (C, Tcl_ChannelProc);

   type Tcl_CloseProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_CloseProc);

   type Tcl_CmdDeleteProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_CmdDeleteProc);

   type Tcl_CmdProc is access function
     (data   : ClientData;
      interp : Tcl_Interp;
      argc   : C.int;
      argv   : CArgv.Chars_Ptr_Ptr)
   return      C.int;
   pragma Convention (C, Tcl_CmdProc);

   type Tcl_CmdTraceProc is access procedure
     (data          : ClientData;
      interp        : Tcl_Interp;
      level         : C.int;
      command       : C.Strings.chars_ptr;
      proc          : Tcl_CmdProc;
      cmdclientdata : ClientData;
      argc          : C.int;
      argv          : CArgv.Chars_Ptr_Ptr);
   pragma Convention (C, Tcl_CmdTraceProc);

   type Tcl_DupInternalRepProc is access procedure;
   pragma Convention (C, Tcl_DupInternalRepProc);

   type Tcl_EncodingConvertProc is access function
     (data        : ClientData;
      src         : C.Strings.chars_ptr;
      srcLen      : C.int;
      flags       : C.int;
      statePtr    : Tcl_EncodingState;
      dst         : C.Strings.chars_ptr;
      dstLen      : C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
   return           C.int;
   pragma Convention (C, Tcl_EncodingConvertProc);

   type Tcl_EncodingFreeProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_EncodingFreeProc);

   type Tcl_EventCheckProc is access procedure
     (data  : ClientData;
      flags : C.int);
   pragma Convention (C, Tcl_EventCheckProc);

   type Tcl_EventDeleteProc is access function
     (evPtr : Tcl_Event;
      data  : ClientData)
   return     C.int;
   pragma Convention (C, Tcl_EventDeleteProc);

   type Tcl_EventSetupProc is access procedure
     (data  : ClientData;
      flags : C.int);
   pragma Convention (C, Tcl_EventSetupProc);

   type Tcl_ExitProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_ExitProc);

   type Tcl_FileProc is access procedure
     (data : ClientData;
      mask : C.int);
   pragma Convention (C, Tcl_FileProc);

   type Tcl_FileFreeProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_FileFreeProc);

   type Tcl_FreeInternalRepProc is access procedure;
   pragma Convention (C, Tcl_FreeInternalRepProc);

   type Tcl_FreeProc is access procedure
     (blockPtr : C.Strings.chars_ptr);
   pragma Convention (C, Tcl_FreeProc);

   type Tcl_IdleProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_IdleProc);

   type Tcl_InterpDeleteProc is access procedure
     (data   : ClientData;
      interp : Tcl_Interp);
   pragma Convention (C, Tcl_InterpDeleteProc);

   type Tcl_MathProc is access function
     (data      : ClientData;
      interp    : Tcl_Interp;
      args      : Tcl_Value;
      resultPtr : Tcl_Value)
   return         C.int;
   pragma Convention (C, Tcl_MathProc);

   type Tcl_NamespaceDeleteProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_NamespaceDeleteProc);

   type Tcl_ObjCmdProc is access function
     (data   : ClientData;
      interp : Tcl_Interp;
      objc   : C.int)
   return      C.int;
   pragma Convention (C, Tcl_ObjCmdProc);

   type Tcl_PackageInitProc is access function
     (interp : Tcl_Interp)
   return      C.int;
   pragma Convention (C, Tcl_PackageInitProc);

   type Tcl_PanicProc is access procedure
     (format  : C.Strings.chars_ptr;
      String1 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Convention (C, Tcl_PanicProc);

   type Tcl_TcpAcceptProc is access procedure
     (callbackdata : ClientData;
      chan         : Tcl_Channel;
      address      : System.Address;
      port         : C.int);
   pragma Convention (C, Tcl_TcpAcceptProc);

   type Tcl_TimerProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_TimerProc);

   type Tcl_SetFromAnyProc is access function
     (interp : Tcl_Interp)
   return      C.int;
   pragma Convention (C, Tcl_SetFromAnyProc);

   type Tcl_UpdateStringProc is access procedure;
   pragma Convention (C, Tcl_UpdateStringProc);

   type Tcl_VarTraceProc is access function
     (data   : ClientData;
      interp : Tcl_Interp;
      part1  : C.Strings.chars_ptr;
      part2  : C.Strings.chars_ptr;
      flags  : C.int)
   return      C.Strings.chars_ptr;
   pragma Convention (C, Tcl_VarTraceProc);

   --  @todo Tcl_CommandTraceProc

   type Tcl_CreateFileHandlerProc is access procedure
     (fd   : C.int;
      mask : C.int;
      proc : Tcl_FileProc;
      data : ClientData);
   pragma Convention (C, Tcl_CreateFileHandlerProc);

   type Tcl_DeleteFileHandlerProc is access procedure (fd : C.int);
   pragma Convention (C, Tcl_DeleteFileHandlerProc);

   type Tcl_AlertNotifierProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_AlertNotifierProc);

   type Tcl_ServiceModeHookProc is access procedure (mode : C.int);
   pragma Convention (C, Tcl_ServiceModeHookProc);

   type Tcl_InitNotifierProc is access function return ClientData;
   pragma Convention (C, Tcl_InitNotifierProc);

   type Tcl_FinalizeNotifierProc is access procedure (data : ClientData);
   pragma Convention (C, Tcl_FinalizeNotifierProc);

   --
   --  Tcl_Obj declarations.
   --

   --
   --  The following structure represents a type of object, which is a
   --  particular internal representation for an object plus a set of
   --  procedures that provide standard operations on objects of that type.
   --

   type Tcl_ObjType_Rec is record
      name : C.Strings.chars_ptr;
      --  Name of the type, e.g. "int".
      freeIntRepProc : Tcl_FreeInternalRepProc;
      --  Called to free any storage for the type's
      --  internal rep. NULL if the internal rep
      --  does not need freeing.
      dupIntRepProc : Tcl_DupInternalRepProc;
      --  Called to create a new object as a copy
      --  of an existing object.
      updateStringProc : Tcl_UpdateStringProc;
      --  Called to update the string rep from the
      --  type's internal representation.
      setFromAnyProc : Tcl_SetFromAnyProc;
      --  Called to convert the object's internal
      --  rep to this type. Frees the internal rep
      --  of the old type. Returns TCL_ERROR on
      --  failure.
   end record;
   pragma Convention (C, Tcl_ObjType_Rec);
   type Tcl_ObjType is access all Tcl_ObjType_Rec;
   pragma Convention (C, Tcl_ObjType);

   --
   --  One of the following structures exists for each object in the Tcl
   --  system. An object stores a value as either a string, some internal
   --  representation, or both.
   --

   type Tcl_Obj_Rec is record
      refCount : C.int;
      bytes : C.Strings.chars_ptr;
      length : C.int;
      typePtr : Tcl_ObjType;
      internalRep : C.double;  -- @todo make precise rep for C union
   end record;
   pragma Convention (C, Tcl_Obj_Rec);
   type Tcl_Obj is access all Tcl_Obj_Rec;
   pragma Convention (C, Tcl_Obj);

   type Tcl_Obj_Array is array (CNatural range <>) of aliased Tcl_Obj;
   pragma Convention (C, Tcl_Obj_Array);

   --
   --  Subprograms to increment and decrement a Tcl_Obj's reference count,
   --  and to test whether an object is shared {i.e. has reference
   --  count > 1}.
   --
   --  These subprograms are implemented in TASH's C bindings.
   --

   procedure Tcl_IncrRefCount (objPtr : not null Tcl_Obj);

   procedure Tcl_DecrRefCount (objPtr : not null Tcl_Obj);

   function Tcl_IsShared (objPtr : not null Tcl_Obj) return C.int;

   --
   --  The following structure contains the state needed by
   --  Tcl_SaveResult.  No-one outside of Tcl should access any of these
   --  fields.  This structure is typically allocated on the stack.
   --

   type Tcl_SavedResult_Rec is private;
   type Tcl_SavedResult is access all Tcl_SavedResult_Rec;
   pragma Convention (C, Tcl_SavedResult);

   --
   --  The following definitions support Tcl's namespace facility.
   --  Note: the first five fields must match exactly the fields in a
   --  Namespace structure {see tclInt.h}.
   --

   type Tcl_Namespace_Rec;
   type Tcl_Namespace is access all Tcl_Namespace_Rec;
   pragma Convention (C, Tcl_Namespace);

   type Tcl_Namespace_Rec is record
      name : C.Strings.chars_ptr;
      --  The namespace's name within its parent namespace. This
      --  contains no ::'s. The name of the global namespace is ""
      --  although "::" is an synonym.
      fullName : C.Strings.chars_ptr;
      --  The namespace's fully qualified name.  This starts with ::.
      data : ClientData;
      --  Arbitrary value associated with this namespace.
      deleteProc : Tcl_NamespaceDeleteProc;
      --  Procedure invoked when deleting the namespace to, e.g., free
      --  clientData.
      parentPtr : Tcl_Namespace;
      --  Points to the namespace that contains this one. NULL if this
      --  is the global namespace.
   end record;
   pragma Convention (C, Tcl_Namespace_Rec);

   --
   --  The following structure represents a call frame, or activation
   --  record.  A call frame defines a naming context for a procedure
   --  call: its local scope {for local variables} and its namespace
   --  scope {used for non-local variables; often the global ::
   --  namespace}. A call frame can also define the naming context for
   --  a namespace eval or namespace inscope command: the namespace in
   --  which the command's code should execute. The Tcl_CallFrame
   --  structures exist only while procedures or namespace
   --  eval/inscope's are being executed, and provide a Tcl call
   --  stack.
   --
   --  A call frame is initialized and pushed using Tcl_PushCallFrame
   --  and popped using Tcl_PopCallFrame. Storage for a Tcl_CallFrame
   --  must be provided by the Tcl_PushCallFrame caller, and callers
   --  typically allocate them on the C call stack for efficiency. For
   --  this reason, Tcl_CallFrame is defined as a structure and not as
   --  an opaque token. However, most Tcl_CallFrame fields are hidden
   --  since applications should not access them directly; others are
   --  declared as "dummyX".
   --
   --  WARNING!! The structure definition must be kept consistent with
   --  the CallFrame structure in tclInt.h. If you change one, change
   --  the other.
   --

   type Tcl_CallFrame_Rec is private;
   type Tcl_CallFrame is access all Tcl_CallFrame_Rec;
   pragma Convention (C, Tcl_CallFrame);

   --
   --  Information about commands that is returned by Tcl_GetCommandInfo and
   --  passed to Tcl_SetCommandInfo. objProc is an objc/objv object-based
   --  command procedure while proc is a traditional Tcl argc/argv
   --  string-based procedure. Tcl_CreateObjCommand and Tcl_CreateCommand
   --  ensure that both objProc and proc are non-NULL and can be called to
   --  execute the command. However, it may be faster to call one instead of
   --  the other. The member isNativeObjectProc is set to 1 if an
   --  object-based procedure was registered by Tcl_CreateObjCommand, and to
   --  0 if a string-based procedure was registered by Tcl_CreateCommand.
   --  The other procedure is typically set to a compatibility wrapper that
   --  does string-to-object or object-to-string argument conversions then
   --  calls the other procedure.
   --

   type Tcl_CmdInfo_Rec is record
      isNativeObjectProc : C.int;
      --  1 if objProc was registered by a call to
      --  Tcl_CreateObjCommand; 0 otherwise.
      --  Tcl_SetCmdInfo does not modify this
      --  field.
      objProc : Tcl_ObjCmdProc;
      --  Command's object-based procedure.
      objclientdata : ClientData;
      --  ClientData for object proc.
      proc : Tcl_CmdProc;
      --  Command's string-based procedure.
      data : ClientData;
      --  ClientData for string proc.
      deleteProc : Tcl_CmdDeleteProc;
      --  Procedure to call when command is
      --  deleted.
      deletedata : ClientData;
      --  Value to pass to deleteProc {usually
      --  the same as clientData}.
      namespacePtr : Tcl_Namespace;
      --  Points to the namespace that contains
      --  this command. Note that Tcl_SetCmdInfo
      --  will not change a command's namespace;
      --  use Tcl_RenameCommand to do that.
   end record;
   pragma Convention (C, Tcl_CmdInfo_Rec);
   type Tcl_CmdInfo is access all Tcl_CmdInfo_Rec;
   pragma Convention (C, Tcl_CmdInfo);

   --
   --  The structure defined below is used to hold dynamic strings.
   --  The only field that clients should use is the string field, and
   --  they should never modify it.
   --
   --  For Tash -- use Tcl_DStringValue

   type Tcl_DString_Rec is private;
   type Tcl_DString is access all Tcl_DString_Rec;
   pragma Convention (C, Tcl_DString);

   function Tcl_DStringLength (dsPtr : not null Tcl_DString) return C.int;
   pragma Import (C, Tcl_DStringLength, "Tcl_CallDStringLength");

   function Tcl_DStringValue
     (dsPtr : not null Tcl_DString)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_DStringValue, "Tcl_CallDStringValue");

   procedure Tcl_DStringSetLength
     (dsPtr  : not null Tcl_DString;
      length : C.int);
   pragma Import (C, Tcl_DStringSetLength, "Tcl_DStringSetLength");

   procedure Tcl_DStringTrunc (dsPtr : not null Tcl_DString; length : C.int)
     renames Tcl_DStringSetLength;

   --
   --  Definitions for the maximum number of digits of precision that may
   --  be specified in the "tcl_precision" variable, and the number of
   --  bytes of buffer space required by Tcl_PrintDouble.
   --
   TCL_MAX_PREC     : constant := 17;
   TCL_DOUBLE_SPACE : constant := TCL_MAX_PREC + 10;

   --
   --  Definition for a number of bytes of buffer space sufficient to
   --  hold the string representation of an integer in base 10
   --  {assuming the existence of 64-bit integers}.
   --
   TCL_INTEGER_SPACE : constant := 24;

   --
   --  Flag that may be passed to Tcl_ConvertElement to force it not to
   --  output braces {careful!  if you change this flag be sure to change
   --  the definitions at the front of tclUtil.c}.
   --
   TCL_DONT_USE_BRACES : constant := 1;

   --
   --  Flag that may be passed to Tcl_GetIndexFromObj to force it to disallow
   --  abbreviated strings.
   --
   TCL_EXACT : constant := 1;

   --
   --  Flag values passed to Tcl_RecordAndEval and/or Tcl_EvalObj.
   --  WARNING: these bit choices must not conflict with the bit choices
   --  for evalFlag bits in tclInt.h!!
   --
   TCL_NO_EVAL     : constant := 16#10000#;
   TCL_EVAL_GLOBAL : constant := 16#20000#;
   TCL_EVAL_DIRECT : constant := 16#40000#;
   TCL_EVAL_INVOKE : constant := 16#80000#;

   --
   --  Special freeProc values that may be passed to Tcl_SetResult {see
   --  the man page for details}:
   --
   TCL_VOLATILE : constant := 1;
   TCL_STATIC   : constant := 0;
   TCL_DYNAMIC  : constant := 3;

   --
   --  Flag values passed to variable-related procedures.
   --
   TCL_GLOBAL_ONLY          : constant := 16#00001#;
   TCL_NAMESPACE_ONLY       : constant := 16#00002#;
   TCL_APPEND_VALUE         : constant := 16#00004#;
   TCL_LIST_ELEMENT         : constant := 16#00008#;
   TCL_TRACE_READS          : constant := 16#00010#;
   TCL_TRACE_WRITES         : constant := 16#00020#;
   TCL_TRACE_UNSETS         : constant := 16#00040#;
   TCL_TRACE_DESTROYED      : constant := 16#00080#;
   TCL_INTERP_DESTROYED     : constant := 16#00100#;
   TCL_LEAVE_ERR_MSG        : constant := 16#00200#;
   TCL_TRACE_ARRAY          : constant := 16#00800#;
   TCL_TRACE_OLD_STYLE      : constant := 16#01000#;
   TCL_TRACE_RESULT_DYNAMIC : constant := 16#08000#;
   TCL_TRACE_RESULT_OBJECT  : constant := 16#10000#;

   --
   --  Flag values passed to command-related procedures.
   --
   TCL_TRACE_RENAME             : constant := 16#02000#;
   TCL_TRACE_DELETE             : constant := 16#04000#;

   TCL_ALLOW_INLINE_COMPILATION : constant := 16#20000#;

   --
   --  Flag values passed to Tcl_CreateObjTrace, and used internally by
   --  command execution traces.  Slots 4,8,16 and 32 are used
   --  internally by execution traces (see tclCmdMZ.c)
   --
   TCL_TRACE_ENTER_EXEC : constant := 1;
   TCL_TRACE_LEAVE_EXEC : constant := 2;

   --
   --  The TCL_PARSE_PART1 flag is deprecated and has no effect.
   --  The part1 is now always parsed whenever the part2 is NULL.
   --  {This is to avoid a common error when converting code to
   --   use the new object based APIs and forgetting to give the
   --   flag}
   --
   TCL_PARSE_PART1 : constant := 16#400#;

   --
   --  Types for linked variables:
   --
   TCL_LINK_INT       : constant := 1;
   TCL_LINK_DOUBLE    : constant := 2;
   TCL_LINK_BOOLEAN   : constant := 3;
   TCL_LINK_STRING    : constant := 4;
   TCL_LINK_READ_ONLY : constant := 16#80#;

   --
   --  Forward declaration of Tcl_HashTable and related types.
   --

   --
   --  Structure definition for an entry in a hash table; use the
   --  subprograms defined below.
   --

   type Tcl_HashEntry_Rec (<>) is private;
   type Tcl_HashEntry is access all Tcl_HashEntry_Rec;
   pragma Convention (C, Tcl_HashEntry);

   type Tcl_HashEntry_Array is
     array (CNatural range <>) of aliased Tcl_HashEntry;
   pragma Convention (C, Tcl_HashEntry_Array);

   type Tcl_HashEntry_Ptr is access all Tcl_HashEntry;
   pragma Convention (C, Tcl_HashEntry_Ptr);

   type Tcl_HashKeyType_Rec (<>) is private;
   type Tcl_HashKeyType is access all Tcl_HashKeyType_Rec;
   pragma Convention (C, Tcl_HashKeyType);

   type Tcl_HashSearch_Rec is private;
   type Tcl_HashSearch is access all Tcl_HashSearch_Rec;
   pragma Convention (C, Tcl_HashSearch);

   type Tcl_HashTable_Rec is private;
   type Tcl_HashTable is access all Tcl_HashTable_Rec;
   pragma Convention (C, Tcl_HashTable);

   --
   --  Acceptable key types for hash tables:
   --

   TCL_STRING_KEYS   : constant := 0;
   TCL_ONE_WORD_KEYS : constant := 1;

   --
   --  Macros for clients to use to access fields of hash entries:
   --

   function Tcl_GetHashValue
     (HashEntry : not null Tcl_HashEntry)
      return      ClientData;

   procedure Tcl_SetHashValue
     (HashEntry : not null Tcl_HashEntry;
      value     : ClientData);

   function Tcl_GetHashKey
     (HashTable : not null Tcl_HashTable;
      HashEntry : not null Tcl_HashEntry)
      return      C.Strings.chars_ptr;

   --
   --  Macros to use for clients to use to invoke find and create procedures
   --  for hash tables:
   --

   function Tcl_FindHashEntry
     (HashTable : not null Tcl_HashTable;
      key       : C.Strings.chars_ptr)
      return      Tcl_HashEntry;

   function Tcl_CreateHashEntry
     (HashTable : not null Tcl_HashTable;
      key       : C.Strings.chars_ptr;
      newPtr    : not null access C.int)
      return      Tcl_HashEntry;

   --
   --  Flag values to pass to Tcl_DoOneEvent to disable searches
   --  for some kinds of events:
   --

   TCL_DONT_WAIT     : constant := 2;
   TCL_WINDOW_EVENTS : constant := 4;
   TCL_FILE_EVENTS   : constant := 8;
   TCL_TIMER_EVENTS  : constant := 16;
   TCL_IDLE_EVENTS   : constant := 32;
   --  WAS 0x10 ????

   TCL_ALL_EVENTS : constant := -3;

   --
   --  The following structure defines a generic event for the Tcl
   --  event system.  These are the things that are queued in calls to
   --  Tcl_QueueEvent and serviced later by Tcl_DoOneEvent.  There can
   --  be many different kinds of events with different fields,
   --  corresponding to window events, timer events, etc.  The
   --  structure for a particular event consists of a Tcl_Event header
   --  followed by additional information specific to that event.
   --

   --
   --  Positions to pass to Tcl_QueueEvent:
   --

   type Tcl_QueuePosition is (TCL_QUEUE_TAIL, TCL_QUEUE_HEAD, TCL_QUEUE_MARK);
   for Tcl_QueuePosition'Size use 32;

   --
   --  Values to pass to Tcl_SetServiceMode to specify the behavior of notifier
   --  event routines.
   --

   TCL_SERVICE_NONE : constant := 0;
   TCL_SERVICE_ALL  : constant := 1;

   --
   --  The following structure is used to hold a time value, either as
   --  an absolute time {the number of seconds from the epoch} or as
   --  an elapsed time. On Unix systems the epoch is Midnight Jan 1,
   --  1970 GMT.  On Macintosh OS 9 systems the epoch is Midnight Jan
   --  1, 1904 GMT.
   --

   type Tcl_Time_Rec is record
      sec : C.long;
      --  Seconds.
      usec : C.long;
      --  Microseconds.
   end record;
   pragma Convention (C, Tcl_Time_Rec);
   type Tcl_Time is access all Tcl_Time_Rec;
   pragma Convention (C, Tcl_Time);

   type Tcl_SetTimerProc is access procedure (timePtr : Tcl_Time);
   pragma Convention (C, Tcl_SetTimerProc);

   type Tcl_WaitForEventProc is access function
     (timePtr : Tcl_Time)
   return       C.int;
   pragma Convention (C, Tcl_WaitForEventProc);

   --
   --  Bits to pass to Tcl_CreateFileHandler and Tcl_CreateChannelHandler
   --  to indicate what sorts of events are of interest:
   --
   TCL_READABLE  : constant := 2;
   TCL_WRITABLE  : constant := 4;
   TCL_EXCEPTION : constant := 8;

   --
   --  Flag values to pass to Tcl_OpenCommandChannel to indicate the
   --  disposition of the stdio handles.  TCL_STDIN, TCL_STDOUT,
   --  TCL_STDERR, are also used in Tcl_GetStdChannel.
   --
   TCL_STDIN        : constant := 2;
   TCL_STDOUT       : constant := 4;
   TCL_STDERR       : constant := 8;
   TCL_ENFORCE_MODE : constant := 16;

   --
   --  Bits passed to Tcl_DriverClose2Proc to indicate which side of a
   --  channel should be closed.
   --
   TCL_CLOSE_READ  : constant := 2;
   TCL_CLOSE_WRITE : constant := 4;

   --
   --  Value to use as the closeProc for a channel that supports the
   --  close2Proc interface.
   --
   TCL_CLOSE2PROC : constant := 1;

   --
   --  Typedefs for the various operations in a channel type:
   --
   type Tcl_DriverBlockModeProc is access function
     (instancedata : ClientData;
      mode         : C.int)
     return            C.int;
   pragma Convention (C, Tcl_DriverBlockModeProc);

   type Tcl_DriverCloseProc is access function
     (instancedata : ClientData;
      interp       : Tcl_Interp)
     return            C.int;
   pragma Convention (C, Tcl_DriverCloseProc);

   type Tcl_DriverClose2Proc is access function
     (instancedata : ClientData;
      interp       : Tcl_Interp;
      flags        : C.int)
     return            C.int;
   pragma Convention (C, Tcl_DriverClose2Proc);

   type Tcl_DriverInputProc is access function
     (instancedata : ClientData;
      buf          : C.Strings.chars_ptr;
      toRead       : C.int;
      errorCodePtr : access C.int)
     return            C.int;
   pragma Convention (C, Tcl_DriverInputProc);

   type Tcl_DriverOutputProc is access function
     (instancedata : ClientData;
      buf          : C.Strings.chars_ptr;
      toWrite      : C.int;
      errorCodePtr : access C.int)
     return            C.int;
   pragma Convention (C, Tcl_DriverOutputProc);

   type Tcl_DriverSeekProc is access function
     (instancedata : ClientData;
      offset       : C.long;
      mode         : C.int;
      errorCodePtr : access C.int)
     return            C.int;
   pragma Convention (C, Tcl_DriverSeekProc);

   type Tcl_DriverSetOptionProc is access function
     (instancedata : ClientData;
      interp       : Tcl_Interp;
      optionName   : C.Strings.chars_ptr;
      value        : C.Strings.chars_ptr)
     return            C.int;
   pragma Convention (C, Tcl_DriverSetOptionProc);

   type Tcl_DriverGetOptionProc is access function
     (instancedata : ClientData;
      interp       : Tcl_Interp;
      optionName   : C.Strings.chars_ptr;
      dsPtr        : Tcl_DString)
     return            C.int;
   pragma Convention (C, Tcl_DriverGetOptionProc);

   type Tcl_DriverWatchProc is access procedure
     (instancedata : ClientData;
      mask         : C.int);
   pragma Convention (C, Tcl_DriverWatchProc);

   type Tcl_DriverGetHandleProc is access function
     (instancedata : ClientData;
      direction    : C.int;
      handleptr    : ClientData)
   return            C.int;
   pragma Convention (C, Tcl_DriverGetHandleProc);

   type Tcl_DriverFlushProc is access function
      (instanceData : ClientData)
   return            C.int;

   type Tcl_DriverHandlerProc is access function
     (instancedata : ClientData;
      interestMask : C.int)
   return            C.int;

   type Tcl_DriverWideSeekProc is access function
     (instancedata : ClientData;
      offset       : Tcl_WideInt;
      mode         : C.int;
      errorCodePtr : access C.int)
   return            Tcl_WideInt;

   type Tcl_DriverThreadActionProc is access procedure
     (instancedata : ClientData;
      action       : C.int);

   type Tcl_DriverTruncateProc is access procedure
     (instancedata : ClientData;
      length       : Tcl_WideInt);

   --
   --  The following declarations either map ckalloc and ckfree to
   --  malloc and free, or they map them to procedures with all sorts
   --  of debugging hooks defined in tclCkalloc.c.
   --

   --  !TCL_MEM_DEBUG

   --
   --  If we are not using the debugging allocator, we should call the
   --  Tcl_Alloc, et al. routines in order to guarantee that every
   --  module is using the same memory allocator both inside and
   --  outside of the Tcl library.
   --

   --  !TCL_MEM_DEBUG

   --
   --  Enum for different end of line translation and recognition
   --  modes.
   --

   type Tcl_EolTranslation is
     (TCL_TRANSLATE_AUTO,      --  Eol == \r, \n and \r\n.
      TCL_TRANSLATE_CR,        --  Eol == \r.
      TCL_TRANSLATE_LF,        --  Eol == \n.
      TCL_TRANSLATE_CRLF);     --  Eol == \r\n.
   for Tcl_EolTranslation'Size use 32;

   --
   --  struct Tcl_ChannelType:
   --
   --  One such structure exists for each type {kind} of channel.  It
   --  collects together in one place all the functions that are part
   --  of the specific channel type.
   --
   --  It is recommend that the Tcl_Channel* functions are used to
   --  access elements of this structure, instead of direct accessing.
   --

   type Tcl_ChannelType_Rec is record
      --  The name of the channel type in Tcl commands. This storage
      --  is owned by channel type.
      typeName : C.Strings.chars_ptr;
      --  Version of the channel type.
      version : System.Address;
      --  Procedure to call to close the channel, or TCL_CLOSE2PROC if
      --  the close2Proc should be used instead.
      closeProc : Tcl_DriverCloseProc;
      --  Procedure to call for input on channel.
      inputProc : Tcl_DriverInputProc;
      --  Procedure to call for output on channel.
      outputProc : Tcl_DriverOutputProc;
      --  Procedure to call to seek on the channel. May be NULL.
      seekProc : Tcl_DriverSeekProc;
      --  Set an option on a channel.
      setOptionProc : Tcl_DriverSetOptionProc;
      --  Get an option from a channel.
      getOptionProc : Tcl_DriverGetOptionProc;
      --  Set up the notifier to watch for events on this channel.
      watchProc : Tcl_DriverWatchProc;
      --  Get an OS handle from the channel or NULL if not supported.
      getHandleProc : Tcl_DriverGetHandleProc;
      --  Procedure to call to close the channel if the device
      --  supports closing the read & write sides independently.
      close2Proc : Tcl_DriverClose2Proc;
      --  Set blocking mode for the raw channel. May be NULL.
      blockModeProc : Tcl_DriverBlockModeProc;
      --  Only valid in TCL_CHANNEL_VERSION_2 channels or later:
      --  Function to call to flush a channel. May be NULL.
      flushProc : Tcl_DriverFlushProc;
      --  Only valid in TCL_CHANNEL_VERSION_2 channels or later:
      --  Function to call to handle a channel event.  This will be
      --  passed up the stacked channel chain.
      handlerProc : Tcl_DriverHandlerProc;
      --  Only valid in TCL_CHANNEL_VERSION_3 channels or later:
      --  Function to call to seek on the channel which can handle
      --  64-bit offsets. May be NULL, and must be NULL if seekProc is
      --  NULL.
      wideSeekProc : Tcl_DriverWideSeekProc;
      --  Only valid in TCL_CHANNEL_VERSION_5 channels or later:
      --  Function to call to truncate the underlying file to a
      --  particular length. May be NULL if the channel does not
      --  support truncation.
      truncateProc : Tcl_DriverTruncateProc;
   end record;
   pragma Convention (C, Tcl_ChannelType_Rec);
   type Tcl_ChannelType is access all Tcl_ChannelType_Rec;
   pragma Convention (C, Tcl_ChannelType);

   --
   --  The following flags determine whether the blockModeProc above
   --  should set the channel into blocking or nonblocking mode. They
   --  are passed as arguments to the blockModeProc procedure in the
   --  above structure.
   --

   TCL_MODE_BLOCKING : constant := 0;
   --  Put channel into blocking mode.

   TCL_MODE_NONBLOCKING : constant := 1;
   --  Put channel into nonblocking mode.

   --
   --  Enum for different types of file paths.
   --
   type Tcl_PathType is (TCL_PATH_ABSOLUTE,
                         TCL_PATH_RELATIVE,
                         TCL_PATH_VOLUME_RELATIVE);
   for Tcl_PathType'Size use 32;

   --
   --  The following structure represents the Notifier functions that
   --  you can override with the Tcl_SetNotifier call.
   --

   type Tcl_NotifierProcs_Rec is record
      setTimerProc          : Tcl_SetTimerProc;
      waitForEventProc      : Tcl_WaitForEventProc;
      createFileHandlerProc : Tcl_CreateFileHandlerProc;
      deleteFileHandlerProc : Tcl_DeleteFileHandlerProc;
      initNotifierProc      : Tcl_InitNotifierProc;
      finalizeNotifierProc  : Tcl_FinalizeNotifierProc;
      alertNotifierProc     : Tcl_AlertNotifierProc;
      serviceModeHookProc   : Tcl_ServiceModeHookProc;
   end record;
   pragma Convention (C, Tcl_NotifierProcs_Rec);
   type Tcl_NotifierProcs is access all Tcl_NotifierProcs_Rec;
   pragma Convention (C, Tcl_NotifierProcs);

   --
   --  The following structure represents a user-defined encoding.  It
   --  collects together all the functions that are used by the
   --  specific encoding.
   --

   type Tcl_EncodingType_Rec is record
      encodingName : C.Strings.chars_ptr;
      --  The name of the encoding, e.g.  "euc-jp".
      --  This name is the unique key for this
      --  encoding type.
      toUtfProc : Tcl_EncodingConvertProc;
      --  Procedure to convert from external
      --  encoding into UTF-8.
      fromUtfProc : Tcl_EncodingConvertProc;
      --  Procedure to convert from UTF-8 into
      --  external encoding.
      freeProc : Tcl_EncodingFreeProc;
      --  If non-NULL, procedure to call when this
      --  encoding is deleted.
      data : ClientData;
      --  Arbitrary value associated with encoding
      --  type.  Passed to conversion procedures.
      nullSize : C.int;
      --  Number of zero bytes that signify
      --  end-of-string in this encoding.  This
      --  number is used to determine the source
      --  string length when the srcLen argument is
      --  negative.  Must be 1 or 2.
   end record;
   pragma Convention (C, Tcl_EncodingType_Rec);
   type Tcl_EncodingType is access all Tcl_EncodingType_Rec;
   pragma Convention (C, Tcl_EncodingType);

   --
   --  The following definitions are used as values for the conversion control
   --  flags argument when converting text from one character set to another:
   --
   --  TCL_ENCODING_START:      Signifies that the source buffer is the first
   --                           block in a {potentially multi-block} input
   --                           stream.  Tells the conversion procedure to
   --                           reset to an initial state and perform any
   --                           initialization that needs to occur before the
   --                           first byte is converted.  If the source
   --                           buffer contains the entire input stream to be
   --                           converted, this flag should be set.
   --
   --  TCL_ENCODING_END:        Signifies that the source buffer is the last
   --                           block in a {potentially multi-block} input
   --                           stream.  Tells the conversion routine to
   --                           perform any finalization that needs to occur
   --                           after the last byte is converted and then to
   --                           reset to an initial state.  If the source
   --                           buffer contains the entire input stream to be
   --                           converted, this flag should be set.
   --
   --  TCL_ENCODING_STOPONERROR: If set, then the converter will return
   --                           immediately upon encountering an invalid
   --                           byte sequence or a source character that has
   --                           no mapping in the target encoding.  If clear,
   --                           then the converter will skip the problem,
   --                           substituting one or more "close" characters
   --                           in the destination buffer and then continue
   --                           to sonvert the source.
   --

   TCL_ENCODING_START       : constant := 1;
   TCL_ENCODING_END         : constant := 2;
   TCL_ENCODING_STOPONERROR : constant := 4;

   --
   -- ----------------------------------------------------------------
   --  The following data structures and declarations are for the new
   --  Tcl parser.
   -- ----------------------------------------------------------------
   --

   --
   --  For each word of a command, and for each piece of a word such as a
   --  variable reference, one of the following structures is created to
   --  describe the token.
   --
   --  The tokens in a parse result are always presented in an array.
   --
   type Tcl_Token_Rec is record
      typ : C.int;
      --  Type of token, such as TCL_TOKEN_WORD; see below for valid
      --  types.
      start : C.Strings.chars_ptr;
      --  First character in token.
      size : C.int;
      --  Number of bytes in token.
      numComponents : C.int;
      --  If this token is composed of other tokens, this field tells
      --  how many of them there are {including components of
      --  components, etc.}. The component tokens immediately follow
      --  this one in the array of tokens of which this token is part.
   end record;
   pragma Convention (C, Tcl_Token_Rec);

   type Tcl_Token_Array is array
     (CNatural range <>) of aliased Tcl_Token_Rec;
   pragma Convention (C, Tcl_Token_Array);

   package Tcl_Token_Pointers is new C.Pointers
     (Index => CNatural,
      Element => Tcl_Token_Rec,
      Element_Array => Tcl_Token_Array,
      Default_Terminator => (0, C.Strings.Null_Ptr, 0, 0));
   --  Note, the Default_Terminator is required by
   --  Interfaces.C.Pointers but isn't significant in a
   --  Tcl_Token_Array, so don't use any of the subprograms that
   --  depend on the terminator.
   subtype Tcl_Token is Tcl_Token_Pointers.Pointer;

   --
   --  Type values defined for Tcl_Token structures.  These values are
   --  defined as mask bits so that it's easy to check for collections of
   --  types.
   --
   --  TCL_TOKEN_WORD -         The token describes one word of a command,
   --                           from the first non-blank character of
   --                           the word (which may be " or \}) up to but
   --                           not including the space, semicolon, or
   --                           bracket that terminates the word.
   --                           NumComponents counts the total number of
   --                           sub-tokens that make up the word.  This
   --                           includes, for example, sub-tokens of
   --                           TCL_TOKEN_VARIABLE tokens.
   --  TCL_TOKEN_SIMPLE_WORD -  This token is just like TCL_TOKEN_WORD
   --                           except that the word is guaranteed to
   --                           consist of a single TCL_TOKEN_TEXT
   --                           sub-token.
   --  TCL_TOKEN_TEXT -         The token describes a range of literal
   --                           text that is part of a word.
   --                           NumComponents is always 0.
   --  TCL_TOKEN_BS -           The token describes a backslash sequence
   --                           that must be collapsed.  NumComponents
   --                           is always 0.
   --  TCL_TOKEN_COMMAND -      The token describes a command whose result
   --                           must be substituted into the word.  The
   --                           token includes the enclosing brackets.
   --                           NumComponents is always 0.
   --  TCL_TOKEN_VARIABLE -     The token describes a variable
   --                           substitution, including the dollar sign,
   --                           variable name, and array index {if there
   --                           is one} up through the right
   --                           parentheses.  NumComponents tells how
   --                           many additional tokens follow to
   --                           represent the variable name.  The first
   --                           token will be a TCL_TOKEN_TEXT token
   --                           that describes the variable name.  If
   --                           the variable is an array reference then
   --                           there will be one or more additional
   --                           tokens, of type TCL_TOKEN_TEXT,
   --                           TCL_TOKEN_BS, TCL_TOKEN_COMMAND, and
   --                           TCL_TOKEN_VARIABLE, that describe the
   --                           array index; numComponents counts the
   --                           total number of nested tokens that make
   --                           up the variable reference, including
   --                           sub-tokens of TCL_TOKEN_VARIABLE tokens.
   --  TCL_TOKEN_SUB_EXPR -     The token describes one subexpression of a
   --                           expression, from the first non-blank
   --                           character of the subexpression up to but not
   --                           including the space, brace, or bracket
   --                           that terminates the subexpression.
   --                           NumComponents counts the total number of
   --                           following subtokens that make up the
   --                           subexpression; this includes all subtokens
   --                           for any nested TCL_TOKEN_SUB_EXPR tokens.
   --                           For example, a numeric value used as a
   --                           primitive operand is described by a
   --                           TCL_TOKEN_SUB_EXPR token followed by a
   --                           TCL_TOKEN_TEXT token. A binary subexpression
   --                           is described by a TCL_TOKEN_SUB_EXPR token
   --                           followed by the TCL_TOKEN_OPERATOR token
   --                           for the operator, then TCL_TOKEN_SUB_EXPR
   --                           tokens for the left then the right operands.
   --  TCL_TOKEN_OPERATOR -     The token describes one expression operator.
   --                           An operator might be the name of a math
   --                           function such as "abs". A TCL_TOKEN_OPERATOR
   --                           token is always preceeded by one
   --                           TCL_TOKEN_SUB_EXPR token for the operator's
   --                           subexpression, and is followed by zero or
   --                           more TCL_TOKEN_SUB_EXPR tokens for the
   --                           operator's operands. NumComponents is
   --                           always 0.
   --
   TCL_TOKEN_WORD        : constant := 1;
   TCL_TOKEN_SIMPLE_WORD : constant := 2;
   TCL_TOKEN_TEXT        : constant := 4;
   TCL_TOKEN_BS          : constant := 8;
   TCL_TOKEN_COMMAND     : constant := 16;
   TCL_TOKEN_VARIABLE    : constant := 32;
   TCL_TOKEN_SUB_EXPR    : constant := 64;
   TCL_TOKEN_OPERATOR    : constant := 128;

   --
   --  Parsing error types.  On any parsing error, one of these values
   --  will be stored in the error field of the Tcl_Parse structure
   --  defined below.
   --
   TCL_PARSE_SUCCESS           : constant := 0;
   TCL_PARSE_QUOTE_EXTRA       : constant := 1;
   TCL_PARSE_BRACE_EXTRA       : constant := 2;
   TCL_PARSE_MISSING_BRACE     : constant := 3;
   TCL_PARSE_MISSING_BRACKET   : constant := 4;
   TCL_PARSE_MISSING_PAREN     : constant := 5;
   TCL_PARSE_MISSING_QUOTE     : constant := 6;
   TCL_PARSE_MISSING_VAR_BRACE : constant := 7;
   TCL_PARSE_SYNTAX            : constant := 8;
   TCL_PARSE_BAD_NUMBER        : constant := 9;

   --
   --  A structure of the following type is filled in by
   --  Tcl_ParseCommand, Tcl_ParseExpr, Tcl_ParseBraces,
   --  Tcl_ParseQuotedString, and Tcl_ParseVarName.
   --
   --  It describes a single command (or other construct, as
   --  appropriate) parsed from an input string.
   --
   --  The first five fields (commentStart through numWords) are only
   --  filled in by Tcl_ParseCommand.
   --
   type Tcl_Parse_Rec is record
      commentStart : C.Strings.chars_ptr;
      --  Pointer to # that begins the first of one or more comments
      --  preceding the command.
      commentSize : C.int;
      --  Number of bytes in comments {up through newline character
      --  that terminates the last comment}.  If there were no
      --  comments, this field is 0.
      commandStart : C.Strings.chars_ptr;
      --  First character in first word of command.
      commandSize : C.int;
      --  Number of bytes in command, including first character of
      --  first word, up through the terminating newline, close
      --  bracket, or semicolon.
      numWords : C.int;
      --  Total number of words in command.  May be 0.
      tokenPtr : Tcl_Token;
      --  Pointer to first token representing the words of the
      --  command.  Initially points to staticTokens, but may change
      --  to point to malloc-ed space if command exceeds space in
      --  staticTokens.
      numTokens : C.int;
      --  Total number of tokens in command.
      tokensAvailable : C.int;
      --  Total number of tokens available at
      --  *tokenPtr.
      errorType : C.int;
      --  One of the parsing error types defined above.
      --
      --  The fields below are intended only for the private use of
      --  the parser.  They should not be used by procedures that
      --  invoke Tcl_ParseCommand.
      --
      strng : C.Strings.chars_ptr;
      --  The original command string passed to Tcl_ParseCommand.
      e_n_d : C.Strings.chars_ptr;
      --  Points to the character just after the last one in the
      --  command string.
      interp : Tcl_Interp;
      --  Interpreter to use for error reporting, or NULL.
      term : C.Strings.chars_ptr;
      --  Points to character in string that terminated most recent
      --  token.  Filled in by ParseTokens.  If an error occurs,
      --  points to beginning of region where the error occurred
      --  {e.g. the open brace if the close brace is missing}.
      incomplete : C.int;
      --  This field is set to 1 by Tcl_ParseCommand if the command
      --  appears to be incomplete.  This information is used by
      --  Tcl_CommandComplete.
      staticTokens : Tcl_Token_Array
        (0 .. Tcl_Record_Sizes.NUM_STATIC_TOKENS - 1);
      --  Initial space for tokens for command.  This space should be
      --  large enough to accommodate most commands; dynamic space is
      --  allocated for very large commands that don't fit here.
   end record;
   pragma Convention (C, Tcl_Parse_Rec);
   type Tcl_Parse is access all Tcl_Parse_Rec;
   pragma Convention (C, Tcl_Parse);

   --
   --  The following definitions are the error codes returned by the
   --  conversion routines:
   --
   --  TCL_OK:                  All characters were converted.
   --
   --  TCL_CONVERT_NOSPACE:     The output buffer would not have been large
   --                           enough for all of the converted data; as many
   --                           characters as could fit were converted though.
   --
   --  TCL_CONVERT_MULTIBYTE:   The last few bytes in the source string were
   --                           the beginning of a multibyte sequence, but
   --                           more bytes were needed to complete this
   --                           sequence.  A subsequent call to the conversion
   --                           routine should pass the beginning of this
   --                           unconverted sequence plus additional bytes
   --                           from the source stream to properly convert
   --                           the formerly split-up multibyte sequence.
   --
   --  TCL_CONVERT_SYNTAX:      The source stream contained an invalid
   --                           character sequence.  This may occur if the
   --                           input stream has been damaged or if the input
   --                           encoding method was misidentified.  This error
   --                           is reported only if TCL_ENCODING_STOPONERROR
   --                           was specified.
   --
   --  TCL_CONVERT_UNKNOWN:     The source string contained a character
   --                           that could not be represented in the target
   --                           encoding.  This error is reported only if
   --                           TCL_ENCODING_STOPONERROR was specified.
   --

   TCL_CONVERT_MULTIBYTE : constant := -1;
   TCL_CONVERT_SYNTAX    : constant := -2;
   TCL_CONVERT_UNKNOWN   : constant := -3;
   TCL_CONVERT_NOSPACE   : constant := -4;

   --
   --  The maximum number of bytes that are necessary to represent a single
   --  Unicode character in UTF-8.
   --
   TCL_UTF_MAX : constant := 3;

   --
   --  This represents a Unicode character.
   --
   subtype Tcl_UniChar is C.unsigned_short;

   --
   --  Deprecated Tcl procedures:
   --

   --
   --  These function have been renamed. The old names are deprecated, but we
   --  define these macros for backwards compatibilty.
   --

   --  Tcl_Return is not implemented because its name conflicts
   --  with an enumeration literal.  Use Tcl_SetResult instead.

   function Tcl_TranslateFileName
     (interp    : not null Tcl_Interp;
      name      : C.Strings.chars_ptr;
      bufferPtr : not null Tcl_DString)
     return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_TranslateFileName, "Tcl_TranslateFileName");

   function Tcl_TildeSubst
     (interp    : not null Tcl_Interp;
      name      : C.Strings.chars_ptr;
      bufferPtr : not null Tcl_DString)
     return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_TildeSubst, "Tcl_TranslateFileName");

   procedure Tcl_Panic
     (format  : C.Strings.chars_ptr;
      String1 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Import (C, Tcl_Panic, "Tcl_Panic");

   procedure panic
     (format  : C.Strings.chars_ptr;
      String1 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Import (C, panic, "Tcl_Panic");

   --
   --  The following constant is used to test for older versions of Tcl
   --  in the stubs tables.
   --
   --  Jan Nijtman's plus patch uses 0xFCA1BACF, so we need to pick a different
   --  value since the stubs tables don't match.
   --

   TCL_STUB_MAGIC : constant := -56378673;

   --
   --  The following function is required to be defined in all stubs aware
   --  extensions.  The function is actually implemented in the stub
   --  library, not the main Tcl library, although there is a trivial
   --  implementation in the main library in case an extension is statically
   --  linked into an application.
   --

   --
   --  When not using stubs, make it a macro.
   --

   --
   --  Include the public function declarations that are accessible via
   --  the stubs table.
   --

   --
   --  tclDecls.h --
   --
   --   Declarations of functions in the platform independent public Tcl API.
   --
   --  Copyright {c} 1998-1999 by Scriptics Corporation.
   --
   --
   --

   --
   --  WARNING: This file is automatically generated by the tools/genStubs.tcl
   --  script.  Any modifications to the function declarations below should be
   --  made
   --  in the generic/tcl.decls script.
   --

   --  !BEGIN!: Do not edit below this line.

   --
   --  Exported function declarations:
   --

   --  0

   procedure Tcl_PkgProvideEx
     (interp  : not null Tcl_Interp;
      name    : C.Strings.chars_ptr;
      version : C.Strings.chars_ptr;
      data    : ClientData);
   pragma Import (C, Tcl_PkgProvideEx, "Tcl_PkgProvideEx");

   --  1

   function Tcl_PkgRequireEx
     (interp        : not null Tcl_Interp;
      name          : C.Strings.chars_ptr;
      version       : C.Strings.chars_ptr;
      exact         : C.int;
      clientdataptr : access ClientData)    -- can be null
      return          C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgRequireEx, "Tcl_PkgRequireEx");

   --  2

   --  3

   --  4

   --  5

   --  6

   function Tcl_DbCkalloc
     (size : C.unsigned;
      file : C.Strings.chars_ptr;
      line : C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_DbCkalloc, "Tcl_DbCkalloc");

   --  7

   function Tcl_DbCkfree
     (ptr  : C.Strings.chars_ptr;
      file : C.Strings.chars_ptr;
      line : C.int)
      return C.int;
   pragma Import (C, Tcl_DbCkfree, "Tcl_DbCkfree");

   --  8

   function Tcl_DbCkrealloc
     (ptr  : C.Strings.chars_ptr;
      size : C.unsigned;
      file : C.Strings.chars_ptr;
      line : C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_DbCkrealloc, "Tcl_DbCkrealloc");

   --  UNIX

   --  9

   procedure Tcl_CreateFileHandler
     (fd   : C.int;
      mask : C.int;
      proc : not null Tcl_FileProc;
      data : ClientData);
   pragma Import (C, Tcl_CreateFileHandler, "Tcl_CreateFileHandler");

   --  UNIX

   --  UNIX

   --  10

   procedure Tcl_DeleteFileHandler (fd : C.int);
   pragma Import (C, Tcl_DeleteFileHandler, "Tcl_DeleteFileHandler");

   --  UNIX

   --  11

   procedure Tcl_SetTimer (timePtr : not null Tcl_Time);
   pragma Import (C, Tcl_SetTimer, "Tcl_SetTimer");

   --  12

   procedure Tcl_Sleep (ms : C.int);
   pragma Import (C, Tcl_Sleep, "Tcl_Sleep");

   --  13

   function Tcl_WaitForEvent (timePtr : Tcl_Time) return C.int;
   pragma Import (C, Tcl_WaitForEvent, "Tcl_WaitForEvent");

   --  14

   function Tcl_AppendAllObjTypes
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_AppendAllObjTypes, "Tcl_AppendAllObjTypes");

   --  15

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
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr);

   --  16

   procedure Tcl_AppendToObj
     (objPtr : not null Tcl_Obj;
      bytes  : C.Strings.chars_ptr;
      length : C.int);
   pragma Import (C, Tcl_AppendToObj, "Tcl_AppendToObj");

   --  17

   function Tcl_ConcatObj
     (objc : C.int;
      objv : Tcl_Obj_Array)
      return Tcl_Obj;
   pragma Import (C, Tcl_ConcatObj, "Tcl_ConcatObj");

   --  18

   function Tcl_ConvertToType
     (interp  : not null Tcl_Interp;
      objPtr  : not null Tcl_Obj;
      typePtr : not null Tcl_ObjType)
      return    C.int;
   pragma Import (C, Tcl_ConvertToType, "Tcl_ConvertToType");

   --  19

   procedure Tcl_DbDecrRefCount
     (objPtr : not null Tcl_Obj;
      file   : C.Strings.chars_ptr;
      line   : C.int);
   pragma Import (C, Tcl_DbDecrRefCount, "Tcl_DbDecrRefCount");

   --  20

   procedure Tcl_DbIncrRefCount
     (objPtr : not null Tcl_Obj;
      file   : C.Strings.chars_ptr;
      line   : C.int);
   pragma Import (C, Tcl_DbIncrRefCount, "Tcl_DbIncrRefCount");

   --  21

   function Tcl_DbIsShared
     (objPtr : not null Tcl_Obj;
      file   : C.Strings.chars_ptr;
      line   : C.int)
      return   C.int;
   pragma Import (C, Tcl_DbIsShared, "Tcl_DbIsShared");

   --  22

   function Tcl_DbNewBooleanObj
     (boolValue : C.int;
      file      : C.Strings.chars_ptr;
      line      : C.int)
      return      Tcl_Obj;
   pragma Import (C, Tcl_DbNewBooleanObj, "Tcl_DbNewBooleanObj");

   --  23

   function Tcl_DbNewByteArrayObj
     (bytes  : C.Strings.chars_ptr;
      length : C.int;
      file   : C.Strings.chars_ptr;
      line   : C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_DbNewByteArrayObj, "Tcl_DbNewByteArrayObj");

   --  24

   function Tcl_DbNewDoubleObj
     (doubleValue : C.double;
      file        : C.Strings.chars_ptr;
      line        : C.int)
      return        Tcl_Obj;
   pragma Import (C, Tcl_DbNewDoubleObj, "Tcl_DbNewDoubleObj");

   --  25

   function Tcl_DbNewListObj
     (objc : C.int;
      objv : Tcl_Obj_Array;
      file : C.Strings.chars_ptr;
      line : C.int)
      return Tcl_Obj;
   pragma Import (C, Tcl_DbNewListObj, "Tcl_DbNewListObj");

   --  26

   function Tcl_DbNewLongObj
     (longValue : C.long;
      file      : C.Strings.chars_ptr;
      line      : C.int)
      return      Tcl_Obj;
   pragma Import (C, Tcl_DbNewLongObj, "Tcl_DbNewLongObj");

   --  27

   function Tcl_DbNewObj
     (file : C.Strings.chars_ptr;
      line : C.int)
      return Tcl_Obj;
   pragma Import (C, Tcl_DbNewObj, "Tcl_DbNewObj");

   --  28

   function Tcl_DbNewStringObj
     (bytes  : C.Strings.chars_ptr;
      length : C.int;
      file   : C.Strings.chars_ptr;
      line   : C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_DbNewStringObj, "Tcl_DbNewStringObj");

   --  29

   function Tcl_DuplicateObj (objPtr : not null Tcl_Obj) return Tcl_Obj;
   pragma Import (C, Tcl_DuplicateObj, "Tcl_DuplicateObj");

   --  30

   --  TclFreeObj is only in tcl.h to support the reference counting
   --  macros, which in TASH are implemented as C functions; so it's
   --  omitted here.

--     procedure TclFreeObj (objPtr : not null Tcl_Obj);
--     pragma Import (C, TclFreeObj, "TclFreeObj");

   --  31

   function Tcl_GetBoolean
     (interp  : not null Tcl_Interp;
      str     : C.Strings.chars_ptr;
      boolPtr : not null access C.int)
      return    C.int;
   pragma Import (C, Tcl_GetBoolean, "Tcl_GetBoolean");

   --  32

   function Tcl_GetBooleanFromObj
     (interp  : not null Tcl_Interp;
      objPtr  : not null Tcl_Obj;
      boolPtr : not null access C.int)
      return    C.int;
   pragma Import (C, Tcl_GetBooleanFromObj, "Tcl_GetBooleanFromObj");

   --  33

   function Tcl_GetByteArrayFromObj
     (objPtr    : not null Tcl_Obj;
      lengthPtr : access C.int)               -- can be null
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetByteArrayFromObj, "Tcl_GetByteArrayFromObj");

   --  34

   function Tcl_GetDouble
     (interp    : not null Tcl_Interp;
      str       : C.Strings.chars_ptr;
      doublePtr : not null access C.double)
      return      C.int;
   pragma Import (C, Tcl_GetDouble, "Tcl_GetDouble");

   --  35

   function Tcl_GetDoubleFromObj
     (interp    : not null Tcl_Interp;
      objPtr    : not null Tcl_Obj;
      doublePtr : not null access C.double)
      return      C.int;
   pragma Import (C, Tcl_GetDoubleFromObj, "Tcl_GetDoubleFromObj");

   --  36

   function Tcl_GetIndexFromObj
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : CArgv.Chars_Ptr_Ptr;
      msg      : C.Strings.chars_ptr;
      flags    : C.int;
      indexPtr : not null access C.int)
      return     C.int;
   pragma Import (C, Tcl_GetIndexFromObj, "Tcl_GetIndexFromObj");

   --  37

   function Tcl_GetInt
     (interp : not null Tcl_Interp;
      str    : C.Strings.chars_ptr;
      intPtr : not null access C.int)
      return   C.int;
   pragma Import (C, Tcl_GetInt, "Tcl_GetInt");

   --  38

   function Tcl_GetIntFromObj
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj;
      intPtr : not null access C.int)
      return   C.int;
   pragma Import (C, Tcl_GetIntFromObj, "Tcl_GetIntFromObj");

   --  39

   function Tcl_GetLongFromObj
     (interp  : not null Tcl_Interp;
      objPtr  : not null Tcl_Obj;
      longPtr : not null access C.long)
      return    C.int;
   pragma Import (C, Tcl_GetLongFromObj, "Tcl_GetLongFromObj");

   --  40

   function Tcl_GetObjType
     (typeName : C.Strings.chars_ptr)
      return     Tcl_ObjType;
   pragma Import (C, Tcl_GetObjType, "Tcl_GetObjType");

   function Tcl_GetObjTypeName
     (objPtr : Tcl_Obj)
      return   C.Strings.chars_ptr;

   --  41

   function Tcl_GetStringFromObj
     (objPtr    : not null Tcl_Obj;
      lengthPtr : access C.int)            -- can be null
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetStringFromObj, "Tcl_GetStringFromObj");

   --  42

   procedure Tcl_InvalidateStringRep (objPtr : not null Tcl_Obj);
   pragma Import (C, Tcl_InvalidateStringRep, "Tcl_InvalidateStringRep");

   --  43

   function Tcl_ListObjAppendList
     (interp      : not null Tcl_Interp;
      listPtr     : not null Tcl_Obj;
      elemListPtr : not null Tcl_Obj)
      return        C.int;
   pragma Import (C, Tcl_ListObjAppendList, "Tcl_ListObjAppendList");

   --  44

   function Tcl_ListObjAppendElement
     (interp  : not null Tcl_Interp;
      listPtr : not null Tcl_Obj;
      objPtr  : not null Tcl_Obj)
      return    C.int;
   pragma Import (C, Tcl_ListObjAppendElement, "Tcl_ListObjAppendElement");

   --  45

   function Tcl_ListObjGetElements
     (interp  : not null Tcl_Interp;
      listPtr : not null Tcl_Obj;
      objcPtr : not null access C.int;
      objv    : Tcl_Obj_Array)
      return    C.int;
   pragma Import (C, Tcl_ListObjGetElements, "Tcl_ListObjGetElements");

   --  46

   function Tcl_ListObjIndex
     (interp    : not null Tcl_Interp;
      listPtr   : not null Tcl_Obj;
      index     : C.int;
      objPtrPtr : not null access Tcl_Obj)
      return      C.int;
   pragma Import (C, Tcl_ListObjIndex, "Tcl_ListObjIndex");

   --  47

   function Tcl_ListObjLength
     (interp  : not null Tcl_Interp;
      listPtr : not null Tcl_Obj;
      intPtr  : not null access C.int)
      return    C.int;
   pragma Import (C, Tcl_ListObjLength, "Tcl_ListObjLength");

   --  48

   function Tcl_ListObjReplace
     (interp  : not null Tcl_Interp;
      listPtr : not null Tcl_Obj;
      first   : C.int;
      count   : C.int;
      objc    : C.int;
      objv    : Tcl_Obj_Array)
      return    C.int;
   pragma Import (C, Tcl_ListObjReplace, "Tcl_ListObjReplace");

   --  49

   function Tcl_NewBooleanObj (boolValue : C.int) return Tcl_Obj;
   pragma Import (C, Tcl_NewBooleanObj, "Tcl_NewBooleanObj");

   --  50

   function Tcl_NewByteArrayObj
     (bytes  : C.Strings.chars_ptr;
      length : C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_NewByteArrayObj, "Tcl_NewByteArrayObj");

   --  51

   function Tcl_NewDoubleObj (doubleValue : C.double) return Tcl_Obj;
   pragma Import (C, Tcl_NewDoubleObj, "Tcl_NewDoubleObj");

   --  52

   function Tcl_NewIntObj (intValue : C.int) return Tcl_Obj;
   pragma Import (C, Tcl_NewIntObj, "Tcl_NewIntObj");

   --  53

   function Tcl_NewListObj
     (objc : C.int;
      objv : Tcl_Obj_Array)
      return Tcl_Obj;
   pragma Import (C, Tcl_NewListObj, "Tcl_NewListObj");

   --  54

   function Tcl_NewLongObj (longValue : C.long) return Tcl_Obj;
   pragma Import (C, Tcl_NewLongObj, "Tcl_NewLongObj");

   --  55

   function Tcl_NewObj return Tcl_Obj;
   pragma Import (C, Tcl_NewObj, "Tcl_NewObj");

   --  56

   function Tcl_NewStringObj
     (bytes  : C.Strings.chars_ptr;
      length : C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_NewStringObj, "Tcl_NewStringObj");

   --  57

   procedure Tcl_SetBooleanObj (objPtr : not null Tcl_Obj;
                                boolValue : C.int);
   pragma Import (C, Tcl_SetBooleanObj, "Tcl_SetBooleanObj");

   --  58

   function Tcl_SetByteArrayLength
     (objPtr : not null Tcl_Obj;
      length : C.int)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_SetByteArrayLength, "Tcl_SetByteArrayLength");

   --  59

   procedure Tcl_SetByteArrayObj
     (objPtr : not null Tcl_Obj;
      bytes  : C.Strings.chars_ptr;
      length : C.int);
   pragma Import (C, Tcl_SetByteArrayObj, "Tcl_SetByteArrayObj");

   --  60

   procedure Tcl_SetDoubleObj
     (objPtr      : not null Tcl_Obj;
      doubleValue : C.double);
   pragma Import (C, Tcl_SetDoubleObj, "Tcl_SetDoubleObj");

   --  61

   procedure Tcl_SetIntObj (objPtr : not null Tcl_Obj; intValue : C.int);
   pragma Import (C, Tcl_SetIntObj, "Tcl_SetIntObj");

   --  62

   procedure Tcl_SetListObj
     (objPtr : not null Tcl_Obj;
      objc   : C.int;
      objv   : Tcl_Obj_Array);
   pragma Import (C, Tcl_SetListObj, "Tcl_SetListObj");

   --  63

   procedure Tcl_SetLongObj (objPtr : not null Tcl_Obj; longValue : C.long);
   pragma Import (C, Tcl_SetLongObj, "Tcl_SetLongObj");

   --  64

   procedure Tcl_SetObjLength (objPtr : not null Tcl_Obj; length : C.int);
   pragma Import (C, Tcl_SetObjLength, "Tcl_SetObjLength");

   --  65

   procedure Tcl_SetStringObj
     (objPtr : not null Tcl_Obj;
      bytes  : C.Strings.chars_ptr;
      length : C.int);
   pragma Import (C, Tcl_SetStringObj, "Tcl_SetStringObj");

   --  66

   procedure Tcl_AddErrorInfo
     (interp  : not null Tcl_Interp;
      message : C.Strings.chars_ptr);
   pragma Import (C, Tcl_AddErrorInfo, "Tcl_AddErrorInfo");

   --  67

   procedure Tcl_AddObjErrorInfo
     (interp  : not null Tcl_Interp;
      message : C.Strings.chars_ptr;
      length  : C.int);
   pragma Import (C, Tcl_AddObjErrorInfo, "Tcl_AddObjErrorInfo");

   --  68

   procedure Tcl_AllowExceptions (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_AllowExceptions, "Tcl_AllowExceptions");

   --  69

   procedure Tcl_AppendElement
     (interp : not null Tcl_Interp;
      strng  : C.Strings.chars_ptr);
   pragma Import (C, Tcl_AppendElement, "Tcl_AppendElement");

   --  70

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
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr);

   --  71

   function Tcl_AsyncCreate
     (proc : not null Tcl_AsyncProc;
      data : ClientData)
      return Tcl_AsyncHandler;
   pragma Import (C, Tcl_AsyncCreate, "Tcl_AsyncCreate");

   --  72

   procedure Tcl_AsyncDelete (async : not null Tcl_AsyncHandler);
   pragma Import (C, Tcl_AsyncDelete, "Tcl_AsyncDelete");

   --  73

   function Tcl_AsyncInvoke
     (interp : not null Tcl_Interp;
      code   : C.int)
      return   C.int;
   pragma Import (C, Tcl_AsyncInvoke, "Tcl_AsyncInvoke");

   --  74

   procedure Tcl_AsyncMark (async : not null Tcl_AsyncHandler);
   pragma Import (C, Tcl_AsyncMark, "Tcl_AsyncMark");

   --  75

   function Tcl_AsyncReady return C.int;
   pragma Import (C, Tcl_AsyncReady, "Tcl_AsyncReady");

   --  76

   procedure Tcl_BackgroundError (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_BackgroundError, "Tcl_BackgroundError");

   --  77

   function Tcl_Backslash
     (src     : C.Strings.chars_ptr;
      readPtr : access C.int)                 -- can be null
      return    C.char;
   pragma Import (C, Tcl_Backslash, "Tcl_Backslash");

   --  78

   function Tcl_BadChannelOption
     (interp     : not null Tcl_Interp;
      optionName : C.Strings.chars_ptr;
      optionList : C.Strings.chars_ptr)
      return       C.int;
   pragma Import (C, Tcl_BadChannelOption, "Tcl_BadChannelOption");

   --  79

   procedure Tcl_CallWhenDeleted
     (interp : not null Tcl_Interp;
      proc   : not null Tcl_InterpDeleteProc;
      data   : ClientData);
   pragma Import (C, Tcl_CallWhenDeleted, "Tcl_CallWhenDeleted");

   --  80

   procedure Tcl_CancelIdleCall
     (idleProc : not null Tcl_IdleProc;
      data     : ClientData);
   pragma Import (C, Tcl_CancelIdleCall, "Tcl_CancelIdleCall");

   --  81

   function Tcl_Close
     (interp : not null Tcl_Interp;
      chan   : not null Tcl_Channel)
      return   C.int;
   pragma Import (C, Tcl_Close, "Tcl_Close");

   --  82

   function Tcl_CommandComplete (cmd : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_CommandComplete, "Tcl_CommandComplete");

   --  83

   function Tcl_Concat
     (argc : C.int;
      argv : CArgv.Chars_Ptr_Ptr)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_Concat, "Tcl_Concat");

   --  84

   function Tcl_ConvertElement
     (src   : C.Strings.chars_ptr;
      dst   : C.Strings.chars_ptr;
      flags : C.int)
      return  C.int;
   pragma Import (C, Tcl_ConvertElement, "Tcl_ConvertElement");

   --  85

   function Tcl_ConvertCountedElement
     (src    : C.Strings.chars_ptr;
      length : C.int;
      dst    : C.Strings.chars_ptr;
      flags  : C.int)
      return   C.int;
   pragma Import (C, Tcl_ConvertCountedElement, "Tcl_ConvertCountedElement");

   --  86

   function Tcl_CreateAlias
     (slave     : not null Tcl_Interp;
      slaveCmd  : C.Strings.chars_ptr;
      target    : not null Tcl_Interp;
      targetCmd : C.Strings.chars_ptr;
      argc      : C.int;
      argv      : CArgv.Chars_Ptr_Ptr)
      return      C.int;
   pragma Import (C, Tcl_CreateAlias, "Tcl_CreateAlias");

   --  87

   function Tcl_CreateAliasObj
     (slave     : not null Tcl_Interp;
      slaveCmd  : C.Strings.chars_ptr;
      target    : not null Tcl_Interp;
      targetCmd : C.Strings.chars_ptr;
      objc      : C.int;
      objv      : Tcl_Obj_Array)
      return      C.int;
   pragma Import (C, Tcl_CreateAliasObj, "Tcl_CreateAliasObj");

   --  88

   function Tcl_CreateChannel
     (typePtr      : not null Tcl_ChannelType;
      chanName     : C.Strings.chars_ptr;
      instancedata : ClientData;
      mask         : C.int)
      return         Tcl_Channel;
   pragma Import (C, Tcl_CreateChannel, "Tcl_CreateChannel");

   --  89

   procedure Tcl_CreateChannelHandler
     (chan : not null Tcl_Channel;
      mask : C.int;
      proc : not null Tcl_ChannelProc;
      data : ClientData);
   pragma Import (C, Tcl_CreateChannelHandler, "Tcl_CreateChannelHandler");

   --  90

   procedure Tcl_CreateCloseHandler
     (chan : not null Tcl_Channel;
      proc : not null Tcl_CloseProc;
      data : ClientData);
   pragma Import (C, Tcl_CreateCloseHandler, "Tcl_CreateCloseHandler");

   --  91

   function Tcl_CreateCommand
     (interp     : not null Tcl_Interp;
      cmdName    : C.Strings.chars_ptr;
      proc       : not null Tcl_CmdProc;
      data       : ClientData;
      deleteProc : Tcl_CmdDeleteProc)    -- can be null
      return       Tcl_Command;
   pragma Import (C, Tcl_CreateCommand, "Tcl_CreateCommand");

   --  92

   procedure Tcl_CreateEventSource
     (setupProc : not null Tcl_EventSetupProc;
      checkProc : not null Tcl_EventCheckProc;
      data      : ClientData);
   pragma Import (C, Tcl_CreateEventSource, "Tcl_CreateEventSource");

   --  93

   procedure Tcl_CreateExitHandler
     (proc : not null Tcl_ExitProc;
      data : ClientData);
   pragma Import (C, Tcl_CreateExitHandler, "Tcl_CreateExitHandler");

   --  94

   function Tcl_CreateInterp return Tcl_Interp;
   pragma Import (C, Tcl_CreateInterp, "Tcl_CreateInterp");

   --  95

   procedure Tcl_CreateMathFunc
     (interp   : not null Tcl_Interp;
      name     : C.Strings.chars_ptr;
      numArgs  : C.int;
      argTypes : Tcl_ValueType;
      proc     : not null Tcl_MathProc;
      data     : ClientData);
   pragma Import (C, Tcl_CreateMathFunc, "Tcl_CreateMathFunc");

   --  96

   function Tcl_CreateObjCommand
     (interp     : not null Tcl_Interp;
      cmdName    : C.Strings.chars_ptr;
      proc       : not null Tcl_ObjCmdProc;
      data       : ClientData;
      deleteProc : Tcl_CmdDeleteProc)    -- can be null
      return       Tcl_Command;
   pragma Import (C, Tcl_CreateObjCommand, "Tcl_CreateObjCommand");

   --  97

   function Tcl_CreateSlave
     (interp    : not null Tcl_Interp;
      slaveName : C.Strings.chars_ptr;
      isSafe    : C.int)
      return      Tcl_Interp;
   pragma Import (C, Tcl_CreateSlave, "Tcl_CreateSlave");

   --  98

   function Tcl_CreateTimerHandler
     (milliseconds : C.int;
      proc         : not null Tcl_TimerProc;
      data         : ClientData)
      return         Tcl_TimerToken;
   pragma Import (C, Tcl_CreateTimerHandler, "Tcl_CreateTimerHandler");

   --  99

   function Tcl_CreateTrace
     (interp : not null Tcl_Interp;
      level  : C.int;
      proc   : not null Tcl_CmdTraceProc;
      data   : ClientData)
      return   Tcl_Trace;
   pragma Import (C, Tcl_CreateTrace, "Tcl_CreateTrace");

   --  100

   procedure Tcl_DeleteAssocData
     (interp : not null Tcl_Interp;
      name   : C.Strings.chars_ptr);
   pragma Import (C, Tcl_DeleteAssocData, "Tcl_DeleteAssocData");

   --  101

   procedure Tcl_DeleteChannelHandler
     (chan : not null Tcl_Channel;
      proc : not null Tcl_ChannelProc;
      data : ClientData);
   pragma Import (C, Tcl_DeleteChannelHandler, "Tcl_DeleteChannelHandler");

   --  102

   procedure Tcl_DeleteCloseHandler
     (chan : not null Tcl_Channel;
      proc : not null Tcl_CloseProc;
      data : ClientData);
   pragma Import (C, Tcl_DeleteCloseHandler, "Tcl_DeleteCloseHandler");

   --  103

   function Tcl_DeleteCommand
     (interp  : not null Tcl_Interp;
      cmdName : C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_DeleteCommand, "Tcl_DeleteCommand");

   --  104

   function Tcl_DeleteCommandFromToken
     (interp  : not null Tcl_Interp;
      command : not null Tcl_Command)
      return    C.int;
   pragma Import
     (C,
      Tcl_DeleteCommandFromToken,
      "Tcl_DeleteCommandFromToken");

   --  105

   procedure Tcl_DeleteEvents
     (proc : not null Tcl_EventDeleteProc;
      data : ClientData);
   pragma Import (C, Tcl_DeleteEvents, "Tcl_DeleteEvents");

   --  106

   procedure Tcl_DeleteEventSource
     (setupProc : not null Tcl_EventSetupProc;
      checkProc : not null Tcl_EventCheckProc;
      data      : ClientData);
   pragma Import (C, Tcl_DeleteEventSource, "Tcl_DeleteEventSource");

   --  107

   procedure Tcl_DeleteExitHandler
     (proc : not null Tcl_ExitProc;
      data : ClientData);
   pragma Import (C, Tcl_DeleteExitHandler, "Tcl_DeleteExitHandler");

   --  108

   procedure Tcl_DeleteHashEntry (entryPtr : not null Tcl_HashEntry);
   pragma Import (C, Tcl_DeleteHashEntry, "Tcl_DeleteHashEntry");

   --  109

   procedure Tcl_DeleteHashTable (tablePtr : not null Tcl_HashTable);
   pragma Import (C, Tcl_DeleteHashTable, "Tcl_DeleteHashTable");

   --  110

   procedure Tcl_DeleteInterp (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_DeleteInterp, "Tcl_DeleteInterp");

   --  UNIX

   --  111

   procedure Tcl_DetachPids (numPids : C.int; pidPtr : not null Tcl_Pid);
   pragma Import (C, Tcl_DetachPids, "Tcl_DetachPids");

   --  UNIX

   --  __WIN32__

   --  112

   procedure Tcl_DeleteTimerHandler (token : not null Tcl_TimerToken);
   pragma Import (C, Tcl_DeleteTimerHandler, "Tcl_DeleteTimerHandler");

   --  113

   procedure Tcl_DeleteTrace (interp : not null Tcl_Interp;
                              trace : not null Tcl_Trace);
   pragma Import (C, Tcl_DeleteTrace, "Tcl_DeleteTrace");

   --  114

   procedure Tcl_DontCallWhenDeleted
     (interp : not null Tcl_Interp;
      proc   : not null Tcl_InterpDeleteProc;
      data   : ClientData);
   pragma Import (C, Tcl_DontCallWhenDeleted, "Tcl_DontCallWhenDeleted");

   --  115

   function Tcl_DoOneEvent (flags : C.int) return C.int;
   pragma Import (C, Tcl_DoOneEvent, "Tcl_DoOneEvent");

   --  116

   procedure Tcl_DoWhenIdle (proc : not null Tcl_IdleProc;
                             data : ClientData);
   pragma Import (C, Tcl_DoWhenIdle, "Tcl_DoWhenIdle");

   --  117

   function Tcl_DStringAppend
     (dsPtr  : not null Tcl_DString;
      str    : C.Strings.chars_ptr;
      length : C.int)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_DStringAppend, "Tcl_DStringAppend");

   --  118

   function Tcl_DStringAppendElement
     (dsPtr : not null Tcl_DString;
      strng : C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_DStringAppendElement, "Tcl_DStringAppendElement");

   --  119

   procedure Tcl_DStringEndSublist (dsPtr : not null Tcl_DString);
   pragma Import (C, Tcl_DStringEndSublist, "Tcl_DStringEndSublist");

   --  120

   procedure Tcl_DStringFree (dsPtr : not null Tcl_DString);
   pragma Import (C, Tcl_DStringFree, "Tcl_DStringFree");

   --  121

   procedure Tcl_DStringGetResult
     (interp : not null Tcl_Interp;
      dsPtr  : not null Tcl_DString);
   pragma Import (C, Tcl_DStringGetResult, "Tcl_DStringGetResult");

   --  122

   procedure Tcl_DStringInit (dsPtr : not null Tcl_DString);
   pragma Import (C, Tcl_DStringInit, "Tcl_DStringInit");

   --  123

   procedure Tcl_DStringResult
     (interp : not null Tcl_Interp;
      dsPtr  : not null Tcl_DString);
   pragma Import (C, Tcl_DStringResult, "Tcl_DStringResult");

   --  124

   --  125

   procedure Tcl_DStringStartSublist (dsPtr : not null Tcl_DString);
   pragma Import (C, Tcl_DStringStartSublist, "Tcl_DStringStartSublist");

   --  126

   function Tcl_Eof (chan : not null Tcl_Channel) return C.int;
   pragma Import (C, Tcl_Eof, "Tcl_Eof");

   --  127

   function Tcl_ErrnoId return C.Strings.chars_ptr;
   pragma Import (C, Tcl_ErrnoId, "Tcl_ErrnoId");

   --  128

   function Tcl_ErrnoMsg (err : C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_ErrnoMsg, "Tcl_ErrnoMsg");

   --  129

   function Tcl_Eval
     (interp : not null Tcl_Interp;
      strng  : C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_Eval, "Tcl_Eval");

   --  130

   function Tcl_EvalFile
     (interp   : not null Tcl_Interp;
      fileName : C.Strings.chars_ptr)
      return     C.int;
   pragma Import (C, Tcl_EvalFile, "Tcl_EvalFile");

   --  131

   function Tcl_EvalObj
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_EvalObj, "Tcl_EvalObj");

   --  132

   procedure Tcl_EventuallyFree
     (data     : ClientData;
      freeProc : not null Tcl_FreeProc);
   pragma Import (C, Tcl_EventuallyFree, "Tcl_EventuallyFree");

   --  133

   procedure Tcl_Exit (status : C.int);
   pragma Import (C, Tcl_Exit, "Tcl_Exit");

   --  134

   function Tcl_ExposeCommand
     (interp         : not null Tcl_Interp;
      hiddenCmdToken : C.Strings.chars_ptr;
      cmdName        : C.Strings.chars_ptr)
      return           C.int;
   pragma Import (C, Tcl_ExposeCommand, "Tcl_ExposeCommand");

   --  135

   function Tcl_ExprBoolean
     (interp : not null Tcl_Interp;
      str    : C.Strings.chars_ptr;
      ptr    : not null access C.int)
      return   C.int;
   pragma Import (C, Tcl_ExprBoolean, "Tcl_ExprBoolean");

   --  136

   function Tcl_ExprBooleanObj
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj;
      ptr    : not null access C.int)
      return   C.int;
   pragma Import (C, Tcl_ExprBooleanObj, "Tcl_ExprBooleanObj");

   --  137

   function Tcl_ExprDouble
     (interp : not null Tcl_Interp;
      str    : C.Strings.chars_ptr;
      ptr    : not null access C.double)
      return   C.int;
   pragma Import (C, Tcl_ExprDouble, "Tcl_ExprDouble");

   --  138

   function Tcl_ExprDoubleObj
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj;
      ptr    : not null access C.double)
      return   C.int;
   pragma Import (C, Tcl_ExprDoubleObj, "Tcl_ExprDoubleObj");

   --  139

   function Tcl_ExprLong
     (interp : not null Tcl_Interp;
      str    : C.Strings.chars_ptr;
      ptr    : not null access C.long)
      return   C.int;
   pragma Import (C, Tcl_ExprLong, "Tcl_ExprLong");

   --  140

   function Tcl_ExprLongObj
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj;
      ptr    : not null access C.long)
      return   C.int;
   pragma Import (C, Tcl_ExprLongObj, "Tcl_ExprLongObj");

   --  141

   function Tcl_ExprObj
     (interp       : not null Tcl_Interp;
      objPtr       : not null Tcl_Obj;
      resultPtrPtr : not null access Tcl_Obj)
      return         C.int;
   pragma Import (C, Tcl_ExprObj, "Tcl_ExprObj");

   --  142

   function Tcl_ExprString
     (interp : not null Tcl_Interp;
      strng  : C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_ExprString, "Tcl_ExprString");

   --  143

   procedure Tcl_Finalize;
   pragma Import (C, Tcl_Finalize, "Tcl_Finalize");

   --  144

   procedure Tcl_FindExecutable (argv0 : C.Strings.chars_ptr);
   pragma Import (C, Tcl_FindExecutable, "Tcl_FindExecutable");

   --  145

   function Tcl_FirstHashEntry
     (tablePtr  : not null Tcl_HashTable;
      searchPtr : not null Tcl_HashSearch)
      return      Tcl_HashEntry;
   pragma Import (C, Tcl_FirstHashEntry, "Tcl_FirstHashEntry");

   --  146

   function Tcl_Flush (chan : not null Tcl_Channel) return C.int;
   pragma Import (C, Tcl_Flush, "Tcl_Flush");

   --  147

   procedure Tcl_FreeResult (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_FreeResult, "Tcl_FreeResult");

   --  148

   function Tcl_GetAlias
     (interp          : not null Tcl_Interp;
      slaveCmd        : C.Strings.chars_ptr;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : CArgv.Chars_Ptr_Ptr;
      argcPtr         : not null access C.int;
      argvPtr         : not null access CArgv.Chars_Ptr_Ptr)
      return            C.int;
   pragma Import (C, Tcl_GetAlias, "Tcl_GetAlias");

   --  149

   function Tcl_GetAliasObj
     (interp          : not null Tcl_Interp;
      slaveCmd        : C.Strings.chars_ptr;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : CArgv.Chars_Ptr_Ptr;
      objcPtr         : not null access C.int;
      objv            : Tcl_Obj_Array)
      return            C.int;
   pragma Import (C, Tcl_GetAliasObj, "Tcl_GetAliasObj");

   --  150

   function Tcl_GetAssocData
     (interp  : not null Tcl_Interp;
      name    : C.Strings.chars_ptr;
      procPtr : access Tcl_InterpDeleteProc)    -- can be null
      return    ClientData;
   pragma Import (C, Tcl_GetAssocData, "Tcl_GetAssocData");

   --  151

   function Tcl_GetChannel
     (interp   : not null Tcl_Interp;
      chanName : C.Strings.chars_ptr;
      modePtr  : not null access C.int)
      return     Tcl_Channel;
   pragma Import (C, Tcl_GetChannel, "Tcl_GetChannel");

   --  152

   function Tcl_GetChannelBufferSize
     (chan : not null Tcl_Channel) return C.int;
   pragma Import (C, Tcl_GetChannelBufferSize, "Tcl_GetChannelBufferSize");

   --  153

   function Tcl_GetChannelHandle
     (chan      : not null Tcl_Channel;
      direction : C.int;
      handleptr : ClientData)
      return      C.int;
   pragma Import (C, Tcl_GetChannelHandle, "Tcl_GetChannelHandle");

   --  154

   function Tcl_GetChannelInstanceData
     (chan : not null Tcl_Channel)
      return ClientData;
   pragma Import
     (C,
      Tcl_GetChannelInstanceData,
      "Tcl_GetChannelInstanceData");

   --  155

   function Tcl_GetChannelMode (chan : not null Tcl_Channel) return C.int;
   pragma Import (C, Tcl_GetChannelMode, "Tcl_GetChannelMode");

   --  156

   function Tcl_GetChannelName
     (chan : not null Tcl_Channel)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetChannelName, "Tcl_GetChannelName");

   --  157

   function Tcl_GetChannelOption
     (interp     : not null Tcl_Interp;
      chan       : not null Tcl_Channel;
      optionName : C.Strings.chars_ptr;
      dsPtr      : not null Tcl_DString)
      return       C.int;
   pragma Import (C, Tcl_GetChannelOption, "Tcl_GetChannelOption");

   --  158

   function Tcl_GetChannelType
     (chan : not null Tcl_Channel)
      return Tcl_ChannelType;
   pragma Import (C, Tcl_GetChannelType, "Tcl_GetChannelType");

   --  159

   function Tcl_GetCommandInfo
     (interp  : not null Tcl_Interp;
      cmdName : C.Strings.chars_ptr;
      infoPtr : not null Tcl_CmdInfo)
      return    C.int;
   pragma Import (C, Tcl_GetCommandInfo, "Tcl_GetCommandInfo");

   --  160

   function Tcl_GetCommandName
     (interp  : not null Tcl_Interp;
      command : not null Tcl_Command)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetCommandName, "Tcl_GetCommandName");

   --  161

   function Tcl_GetErrno return C.int;
   pragma Import (C, Tcl_GetErrno, "Tcl_GetErrno");

   --  162

   function Tcl_GetHostName return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetHostName, "Tcl_GetHostName");

   --  163

   function Tcl_GetInterpPath
     (askInterp   : not null Tcl_Interp;
      slaveInterp : not null Tcl_Interp)
      return        C.int;
   pragma Import (C, Tcl_GetInterpPath, "Tcl_GetInterpPath");

   --  164

   function Tcl_GetMaster (interp : not null Tcl_Interp) return Tcl_Interp;
   pragma Import (C, Tcl_GetMaster, "Tcl_GetMaster");

   --  165

   function Tcl_GetNameOfExecutable return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetNameOfExecutable, "Tcl_GetNameOfExecutable");

   --  166

   function Tcl_GetObjResult (interp : Tcl_Interp) return Tcl_Obj;
   pragma Import (C, Tcl_GetObjResult, "Tcl_GetObjResult");

   --  UNIX

   --  167

   procedure Tcl_GetOpenFile
     (interp     : not null Tcl_Interp;
      str        : C.Strings.chars_ptr;
      forWriting : C.int;
      checkUsage : C.int;
      fileptr    : ClientData);
   pragma Import (C, Tcl_GetOpenFile, "Tcl_GetOpenFile");

   --  UNIX

   --  168

   function Tcl_GetPathType
     (path : C.Strings.chars_ptr)
      return Tcl_PathType;
   pragma Import (C, Tcl_GetPathType, "Tcl_GetPathType");

   function Tcl_GetRefCount (objPtr : Tcl_Obj) return C.int;

   function Tcl_GetResult
     (interp : not null Tcl_Interp)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetResult, "Tcl_GetStringResult");

   --  169

   function Tcl_Gets
     (chan  : not null Tcl_Channel;
      dsPtr : not null Tcl_DString)
      return  C.int;
   pragma Import (C, Tcl_Gets, "Tcl_Gets");

   --  170

   function Tcl_GetsObj
     (chan   : not null Tcl_Channel;
      objPtr : not null Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_GetsObj, "Tcl_GetsObj");

   --  171

   function Tcl_GetServiceMode return C.int;
   pragma Import (C, Tcl_GetServiceMode, "Tcl_GetServiceMode");

   --  172

   function Tcl_GetSlave
     (interp    : not null Tcl_Interp;
      slaveName : C.Strings.chars_ptr)
      return      Tcl_Interp;
   pragma Import (C, Tcl_GetSlave, "Tcl_GetSlave");

   --  173

   function Tcl_GetStdChannel (typ : C.int) return Tcl_Channel;
   pragma Import (C, Tcl_GetStdChannel, "Tcl_GetStdChannel");

   --  174

   function Tcl_GetStringResult
     (interp : not null Tcl_Interp)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetStringResult, "Tcl_GetStringResult");

   --  175

   function Tcl_GetVar
     (interp  : not null Tcl_Interp;
      varName : C.Strings.chars_ptr;
      flags   : C.int)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetVar, "Tcl_GetVar");

   --  176

   function Tcl_GetVar2
     (interp : not null Tcl_Interp;
      part1  : C.Strings.chars_ptr;
      part2  : C.Strings.chars_ptr;
      flags  : C.int)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetVar2, "Tcl_GetVar2");

   --  177

   function Tcl_GlobalEval
     (interp  : not null Tcl_Interp;
      command : C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_GlobalEval, "Tcl_GlobalEval");

   --  178

   function Tcl_GlobalEvalObj
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_GlobalEvalObj, "Tcl_GlobalEvalObj");

   --  179

   function Tcl_HideCommand
     (interp         : not null Tcl_Interp;
      cmdName        : C.Strings.chars_ptr;
      hiddenCmdToken : C.Strings.chars_ptr)
      return           C.int;
   pragma Import (C, Tcl_HideCommand, "Tcl_HideCommand");

   --  180

   function Tcl_Init (interp : not null Tcl_Interp) return C.int;
   pragma Import (C, Tcl_Init, "Tcl_Init");

   --  181

   procedure Tcl_InitHashTable
     (tablePtr : not null Tcl_HashTable;
      keyType  : C.int);
   pragma Import (C, Tcl_InitHashTable, "Tcl_InitHashTable");

   --  182

   function Tcl_InputBlocked (chan : not null Tcl_Channel) return C.int;
   pragma Import (C, Tcl_InputBlocked, "Tcl_InputBlocked");

   --  183

   function Tcl_InputBuffered (chan : not null Tcl_Channel) return C.int;
   pragma Import (C, Tcl_InputBuffered, "Tcl_InputBuffered");

   --  184

   function Tcl_InterpDeleted (interp : not null Tcl_Interp) return C.int;
   pragma Import (C, Tcl_InterpDeleted, "Tcl_InterpDeleted");

   --  185

   function Tcl_IsSafe (interp : not null Tcl_Interp) return C.int;
   pragma Import (C, Tcl_IsSafe, "Tcl_IsSafe");

   --  186

   function Tcl_JoinPath
     (argc      : C.int;
      argv      : CArgv.Chars_Ptr_Ptr;
      resultPtr : not null Tcl_DString)
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_JoinPath, "Tcl_JoinPath");

   --  187

   function Tcl_LinkVar
     (interp  : not null Tcl_Interp;
      varName : C.Strings.chars_ptr;
      addr    : System.Address;
      typ     : C.int)
      return    C.int;
   pragma Import (C, Tcl_LinkVar, "Tcl_LinkVar");

   --  Slot 188 is reserved

   --  189

   function Tcl_MakeFileChannel
     (handle : ClientData;
      mode   : C.int)
      return   Tcl_Channel;
   pragma Import (C, Tcl_MakeFileChannel, "Tcl_MakeFileChannel");

   --  190

   function Tcl_MakeSafe (interp : not null Tcl_Interp) return C.int;
   pragma Import (C, Tcl_MakeSafe, "Tcl_MakeSafe");

   --  191

   function Tcl_MakeTcpClientChannel
     (tcpsocket : ClientData)
      return      Tcl_Channel;
   pragma Import (C, Tcl_MakeTcpClientChannel, "Tcl_MakeTcpClientChannel");

   --  192

   function Tcl_Merge
     (argc : C.int;
      argv : CArgv.Chars_Ptr_Ptr)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_Merge, "Tcl_Merge");

   --  193

   function Tcl_NextHashEntry
     (searchPtr : not null Tcl_HashSearch)
      return      Tcl_HashEntry;
   pragma Import (C, Tcl_NextHashEntry, "Tcl_NextHashEntry");

   --  194

   procedure Tcl_NotifyChannel (channel : not null Tcl_Channel;
                                mask : C.int);
   pragma Import (C, Tcl_NotifyChannel, "Tcl_NotifyChannel");

   --  195

   function Tcl_ObjGetVar2
     (interp   : not null Tcl_Interp;
      part1Ptr : not null Tcl_Obj;
      part2Ptr : not null Tcl_Obj;
      flags    : C.int)
      return     Tcl_Obj;
   pragma Import (C, Tcl_ObjGetVar2, "Tcl_ObjGetVar2");

   --  196

   function Tcl_ObjSetVar2
     (interp      : not null Tcl_Interp;
      part1Ptr    : not null Tcl_Obj;
      part2Ptr    : not null Tcl_Obj;
      newValuePtr : not null Tcl_Obj;
      flags       : C.int)
      return        Tcl_Obj;
   pragma Import (C, Tcl_ObjSetVar2, "Tcl_ObjSetVar2");

   --  UNIX

   --  197

   function Tcl_OpenCommandChannel
     (interp : not null Tcl_Interp;
      argc   : C.int;
      argv   : CArgv.Chars_Ptr_Ptr;
      flags  : C.int)
      return   Tcl_Channel;
   pragma Import (C, Tcl_OpenCommandChannel, "Tcl_OpenCommandChannel");

   --  UNIX

   --  __WIN32__

   --  198

   function Tcl_OpenFileChannel
     (interp      : not null Tcl_Interp;
      fileName    : C.Strings.chars_ptr;
      modeString  : C.Strings.chars_ptr;
      permissions : C.int)
      return        Tcl_Channel;
   pragma Import (C, Tcl_OpenFileChannel, "Tcl_OpenFileChannel");

   --  199

   function Tcl_OpenTcpClient
     (interp  : not null Tcl_Interp;
      port    : C.int;
      address : System.Address;
      myaddr  : System.Address;
      myport  : C.int;
      async   : C.int)
      return    Tcl_Channel;
   pragma Import (C, Tcl_OpenTcpClient, "Tcl_OpenTcpClient");

   --  200

   function Tcl_OpenTcpServer
     (interp       : not null Tcl_Interp;
      port         : C.int;
      host         : C.Strings.chars_ptr;
      acceptProc   : not null Tcl_TcpAcceptProc;
      callbackdata : ClientData)
      return         Tcl_Channel;
   pragma Import (C, Tcl_OpenTcpServer, "Tcl_OpenTcpServer");

   --  201

   procedure Tcl_Preserve (data : ClientData);
   pragma Import (C, Tcl_Preserve, "Tcl_Preserve");

   --  202

   procedure Tcl_PrintDouble
     (interp : not null Tcl_Interp;
      value  : C.double;
      dst    : C.Strings.chars_ptr);
   pragma Import (C, Tcl_PrintDouble, "Tcl_PrintDouble");

   procedure Tcl_PrintObj (Ptr : Tcl_Obj);

   --  203

   function Tcl_PutEnv (strng : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_PutEnv, "Tcl_PutEnv");

   --  204

   function Tcl_PosixError
     (interp : not null Tcl_Interp)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_PosixError, "Tcl_PosixError");

   --  205

   procedure Tcl_QueueEvent
     (evPtr    : not null Tcl_Event;
      position : Tcl_QueuePosition);
   pragma Import (C, Tcl_QueueEvent, "Tcl_QueueEvent");

   --  206

   function Tcl_Read
     (chan   : not null Tcl_Channel;
      bufPtr : C.Strings.chars_ptr;
      toRead : C.int)
      return   C.int;
   pragma Import (C, Tcl_Read, "Tcl_Read");

   --  UNIX

   --  207

   procedure Tcl_ReapDetachedProcs;
   pragma Import (C, Tcl_ReapDetachedProcs, "Tcl_ReapDetachedProcs");

   --  UNIX

   --  __WIN32__

   --  208

   function Tcl_RecordAndEval
     (interp : not null Tcl_Interp;
      cmd    : C.Strings.chars_ptr;
      flags  : C.int)
      return   C.int;
   pragma Import (C, Tcl_RecordAndEval, "Tcl_RecordAndEval");

   --  209

   function Tcl_RecordAndEvalObj
     (interp : not null Tcl_Interp;
      cmdPtr : not null Tcl_Obj;
      flags  : C.int)
      return   C.int;
   pragma Import (C, Tcl_RecordAndEvalObj, "Tcl_RecordAndEvalObj");

   --  210

   procedure Tcl_RegisterChannel
     (interp : not null Tcl_Interp;
      chan   : not null Tcl_Channel);
   pragma Import (C, Tcl_RegisterChannel, "Tcl_RegisterChannel");

   --  211

   procedure Tcl_RegisterObjType (typePtr : not null Tcl_ObjType);
   pragma Import (C, Tcl_RegisterObjType, "Tcl_RegisterObjType");

   --  212

   function Tcl_RegExpCompile
     (interp : not null Tcl_Interp;
      strng  : C.Strings.chars_ptr)
      return   Tcl_RegExp;
   pragma Import (C, Tcl_RegExpCompile, "Tcl_RegExpCompile");

   --  213

   function Tcl_RegExpExec
     (interp : not null Tcl_Interp;
      regexp : not null Tcl_RegExp;
      str    : C.Strings.chars_ptr;
      start  : C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_RegExpExec, "Tcl_RegExpExec");

   --  214

   function Tcl_RegExpMatch
     (interp  : not null Tcl_Interp;
      str     : C.Strings.chars_ptr;
      pattern : C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_RegExpMatch, "Tcl_RegExpMatch");

   --  215

   procedure Tcl_RegExpRange
     (regexp   : not null Tcl_RegExp;
      index    : C.int;
      startPtr : CArgv.Chars_Ptr_Ptr;
      endPtr   : CArgv.Chars_Ptr_Ptr);
   pragma Import (C, Tcl_RegExpRange, "Tcl_RegExpRange");

   --  216

   procedure Tcl_Release (data : ClientData);
   pragma Import (C, Tcl_Release, "Tcl_Release");

   --  217

   procedure Tcl_ResetResult (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_ResetResult, "Tcl_ResetResult");

   --  218

   function Tcl_ScanElement
     (str     : C.Strings.chars_ptr;
      flagPtr : not null access C.int)
      return    C.int;
   pragma Import (C, Tcl_ScanElement, "Tcl_ScanElement");

   --  219

   function Tcl_ScanCountedElement
     (str     : C.Strings.chars_ptr;
      length  : C.int;
      flagPtr : not null access C.int)
      return    C.int;
   pragma Import (C, Tcl_ScanCountedElement, "Tcl_ScanCountedElement");

   --  220

   function Tcl_Seek
     (chan   : not null Tcl_Channel;
      offset : C.int;
      mode   : C.int)
      return   C.int;
   pragma Import (C, Tcl_Seek, "Tcl_Seek");

   --  221

   function Tcl_ServiceAll return C.int;
   pragma Import (C, Tcl_ServiceAll, "Tcl_ServiceAll");

   --  222

   function Tcl_ServiceEvent (flags : C.int) return C.int;
   pragma Import (C, Tcl_ServiceEvent, "Tcl_ServiceEvent");

   --  223

   procedure Tcl_SetAssocData
     (interp : not null Tcl_Interp;
      name   : C.Strings.chars_ptr;
      proc   : not null Tcl_InterpDeleteProc;
      data   : ClientData);
   pragma Import (C, Tcl_SetAssocData, "Tcl_SetAssocData");

   --  224

   procedure Tcl_SetChannelBufferSize
     (chan : not null Tcl_Channel;
      sz   : C.int);
   pragma Import (C, Tcl_SetChannelBufferSize, "Tcl_SetChannelBufferSize");

   --  225

   function Tcl_SetChannelOption
     (interp     : not null Tcl_Interp;
      chan       : not null Tcl_Channel;
      optionName : C.Strings.chars_ptr;
      newValue   : C.Strings.chars_ptr)
      return       C.int;
   pragma Import (C, Tcl_SetChannelOption, "Tcl_SetChannelOption");

   --  226

   function Tcl_SetCommandInfo
     (interp  : not null Tcl_Interp;
      cmdName : C.Strings.chars_ptr;
      infoPtr : not null Tcl_CmdInfo)
      return    C.int;
   pragma Import (C, Tcl_SetCommandInfo, "Tcl_SetCommandInfo");

   --  227

   procedure Tcl_SetErrno (err : C.int);
   pragma Import (C, Tcl_SetErrno, "Tcl_SetErrno");

   --  228

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
      String9 : C.Strings.chars_ptr := C.Strings.Null_Ptr);

   --  229

   procedure Tcl_SetMaxBlockTime (timePtr : not null Tcl_Time);
   pragma Import (C, Tcl_SetMaxBlockTime, "Tcl_SetMaxBlockTime");

   --  230

   procedure Tcl_SetPanicProc (panicProc : not null Tcl_PanicProc);
   pragma Import (C, Tcl_SetPanicProc, "Tcl_SetPanicProc");

   --  231

   function Tcl_SetRecursionLimit
     (interp : not null Tcl_Interp;
      depth  : C.int)
      return   C.int;
   pragma Import (C, Tcl_SetRecursionLimit, "Tcl_SetRecursionLimit");

   --  232

   procedure Tcl_SetResult
     (interp   : not null Tcl_Interp;
      str      : C.Strings.chars_ptr;
      freeProc : not null Tcl_FreeProc);

   procedure Tcl_SetResult
     (interp   : not null Tcl_Interp;
      str      : C.Strings.chars_ptr;
      freeProc : C.int);
   pragma Import (C, Tcl_Setresult, "Tcl_SetResult");

   --  233

   function Tcl_SetServiceMode (mode : C.int) return C.int;
   pragma Import (C, Tcl_SetServiceMode, "Tcl_SetServiceMode");

   --  234

   procedure Tcl_SetObjErrorCode
     (interp      : not null Tcl_Interp;
      errorObjPtr : not null Tcl_Obj);
   pragma Import (C, Tcl_SetObjErrorCode, "Tcl_SetObjErrorCode");

   --  235

   procedure Tcl_SetObjResult
     (interp       : not null Tcl_Interp;
      resultObjPtr : not null Tcl_Obj);
   pragma Import (C, Tcl_SetObjResult, "Tcl_SetObjResult");

   --  236

   procedure Tcl_SetStdChannel (channel : not null Tcl_Channel;
                                typ : C.int);
   pragma Import (C, Tcl_SetStdChannel, "Tcl_SetStdChannel");

   --  237

   function Tcl_SetVar
     (interp   : not null Tcl_Interp;
      varName  : C.Strings.chars_ptr;
      newValue : C.Strings.chars_ptr;
      flags    : C.int)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_SetVar, "Tcl_SetVar");

   --  238

   function Tcl_SetVar2
     (interp   : not null Tcl_Interp;
      part1    : C.Strings.chars_ptr;
      part2    : C.Strings.chars_ptr;
      newValue : C.Strings.chars_ptr;
      flags    : C.int)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_SetVar2, "Tcl_SetVar2");

   --  239

   function Tcl_SignalId (sig : C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_SignalId, "Tcl_SignalId");

   --  240

   function Tcl_SignalMsg (sig : C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_SignalMsg, "Tcl_SignalMsg");

   --  241

   procedure Tcl_SourceRCFile (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_SourceRCFile, "Tcl_SourceRCFile");

   --  242

   function Tcl_SplitList
     (interp  : not null Tcl_Interp;
      listStr : C.Strings.chars_ptr;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr)
      return    C.int;
   pragma Import (C, Tcl_SplitList, "Tcl_SplitList");

   --  243

   procedure Tcl_SplitPath
     (path    : C.Strings.chars_ptr;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr);
   pragma Import (C, Tcl_SplitPath, "Tcl_SplitPath");

   --  244

   procedure Tcl_StaticPackage
     (interp       : Tcl_Interp;               -- can be null
      pkgName      : C.Strings.chars_ptr;
      initProc     : not null Tcl_PackageInitProc;
      safeInitProc : Tcl_PackageInitProc);     -- can be null
   pragma Import (C, Tcl_StaticPackage, "Tcl_StaticPackage");

   --  245

   function Tcl_StringMatch
     (str     : C.Strings.chars_ptr;
      pattern : C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_StringMatch, "Tcl_StringMatch");

   --  246

   function Tcl_Tell (chan : not null Tcl_Channel) return C.int;
   pragma Import (C, Tcl_Tell, "Tcl_Tell");

   --  247

   procedure Tcl_TraceVar
     (interp  : not null Tcl_Interp;
      varName : C.Strings.chars_ptr;
      flags   : C.int;
      proc    : not null Tcl_VarTraceProc;
      data    : ClientData);
   pragma Import (C, Tcl_TraceVar, "Tcl_TraceVar");

   --  248

   procedure Tcl_TraceVar2
     (interp : not null Tcl_Interp;
      part1  : C.Strings.chars_ptr;
      part2  : C.Strings.chars_ptr;
      flags  : C.int;
      proc   : not null Tcl_VarTraceProc;
      data   : ClientData);
   pragma Import (C, Tcl_TraceVar2, "Tcl_TraceVar2");

   --  249

   --  250

   function Tcl_Ungets
     (chan   : not null Tcl_Channel;
      str    : C.Strings.chars_ptr;
      len    : C.int;
      atHead : C.int)
      return   C.int;
   pragma Import (C, Tcl_Ungets, "Tcl_Ungets");

   --  251

   procedure Tcl_UnlinkVar
     (interp  : not null Tcl_Interp;
      varName : C.Strings.chars_ptr);
   pragma Import (C, Tcl_UnlinkVar, "Tcl_UnlinkVar");

   --  252

   function Tcl_UnregisterChannel
     (interp : not null Tcl_Interp;
      chan   : not null Tcl_Channel)
      return   C.int;
   pragma Import (C, Tcl_UnregisterChannel, "Tcl_UnregisterChannel");

   --  253

   function Tcl_UnsetVar
     (interp  : not null Tcl_Interp;
      varName : C.Strings.chars_ptr;
      flags   : C.int)
      return    C.int;
   pragma Import (C, Tcl_UnsetVar, "Tcl_UnsetVar");

   --  254

   function Tcl_UnsetVar2
     (interp : not null Tcl_Interp;
      part1  : C.Strings.chars_ptr;
      part2  : C.Strings.chars_ptr;
      flags  : C.int)
      return   C.int;
   pragma Import (C, Tcl_UnsetVar2, "Tcl_UnsetVar2");

   --  255

   procedure Tcl_UntraceVar
     (interp  : not null Tcl_Interp;
      varName : C.Strings.chars_ptr;
      flags   : C.int;
      proc    : not null Tcl_VarTraceProc;
      data    : ClientData);
   pragma Import (C, Tcl_UntraceVar, "Tcl_UntraceVar");

   --  256

   procedure Tcl_UntraceVar2
     (interp : not null Tcl_Interp;
      part1  : C.Strings.chars_ptr;
      part2  : C.Strings.chars_ptr;
      flags  : C.int;
      proc   : not null Tcl_VarTraceProc;
      data   : ClientData);
   pragma Import (C, Tcl_UntraceVar2, "Tcl_UntraceVar2");

   --  257

   procedure Tcl_UpdateLinkedVar
     (interp  : not null Tcl_Interp;
      varName : C.Strings.chars_ptr);
   pragma Import (C, Tcl_UpdateLinkedVar, "Tcl_UpdateLinkedVar");

   --  258

   function Tcl_UpVar
     (interp    : not null Tcl_Interp;
      frameName : C.Strings.chars_ptr;
      varName   : C.Strings.chars_ptr;
      localName : C.Strings.chars_ptr;
      flags     : C.int)
      return      C.int;
   pragma Import (C, Tcl_UpVar, "Tcl_UpVar");

   --  259

   function Tcl_UpVar2
     (interp    : not null Tcl_Interp;
      frameName : C.Strings.chars_ptr;
      part1     : C.Strings.chars_ptr;
      part2     : C.Strings.chars_ptr;
      localName : C.Strings.chars_ptr;
      flags     : C.int)
      return      C.int;
   pragma Import (C, Tcl_UpVar2, "Tcl_UpVar2");

   --  260

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
      return    C.int;

   --  261

   function Tcl_VarTraceInfo
     (interp         : not null Tcl_Interp;
      varName        : C.Strings.chars_ptr;
      flags          : C.int;
      procPtr        : not null Tcl_VarTraceProc;
      prevclientdata : ClientData)
      return           ClientData;
   pragma Import (C, Tcl_VarTraceInfo, "Tcl_VarTraceInfo");

   --  262

   function Tcl_VarTraceInfo2
     (interp         : not null Tcl_Interp;
      part1          : C.Strings.chars_ptr;
      part2          : C.Strings.chars_ptr;
      flags          : C.int;
      procPtr        : not null Tcl_VarTraceProc;
      prevclientdata : ClientData)
      return           ClientData;
   pragma Import (C, Tcl_VarTraceInfo2, "Tcl_VarTraceInfo2");

   --  263

   function Tcl_Write
     (chan : not null Tcl_Channel;
      s    : C.Strings.chars_ptr;
      slen : C.int)
      return C.int;
   pragma Import (C, Tcl_Write, "Tcl_Write");

   --  264

   procedure Tcl_WrongNumArgs
     (interp  : not null Tcl_Interp;
      objc    : C.int;
      objv    : Tcl_Obj_Array;
      message : C.Strings.chars_ptr);
   pragma Import (C, Tcl_WrongNumArgs, "Tcl_WrongNumArgs");

   --  265

   function Tcl_DumpActiveMemory
     (fileName : C.Strings.chars_ptr)
      return     C.int;
   pragma Import (C, Tcl_DumpActiveMemory, "Tcl_DumpActiveMemory");

   --  266

   procedure Tcl_ValidateAllMemory
     (file : C.Strings.chars_ptr;
      line : C.int);
   pragma Import (C, Tcl_ValidateAllMemory, "Tcl_ValidateAllMemory");

   --  267

   --  268

   --  269

   function Tcl_HashStats
     (tablePtr : not null Tcl_HashTable)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_HashStats, "Tcl_HashStats");

   --  270

   function Tcl_ParseVar
     (interp  : not null Tcl_Interp;
      str     : C.Strings.chars_ptr;
      termPtr : CArgv.Chars_Ptr_Ptr)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_ParseVar, "Tcl_ParseVar");

   --  271

   function Tcl_PkgPresent
     (interp  : not null Tcl_Interp;
      name    : C.Strings.chars_ptr;
      version : C.Strings.chars_ptr;
      exact   : C.int)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgPresent, "Tcl_PkgPresent");

   --  272

   function Tcl_PkgPresentEx
     (interp        : not null Tcl_Interp;
      name          : C.Strings.chars_ptr;
      version       : C.Strings.chars_ptr;
      exact         : C.int;
      clientdataptr : access ClientData)    -- can be null
      return          C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgPresentEx, "Tcl_PkgPresentEx");

   --  273

   function Tcl_PkgProvide
     (interp  : not null Tcl_Interp;
      name    : C.Strings.chars_ptr;
      version : C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_PkgProvide, "Tcl_PkgProvide");

   --  274

   function Tcl_PkgRequire
     (interp  : not null Tcl_Interp;
      name    : C.Strings.chars_ptr;
      version : C.Strings.chars_ptr;
      exact   : C.int)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgRequire, "Tcl_PkgRequire");

   --  275

   --  276

   --  277

   function Tcl_WaitPid
     (pid     : not null Tcl_Pid;
      statPtr : not null access C.int;
      options : C.int)
      return    Tcl_Pid;
   pragma Import (C, Tcl_WaitPid, "Tcl_WaitPid");

   --  UNIX

   --  278

   --  UNIX

   --  __WIN32__

   --  279

   procedure Tcl_GetVersion
     (major      : not null access C.int;
      minor      : not null access C.int;
      patchLevel : not null access C.int;
      typ        : not null access C.int);
   pragma Import (C, Tcl_GetVersion, "Tcl_GetVersion");

   --  280

   procedure Tcl_InitMemory (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_InitMemory, "Tcl_InitMemory");

   --  281

   --  XXX outdated?
   function Tcl_StackChannel
     (interp       : not null Tcl_Interp;
      typePtr      : not null Tcl_ChannelType;
      instancedata : ClientData;
      mask         : C.int;
      prevChan     : not null Tcl_Channel)
      return         Tcl_Channel;
   pragma Import (C, Tcl_StackChannel, "Tcl_StackChannel");

   --  282

   procedure Tcl_UnstackChannel
     (interp : not null Tcl_Interp;
      chan   : not null Tcl_Channel);
   pragma Import (C, Tcl_UnstackChannel, "Tcl_UnstackChannel");

   --  283

   function Tcl_GetStackedChannel
     (chan : not null Tcl_Channel)
      return Tcl_Channel;
   pragma Import (C, Tcl_GetStackedChannel, "Tcl_GetStackedChannel");

   --  Slot 284 is reserved

   --  Slot 285 is reserved

   --  286

   procedure Tcl_AppendObjToObj
     (objPtr       : not null Tcl_Obj;
      appendObjPtr : not null Tcl_Obj);
   pragma Import (C, Tcl_AppendObjToObj, "Tcl_AppendObjToObj");

   --  287

   function Tcl_CreateEncoding
     (typePtr : not null Tcl_EncodingType)
      return    Tcl_Encoding;
   pragma Import (C, Tcl_CreateEncoding, "Tcl_CreateEncoding");

   --  288

   procedure Tcl_CreateThreadExitHandler
     (proc : not null Tcl_ExitProc;
      data : ClientData);
   pragma Import
     (C,
      Tcl_CreateThreadExitHandler,
      "Tcl_CreateThreadExitHandler");

   --  289

   procedure Tcl_DeleteThreadExitHandler
     (proc : not null Tcl_ExitProc;
      data : ClientData);
   pragma Import
     (C,
      Tcl_DeleteThreadExitHandler,
      "Tcl_DeleteThreadExitHandler");

   --  290

   procedure Tcl_DiscardResult (statePtr : not null Tcl_SavedResult);
   pragma Import (C, Tcl_DiscardResult, "Tcl_DiscardResult");

   --  291

   function Tcl_EvalEx
     (interp   : not null Tcl_Interp;
      script   : C.Strings.chars_ptr;
      numBytes : C.int;
      flags    : C.int)
      return     C.int;
   pragma Import (C, Tcl_EvalEx, "Tcl_EvalEx");

   --  292

   function Tcl_EvalObjv
     (interp : not null Tcl_Interp;
      objc   : C.int;
      objv   : Tcl_Obj_Array;
      flags  : C.int)
      return   C.int;
   pragma Import (C, Tcl_EvalObjv, "Tcl_EvalObjv");

   --  293

   function Tcl_EvalObjEx
     (interp : not null Tcl_Interp;
      objPtr : not null Tcl_Obj;
      flags  : C.int)
      return   C.int;
   pragma Import (C, Tcl_EvalObjEx, "Tcl_EvalObjEx");

   --  294

   procedure Tcl_ExitThread (status : C.int);
   pragma Import (C, Tcl_ExitThread, "Tcl_ExitThread");

   --  295

   function Tcl_ExternalToUtf
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : C.Strings.chars_ptr;
      srcLen      : C.int;
      flags       : C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : C.Strings.chars_ptr;
      dstLen      : C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int)    -- can be null
      return        C.int;
   pragma Import (C, Tcl_ExternalToUtf, "Tcl_ExternalToUtf");

   --  296

   function Tcl_ExternalToUtfDString
     (encoding : not null Tcl_Encoding;
      src      : C.Strings.chars_ptr;
      srcLen   : C.int;
      dsPtr    : not null Tcl_DString)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_ExternalToUtfDString, "Tcl_ExternalToUtfDString");

   --  297

   procedure Tcl_FinalizeThread;
   pragma Import (C, Tcl_FinalizeThread, "Tcl_FinalizeThread");

   --  298

   procedure Tcl_FinalizeNotifier (data : ClientData);
   pragma Import (C, Tcl_FinalizeNotifier, "Tcl_FinalizeNotifier");

   --  299

   procedure Tcl_FreeEncoding (encoding : not null Tcl_Encoding);
   pragma Import (C, Tcl_FreeEncoding, "Tcl_FreeEncoding");

   --  300

   function Tcl_GetCurrentThread return Tcl_ThreadId;
   pragma Import (C, Tcl_GetCurrentThread, "Tcl_GetCurrentThread");

   --  301

   function Tcl_GetEncoding
     (interp : not null Tcl_Interp;
      name   : C.Strings.chars_ptr)
      return   Tcl_Encoding;
   pragma Import (C, Tcl_GetEncoding, "Tcl_GetEncoding");

   --  302

   function Tcl_GetEncodingName
     (encoding : not null Tcl_Encoding)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetEncodingName, "Tcl_GetEncodingName");

   --  303

   procedure Tcl_GetEncodingNames (interp : not null Tcl_Interp);
   pragma Import (C, Tcl_GetEncodingNames, "Tcl_GetEncodingNames");

   --  304

   function Tcl_GetIndexFromObjStruct
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : CArgv.Chars_Ptr_Ptr;
      offset   : C.int;
      msg      : C.Strings.chars_ptr;
      flags    : C.int;
      indexPtr : not null access C.int)
      return     C.int;
   pragma Import (C, Tcl_GetIndexFromObjStruct, "Tcl_GetIndexFromObjStruct");

   --  305

   procedure Tcl_GetThreadData
     (keyPtr : not null Tcl_ThreadDataKey;
      size   : C.int);
   pragma Import (C, Tcl_GetThreadData, "Tcl_GetThreadData");

   --  306

   function Tcl_GetVar2Ex
     (interp : not null Tcl_Interp;
      part1  : C.Strings.chars_ptr;
      part2  : C.Strings.chars_ptr;
      flags  : C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_GetVar2Ex, "Tcl_GetVar2Ex");

   --  307

   function Tcl_InitNotifier return ClientData;
   pragma Import (C, Tcl_InitNotifier, "Tcl_InitNotifier");

   --  308

   procedure Tcl_MutexLock (mutexPtr : not null Tcl_Mutex);
   pragma Import (C, Tcl_MutexLock, "Tcl_MutexLock");

   --  309

   procedure Tcl_MutexUnlock (mutexPtr : not null Tcl_Mutex);
   pragma Import (C, Tcl_MutexUnlock, "Tcl_MutexUnlock");

   --  310

   procedure Tcl_ConditionNotify (condPtr : not null Tcl_Condition);
   pragma Import (C, Tcl_ConditionNotify, "Tcl_ConditionNotify");

   --  311

   procedure Tcl_ConditionWait
     (condPtr  : not null Tcl_Condition;
      mutexPtr : not null Tcl_Mutex;
      timePtr  : Tcl_Time);
   pragma Import (C, Tcl_ConditionWait, "Tcl_ConditionWait");

   --  312

   function Tcl_NumUtfChars
     (src  : C.Strings.chars_ptr;
      len  : C.int)
      return C.int;
   pragma Import (C, Tcl_NumUtfChars, "Tcl_NumUtfChars");

   --  313

   function Tcl_ReadChars
     (channel     : not null Tcl_Channel;
      objPtr      : not null Tcl_Obj;
      charsToRead : C.int;
      appendFlag  : C.int)
      return        C.int;
   pragma Import (C, Tcl_ReadChars, "Tcl_ReadChars");

   --  314

   procedure Tcl_RestoreResult
     (interp   : not null Tcl_Interp;
      statePtr : not null Tcl_SavedResult);
   pragma Import (C, Tcl_RestoreResult, "Tcl_RestoreResult");

   --  315

   procedure Tcl_SaveResult
     (interp   : not null Tcl_Interp;
      statePtr : not null Tcl_SavedResult);
   pragma Import (C, Tcl_SaveResult, "Tcl_SaveResult");

   --  316

   function Tcl_SetSystemEncoding
     (interp : not null Tcl_Interp;
      name   : C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_SetSystemEncoding, "Tcl_SetSystemEncoding");

   --  317

   function Tcl_SetVar2Ex
     (interp      : not null Tcl_Interp;
      part1       : C.Strings.chars_ptr;
      part2       : C.Strings.chars_ptr;
      newValuePtr : not null Tcl_Obj;
      flags       : C.int)
      return        Tcl_Obj;
   pragma Import (C, Tcl_SetVar2Ex, "Tcl_SetVar2Ex");

   --  318

   procedure Tcl_ThreadAlert (threadId : not null Tcl_ThreadId);
   pragma Import (C, Tcl_ThreadAlert, "Tcl_ThreadAlert");

   --  319

   procedure Tcl_ThreadQueueEvent
     (threadId : not null Tcl_ThreadId;
      evPtr    : not null Tcl_Event;
      position : Tcl_QueuePosition);
   pragma Import (C, Tcl_ThreadQueueEvent, "Tcl_ThreadQueueEvent");

   --  320

   function Tcl_UniCharAtIndex
     (src   : C.Strings.chars_ptr;
      index : C.int)
      return  Tcl_UniChar;
   pragma Import (C, Tcl_UniCharAtIndex, "Tcl_UniCharAtIndex");

   --  321

   function Tcl_UniCharToLower (ch : C.int) return Tcl_UniChar;
   pragma Import (C, Tcl_UniCharToLower, "Tcl_UniCharToLower");

   --  322

   function Tcl_UniCharToTitle (ch : C.int) return Tcl_UniChar;
   pragma Import (C, Tcl_UniCharToTitle, "Tcl_UniCharToTitle");

   --  323

   function Tcl_UniCharToUpper (ch : C.int) return Tcl_UniChar;
   pragma Import (C, Tcl_UniCharToUpper, "Tcl_UniCharToUpper");

   --  324

   function Tcl_UniCharToUtf
     (ch   : C.int;
      buf  : C.Strings.chars_ptr)
      return C.int;
   pragma Import (C, Tcl_UniCharToUtf, "Tcl_UniCharToUtf");

   --  325

   function Tcl_UtfAtIndex
     (src   : C.Strings.chars_ptr;
      index : C.int)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfAtIndex, "Tcl_UtfAtIndex");

   --  326

   function Tcl_UtfCharComplete
     (src  : C.Strings.chars_ptr;
      len  : C.int)
      return C.int;
   pragma Import (C, Tcl_UtfCharComplete, "Tcl_UtfCharComplete");

   --  327

   function Tcl_UtfBackslash
     (src     : C.Strings.chars_ptr;
      readPtr : access C.int;               -- can be null
      dst     : C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_UtfBackslash, "Tcl_UtfBackslash");

   --  328

   function Tcl_UtfFindFirst
     (src  : C.Strings.chars_ptr;
      ch   : C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfFindFirst, "Tcl_UtfFindFirst");

   --  329

   function Tcl_UtfFindLast
     (src  : C.Strings.chars_ptr;
      ch   : C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfFindLast, "Tcl_UtfFindLast");

   --  330

   function Tcl_UtfNext
     (src  : C.Strings.chars_ptr)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfNext, "Tcl_UtfNext");

   --  331

   function Tcl_UtfPrev
     (src   : C.Strings.chars_ptr;
      start : C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfPrev, "Tcl_UtfPrev");

   --  332

   function Tcl_UtfToExternal
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : C.Strings.chars_ptr;
      srcLen      : C.int;
      flags       : C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : C.Strings.chars_ptr;
      dstLen      : C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int)    -- can be null
      return        C.int;
   pragma Import (C, Tcl_UtfToExternal, "Tcl_UtfToExternal");

   --  333

   function Tcl_UtfToExternalDString
     (encoding : not null Tcl_Encoding;
      src      : C.Strings.chars_ptr;
      srcLen   : C.int;
      dsPtr    : not null Tcl_DString)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfToExternalDString, "Tcl_UtfToExternalDString");

   --  334

   function Tcl_UtfToLower (src : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_UtfToLower, "Tcl_UtfToLower");

   --  335

   function Tcl_UtfToTitle (src : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_UtfToTitle, "Tcl_UtfToTitle");

   --  336

   function Tcl_UtfToUniChar
     (src   : C.Strings.chars_ptr;
      chPtr : not null access Tcl_UniChar)
      return  C.int;
   pragma Import (C, Tcl_UtfToUniChar, "Tcl_UtfToUniChar");

   --  337

   function Tcl_UtfToUpper (src : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_UtfToUpper, "Tcl_UtfToUpper");

   --  338

   function Tcl_WriteChars
     (chan   : not null Tcl_Channel;
      src    : C.Strings.chars_ptr;
      srcLen : C.int)
      return   C.int;
   pragma Import (C, Tcl_WriteChars, "Tcl_WriteChars");

   --  339

   function Tcl_WriteObj
     (chan   : not null Tcl_Channel;
      objPtr : not null Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_WriteObj, "Tcl_WriteObj");

   --  340

   function Tcl_GetString
     (objPtr : not null Tcl_Obj) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetString, "Tcl_GetString");

   --  341

   function Tcl_GetDefaultEncodingDir return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetDefaultEncodingDir, "Tcl_GetDefaultEncodingDir");

   --  342

   procedure Tcl_SetDefaultEncodingDir (path : C.Strings.chars_ptr);
   pragma Import (C, Tcl_SetDefaultEncodingDir, "Tcl_SetDefaultEncodingDir");

   --  343

   procedure Tcl_AlertNotifier (data : ClientData);
   pragma Import (C, Tcl_AlertNotifier, "Tcl_AlertNotifier");

   --  344

   procedure Tcl_ServiceModeHook (mode : C.int);
   pragma Import (C, Tcl_ServiceModeHook, "Tcl_ServiceModeHook");

   --  345

   function Tcl_UniCharIsAlnum (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsAlnum, "Tcl_UniCharIsAlnum");

   --  346

   function Tcl_UniCharIsAlpha (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsAlpha, "Tcl_UniCharIsAlpha");

   --  347

   function Tcl_UniCharIsDigit (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsDigit, "Tcl_UniCharIsDigit");

   --  348

   function Tcl_UniCharIsLower (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsLower, "Tcl_UniCharIsLower");

   --  349

   function Tcl_UniCharIsSpace (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsSpace, "Tcl_UniCharIsSpace");

   --  350

   function Tcl_UniCharIsUpper (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsUpper, "Tcl_UniCharIsUpper");

   --  351

   function Tcl_UniCharIsWordChar (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsWordChar, "Tcl_UniCharIsWordChar");

   --  352

   function Tcl_UniCharLen (str : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_UniCharLen, "Tcl_UniCharLen");

   --  353

   function Tcl_UniCharNcmp
     (cs   : C.Strings.chars_ptr;
      ct   : C.Strings.chars_ptr;
      n    : C.unsigned_long)
      return C.int;
   pragma Import (C, Tcl_UniCharNcmp, "Tcl_UniCharNcmp");

   --  354

   function Tcl_UniCharToUtfDString
     (strng    : C.Strings.chars_ptr;
      numChars : C.int;
      dsPtr    : not null Tcl_DString)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_UniCharToUtfDString, "Tcl_UniCharToUtfDString");

   --  355

   function Tcl_UtfToUniCharDString
     (strng  : C.Strings.chars_ptr;
      length : C.int;
      dsPtr  : not null Tcl_DString)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfToUniCharDString, "Tcl_UtfToUniCharDString");

   --  356

   function Tcl_GetRegExpFromObj
     (interp : not null Tcl_Interp;
      patObj : not null Tcl_Obj;
      flags  : C.int)
      return   Tcl_RegExp;
   pragma Import (C, Tcl_GetRegExpFromObj, "Tcl_GetRegExpFromObj");

   --  357

   function Tcl_EvalTokens
     (interp   : not null Tcl_Interp;
      tokenPtr : not null Tcl_Token;
      count    : C.int)
      return     Tcl_Obj;
   pragma Import (C, Tcl_EvalTokens, "Tcl_EvalTokens");

   --  358

   procedure Tcl_FreeParse (parsePtr : not null Tcl_Parse);
   pragma Import (C, Tcl_FreeParse, "Tcl_FreeParse");

   --  359

   procedure Tcl_LogCommandInfo
     (interp  : not null Tcl_Interp;
      script  : C.Strings.chars_ptr;
      command : C.Strings.chars_ptr;
      length  : C.int);
   pragma Import (C, Tcl_LogCommandInfo, "Tcl_LogCommandInfo");

   --  360

   function Tcl_ParseBraces
     (interp   : not null Tcl_Interp;
      strng    : C.Strings.chars_ptr;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int;
      termPtr  : CArgv.Chars_Ptr_Ptr)
      return     C.int;
   pragma Import (C, Tcl_ParseBraces, "Tcl_ParseBraces");

   --  361

   function Tcl_ParseCommand
     (interp   : not null Tcl_Interp;
      strng    : C.Strings.chars_ptr;
      numBytes : C.int;
      nested   : C.int;
      parsePtr : not null Tcl_Parse)
      return     C.int;
   pragma Import (C, Tcl_ParseCommand, "Tcl_ParseCommand");

   --  362

   function Tcl_ParseExpr
     (interp   : not null Tcl_Interp;
      strng    : C.Strings.chars_ptr;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse)
      return     C.int;
   pragma Import (C, Tcl_ParseExpr, "Tcl_ParseExpr");

   --  363

   function Tcl_ParseQuotedString
     (interp   : not null Tcl_Interp;
      strng    : C.Strings.chars_ptr;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int;
      termPtr  : CArgv.Chars_Ptr_Ptr)
      return     C.int;
   pragma Import (C, Tcl_ParseQuotedString, "Tcl_ParseQuotedString");

   --  364

   function Tcl_ParseVarName
     (interp   : not null Tcl_Interp;
      strng    : C.Strings.chars_ptr;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int)
      return     C.int;
   pragma Import (C, Tcl_ParseVarName, "Tcl_ParseVarName");

   --  365

   --  366

   function Tcl_Chdir (dirName : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_Chdir, "Tcl_Chdir");

   --  367

   function Tcl_Access
     (path : C.Strings.chars_ptr;
      mode : C.int)
      return C.int;
   pragma Import (C, Tcl_Access, "Tcl_Access");

   --  368

   function Tcl_Stat (path : C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_Stat, "Tcl_Stat");

   --  369

   function Tcl_UtfNcmp
     (s1   : C.Strings.chars_ptr;
      s2   : C.Strings.chars_ptr;
      n    : C.unsigned_long)
      return C.int;
   pragma Import (C, Tcl_UtfNcmp, "Tcl_UtfNcmp");

   --  370

   function Tcl_UtfNcasecmp
     (s1   : C.Strings.chars_ptr;
      s2   : C.Strings.chars_ptr;
      n    : C.unsigned_long)
      return C.int;
   pragma Import (C, Tcl_UtfNcasecmp, "Tcl_UtfNcasecmp");

   --  371

   function Tcl_StringCaseMatch
     (str     : C.Strings.chars_ptr;
      pattern : C.Strings.chars_ptr;
      nocase  : C.int)
      return    C.int;
   pragma Import (C, Tcl_StringCaseMatch, "Tcl_StringCaseMatch");

   --  372

   function Tcl_UniCharIsControl (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsControl, "Tcl_UniCharIsControl");

   --  373

   function Tcl_UniCharIsGraph (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsGraph, "Tcl_UniCharIsGraph");

   --  374

   function Tcl_UniCharIsPrint (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsPrint, "Tcl_UniCharIsPrint");

   --  375

   function Tcl_UniCharIsPunct (ch : C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsPunct, "Tcl_UniCharIsPunct");

   --  376

   function Tcl_RegExpExecObj
     (interp   : not null Tcl_Interp;
      regexp   : not null Tcl_RegExp;
      objPtr   : not null Tcl_Obj;
      offset   : C.int;
      nmatches : C.int;
      flags    : C.int)
      return     C.int;
   pragma Import (C, Tcl_RegExpExecObj, "Tcl_RegExpExecObj");

   --  377

   procedure Tcl_RegExpGetInfo
     (regexp  : not null Tcl_RegExp;
      infoPtr : not null Tcl_RegExpInfo);
   pragma Import (C, Tcl_RegExpGetInfo, "Tcl_RegExpGetInfo");

   --  378

   function Tcl_NewUnicodeObj
     (unicode  : C.Strings.chars_ptr;
      numChars : C.int)
      return     Tcl_Obj;
   pragma Import (C, Tcl_NewUnicodeObj, "Tcl_NewUnicodeObj");

   --  379

   procedure Tcl_SetUnicodeObj
     (objPtr   : not null Tcl_Obj;
      unicode  : C.Strings.chars_ptr;
      numChars : C.int);
   pragma Import (C, Tcl_SetUnicodeObj, "Tcl_SetUnicodeObj");

   --  380

   function Tcl_GetCharLength (objPtr : not null Tcl_Obj) return C.int;
   pragma Import (C, Tcl_GetCharLength, "Tcl_GetCharLength");

   --  381

   function Tcl_GetUniChar
     (objPtr : not null Tcl_Obj;
      index  : C.int)
      return   Tcl_UniChar;
   pragma Import (C, Tcl_GetUniChar, "Tcl_GetUniChar");

   --  382

   function Tcl_GetUnicode (objPtr : not null Tcl_Obj) return Tcl_UniChar;
   pragma Import (C, Tcl_GetUnicode, "Tcl_GetUnicode");

   --  383

   function Tcl_GetRange
     (objPtr : not null Tcl_Obj;
      first  : C.int;
      last   : C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_GetRange, "Tcl_GetRange");

   --  384

   procedure Tcl_AppendUnicodeToObj
     (objPtr  : not null Tcl_Obj;
      unicode : C.Strings.chars_ptr;
      length  : C.int);
   pragma Import (C, Tcl_AppendUnicodeToObj, "Tcl_AppendUnicodeToObj");

   --  385

   function Tcl_RegExpMatchObj
     (interp     : not null Tcl_Interp;
      stringObj  : not null Tcl_Obj;
      patternObj : not null Tcl_Obj)
      return       C.int;
   pragma Import (C, Tcl_RegExpMatchObj, "Tcl_RegExpMatchObj");

   --  386

   procedure Tcl_SetNotifier (notifierProcPtr : not null Tcl_NotifierProcs);
   pragma Import (C, Tcl_SetNotifier, "Tcl_SetNotifier");

   --  387

   function Tcl_GetAllocMutex return Tcl_Mutex;
   pragma Import (C, Tcl_GetAllocMutex, "Tcl_GetAllocMutex");

   --  388

   function Tcl_GetChannelNames (interp : Tcl_Interp) return C.int;
   pragma Import (C, Tcl_GetChannelNames, "Tcl_GetChannelNames");

   --  389

   function Tcl_GetChannelNamesEx
     (interp  : not null Tcl_Interp;
      pattern : C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_GetChannelNamesEx, "Tcl_GetChannelNamesEx");

   --  390

   function Tcl_ProcObjCmd
     (data   : ClientData;
      interp : not null Tcl_Interp;
      objc   : C.int;
      objv   : Tcl_Obj_Array)
      return   C.int;
   pragma Import (C, Tcl_ProcObjCmd, "Tcl_ProcObjCmd");

   --  defined {USE_TCL_STUBS} && !defined {USE_TCL_STUB_PROCS}

   --  !END!: Do not edit above this line.

   --
   --  Public functions that are not accessible via the stubs table.
   --

   procedure Tcl_Main
     (argc        : C.int;
      argv        : CArgv.Chars_Ptr_Ptr;
      appInitProc : not null Tcl_AppInitProc);
   pragma Import (C, Tcl_Main, "Tcl_Main");

   --
   --  Convenience declaration of Tcl_AppInit for backwards compatibility.
   --  This function is not *implemented * by the tcl library, so the storage
   --  class is neither DLLEXPORT nor DLLIMPORT
   --

   function Tcl_AppInit (interp : not null Tcl_Interp) return C.int;
   pragma Import (C, Tcl_AppInit, "Tcl_AppInit");

   --
   --  end block for C++
   --

private

   type Tcl_AsyncHandler_Rec is null record;

   type Tcl_CallFrame_Rec is new Interfaces.C.char_array
     (0 .. Tcl_Record_Sizes.Tcl_CallFrame_Size - 1);
   for Tcl_CallFrame_Rec'Alignment
     use Tcl_Record_Sizes.Tcl_CallFrame_Alignment;

   type Tcl_Channel_Rec is null record;

   type Tcl_ChannelTypeVersion_Rec is null record;

   type Tcl_Command_Rec is null record;

   type Tcl_Condition_Rec is null record;

   type Tcl_DString_Rec is record
      strng : C.Strings.chars_ptr;
      --  Points to beginning of string: either staticSpace below or a
      --  malloced array.
      length : C.int;
      --  Number of non-NULL characters in the string.
      spaceAvl : C.int;
      --  Total number of bytes available for the string and its
      --  terminating NULL char.
      staticSpace : C.char_array
        (0 .. (Tcl_Record_Sizes.TCL_DSTRING_STATIC_SIZE - 1));
      --  Space to use in common case where string is small.
   end record;
   pragma Convention (C, Tcl_DString_Rec);

   type Tcl_EncodingState_Rec is null record;

   type Tcl_Encoding_Rec is null record;

   --
   --  Structure definition for an entry in a hash table.  No-one outside
   --  Tcl should access any of these fields directly.
   --

   type Tcl_HashEntry_Rec is record
      --  Pointer to next entry in this hash bucket, or NULL for end
      --  of chain.
      nextPtr : Tcl_HashEntry;
      --  Pointer to table containing entry.
      tablePtr : Tcl_HashTable;
      --  Pointer to bucket that points to first entry in this entry's
      --  chain: used for deleting the entry.
      bucketPtr : Tcl_HashEntry;
      --  Application stores something here with Tcl_SetHashValue.
      data : ClientData;
      --  Key is a C union - @todo make exact representation (but note
      --  that the actual size will be as large as necessary for this
      --  table's key).
      key : C.char_array (0 .. 3);
      --  MUST BE LAST FIELD IN RECORD!!
   end record;
   pragma Convention (C, Tcl_HashEntry_Rec);

   --
   --  @todo make exact mapping for Tcl_HashKeyType (may need to be
   --  visible)
   --
   type Tcl_HashKeyType_Rec is null record;
   pragma Convention (C, Tcl_HashKeyType_Rec);

   type Tcl_HashTable_Rec is new Interfaces.C.char_array
     (0 .. Tcl_Record_Sizes.Tcl_HashTable_Size - 1);
   for Tcl_HashTable_Rec'Alignment
     use Tcl_Record_Sizes.Tcl_HashTable_Alignment;

   type Tcl_HashSearch_Rec is new Interfaces.C.char_array
     (0 .. Tcl_Record_Sizes.Tcl_HashSearch_Size - 1);
   for Tcl_HashSearch_Rec'Alignment
     use Tcl_Record_Sizes.Tcl_HashSearch_Alignment;

   type Tcl_Interp_Rec is new Interfaces.C.char_array
     (0 .. Tcl_Record_Sizes.Tcl_Interp_Size - 1);
   for Tcl_Interp_Rec'Alignment
     use Tcl_Record_Sizes.Tcl_Interp_Alignment;

   type Tcl_LoadHandle_Rec is null record;

   type Tcl_Mutex_Rec is null record;

   type Tcl_Pid_Rec is null record;

   type Tcl_RegExp_Rec is null record;

   type Tcl_SavedResult_Rec is new Interfaces.C.char_array
     (0 .. Tcl_Record_Sizes.Tcl_SavedResult_Size - 1);
   for Tcl_SavedResult_Rec'Alignment
     use Tcl_Record_Sizes.Tcl_SavedResult_Alignment;

   type Tcl_OldStat_Rec is null record;  -- stat

   type Tcl_StatBuf_Rec is null record;

   type Tcl_ThreadDataKey_Rec is null record;

   type Tcl_ThreadId_Rec is null record;

   type Tcl_TimerToken_Rec is null record;

   type Tcl_Trace_Rec is null record;

   type Tcl_Var_Rec is null record;

end Tcl;
