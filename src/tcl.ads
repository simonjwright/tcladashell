--------------------------------------------------------------------
--
--  tcl.ads -- This package is the "thin" binding to Tcl.
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
--  Tash is maintained by Terry Westley (http://www.adatcl.com).
--
--------------------------------------------------------------------
--
--  This package is automatically generated from tcl.h.
--  Note that some of the comments below, preserved from tcl.h,
--  do not apply to the Ada version.  Someday, these comments may be
--  customized better.
--
--------------------------------------------------------------------

with CArgv;
with CHelper;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;

package Tcl is

   package C renames Interfaces.C;

   use type C.int;
   use type C.size_t;
   --
   --  tcl.h --
   --
   --   This header file describes the externally-visible facilities
   --   of the Tcl interpreter.
   --
   --  Copyright {c} 1987-1994 The Regents of the University of California.
   --  Copyright {c} 1993-1996 Lucent Technologies.
   --  Copyright {c} 1994-1998 Sun Microsystems, Inc.
   --  Copyright {c} 1998-1999 by Scriptics Corporation.
   --
   --
   --

   --
   --  For C++ compilers, use extern
   --

   --
   --  The following defines are used to indicate the various release levels.
   --

   TCL_ALPHA_RELEASE : constant := 0;
   TCL_BETA_RELEASE  : constant := 1;
   TCL_FINAL_RELEASE : constant := 2;
   --
   --  When version numbers change here, must also go into the following files
   --  and update the version numbers:
   --
   --  library/init.tcl {only if Major.minor changes, not patchlevel} 1 LOC
   --  unix/configure.in        {2 LOC Major, 2 LOC minor, 1 LOC patch}
   --  win/configure.in {as above}
   --  win/tcl.m4               {not patchlevel}
   --  win/makefile.vc  {not patchlevel} 2 LOC
   --  win/pkgIndex.tcl {not patchlevel, for tclregNN.dll}
   --  README           {sections 0 and 2}
   --  mac/README               {2 LOC, not patchlevel}
   --  win/README.binary        {sections 0-4}
   --  win/README               {not patchlevel} {sections 0 and 2}
   --  unix/README              {not patchlevel} {part {h}}
   --  tests/basic.test {not patchlevel} {version checks}
   --  tools/tcl.hpj.in {not patchlevel, for windows installer}
   --  tools/tcl.wse.in {for windows installer}
   --  tools/tclSplash.bmp      {not patchlevel}
   --

   TCL_MAJOR_VERSION  : constant := 8;
   TCL_MINOR_VERSION  : constant := 3;
   TCL_RELEASE_LEVEL  : constant String := "TCL_FINAL_RELEASE";
   TCL_RELEASE_SERIAL : constant := 0;
   TCL_VERSION        : constant String := "8.3";
   TCL_PATCH_LEVEL    : constant String := "8.3.0";
   --
   --  The following definitions set up the proper options for Windows
   --  compilers.  We use this method because there is no autoconf equivalent.
   --

   --  __WIN32__

   --
   --  The following definitions set up the proper options for Macintosh
   --  compilers.  We use this method because there is no autoconf equivalent.
   --

   --
   --  Utility macros: STRINGIFY takes an argument and wraps it in "" {double
   --  quotation marks}, JOIN joins two arguments.
   --

   --
   --  Special macro to define mutexes, that doesn't do anything
   --  if we are not using threads.
   --

   TCL_DECLARE_MUTEX : constant String := "name";
   --
   --  Macros that eliminate the overhead of the thread synchronization
   --  functions when compiling without thread support.
   --

   --  TCL_THREADS

   --
   --  A special definition used to allow this header file to be included
   --  in resource files so that they can get obtain version information from
   --  this file.  Resource compilers don't like all the C stuff, like typedefs
   --  and procedure declarations, that occur below.
   --

   --
   --  Definitions that allow Tcl functions with variable numbers of
   --  arguments to be used with either varargs.h or stdarg.h.  TCL_VARARGS
   --  is used in procedure prototypes.  TCL_VARARGS_DEF is used to declare
   --  the arguments in a function definiton: it takes the type and name of
   --  the first argument and supplies the appropriate argument declaration
   --  string for use in the function definition.  TCL_VARARGS_START
   --  initializes the va_list data structure and returns the first argument.
   --

   --
   --  Macros used to declare a function to be exported by a DLL.
   --  Used by Windows, maps to no-op declarations on non-Windows systems.
   --  The default build on windows is for a DLL, which causes the DLLIMPORT
   --  and DLLEXPORT macros to be nonempty. To build a static library, the
   --  macro STATIC_BUILD should be defined.
   --

   --
   --  These macros are used to control whether functions are being declared
   --  for
   --  import or export.  If a function is being declared while it is being
   --  built
   --  to be included in a shared library, then it should have the DLLEXPORT
   --  storage class.  If is being declared for use by a module that is going
   --  to
   --  link against the shared library, then it should have the DLLIMPORT
   --  storage
   --  class.  If the symbol is beind declared for a static build or for use
   --  from a
   --  stub library, then the storage class should be empty.
   --
   --  The convention is that a macro called BUILD_xxxx, where xxxx is the
   --  name of a library we are building, is set on the compile line for
   --  sources
   --  that are to be placed in the library.  When this macro is set, the
   --  storage class will be set to DLLEXPORT.  At the end of the header file,
   --  the
   --  storage class will be reset to DLLIMPORt.
   --

   TCL_STORAGE_CLASS : constant String := "DLLIMPORT";
   --
   --  Definitions that allow this header file to be used either with or
   --  without ANSI C features like function prototypes.
   --

   --
   --  Make sure extern isn't defined elsewhere
   --

   --  extern

   --
   --  Macro to use instead of "void" for arguments that must have
   --  type "void *" in ANSI C;  maps them to type "char *" in
   --  non-ANSI systems.
   --

   --  __WIN32__

   --  __WIN32__

   --
   --  Miscellaneous declarations.
   --

   subtype ClientData is System.Address;

   --  __STDC__

   --
   --  Data structures defined opaquely in this module. The definitions below
   --  just provide dummy types. A few fields are made visible in Tcl_Interp
   --  structures, namely those used for returning a string result from
   --  commands. Direct access to the result field is discouraged in Tcl 8.0.
   --  The interpreter result is either an object or a string, and the two
   --  values are kept consistent unless some C code sets interp->result
   --  directly. Programmers should use either the procedure Tcl_GetObjResult
   --  {}
   --  or Tcl_GetStringResult {} to read the interpreter's result. See the
   --  SetResult man page for details.
   --
   --  Note: any change to the Tcl_Interp definition below must be mirrored
   --  in the "real" definition in tclInt.h.
   --
   --  Note: Tcl_ObjCmdProc procedures do not directly set result and freeProc.
   --  Instead, they set a Tcl_Obj member in the "real" structure that can be
   --  accessed with Tcl_GetObjResult {} and Tcl_SetObjResult {}.
   --

   type Tcl_Interp_Rec is private;
   type Tcl_Interp is access all Tcl_Interp_Rec;
   pragma Convention (C, Tcl_Interp);

   Null_Tcl_Interp : constant Tcl_Interp;

   function Is_Null (Ptr : in Tcl_Interp) return Boolean;

   type Tcl_AsyncHandler_Rec is private;
   type Tcl_AsyncHandler is access all Tcl_AsyncHandler_Rec;
   pragma Convention (C, Tcl_AsyncHandler);

   Null_Tcl_AsyncHandler : constant Tcl_AsyncHandler;

   function Is_Null (Ptr : in Tcl_AsyncHandler) return Boolean;

   type Tcl_Channel_Rec is private;
   type Tcl_Channel is access all Tcl_Channel_Rec;
   pragma Convention (C, Tcl_Channel);

   Null_Tcl_Channel : constant Tcl_Channel;

   function Is_Null (Ptr : in Tcl_Channel) return Boolean;

   type Tcl_Command_Rec is private;
   type Tcl_Command is access all Tcl_Command_Rec;
   pragma Convention (C, Tcl_Command);

   Null_Tcl_Command : constant Tcl_Command;

   function Is_Null (Ptr : in Tcl_Command) return Boolean;

   type Tcl_Condition_Rec is private;
   type Tcl_Condition is access all Tcl_Condition_Rec;
   pragma Convention (C, Tcl_Condition);

   Null_Tcl_Condition : constant Tcl_Condition;

   function Is_Null (Ptr : in Tcl_Condition) return Boolean;

   type Tcl_EncodingState_Rec is private;
   type Tcl_EncodingState is access all Tcl_EncodingState_Rec;
   pragma Convention (C, Tcl_EncodingState);

   Null_Tcl_EncodingState : constant Tcl_EncodingState;

   function Is_Null (Ptr : in Tcl_EncodingState) return Boolean;

   type Tcl_Encoding_Rec is private;
   type Tcl_Encoding is access all Tcl_Encoding_Rec;
   pragma Convention (C, Tcl_Encoding);

   Null_Tcl_Encoding : constant Tcl_Encoding;

   function Is_Null (Ptr : in Tcl_Encoding) return Boolean;

   type Tcl_Event_Rec is private;
   type Tcl_Event is access all Tcl_Event_Rec;
   pragma Convention (C, Tcl_Event);

   Null_Tcl_Event : constant Tcl_Event;

   function Is_Null (Ptr : in Tcl_Event) return Boolean;

   type Tcl_Mutex_Rec is private;
   type Tcl_Mutex is access all Tcl_Mutex_Rec;
   pragma Convention (C, Tcl_Mutex);

   Null_Tcl_Mutex : constant Tcl_Mutex;

   function Is_Null (Ptr : in Tcl_Mutex) return Boolean;

   type Tcl_Pid_Rec is private;
   type Tcl_Pid is access all Tcl_Pid_Rec;
   pragma Convention (C, Tcl_Pid);

   Null_Tcl_Pid : constant Tcl_Pid;

   function Is_Null (Ptr : in Tcl_Pid) return Boolean;

   type Tcl_RegExp_Rec is private;
   type Tcl_RegExp is access all Tcl_RegExp_Rec;
   pragma Convention (C, Tcl_RegExp);

   Null_Tcl_RegExp : constant Tcl_RegExp;

   function Is_Null (Ptr : in Tcl_RegExp) return Boolean;

   type Tcl_ThreadDataKey_Rec is private;
   type Tcl_ThreadDataKey is access all Tcl_ThreadDataKey_Rec;
   pragma Convention (C, Tcl_ThreadDataKey);

   Null_Tcl_ThreadDataKey : constant Tcl_ThreadDataKey;

   function Is_Null (Ptr : in Tcl_ThreadDataKey) return Boolean;

   type Tcl_ThreadId_Rec is private;
   type Tcl_ThreadId is access all Tcl_ThreadId_Rec;
   pragma Convention (C, Tcl_ThreadId);

   Null_Tcl_ThreadId : constant Tcl_ThreadId;

   function Is_Null (Ptr : in Tcl_ThreadId) return Boolean;

   type Tcl_TimerToken_Rec is private;
   type Tcl_TimerToken is access all Tcl_TimerToken_Rec;
   pragma Convention (C, Tcl_TimerToken);

   Null_Tcl_TimerToken : constant Tcl_TimerToken;

   function Is_Null (Ptr : in Tcl_TimerToken) return Boolean;

   type Tcl_Trace_Rec is private;
   type Tcl_Trace is access all Tcl_Trace_Rec;
   pragma Convention (C, Tcl_Trace);

   Null_Tcl_Trace : constant Tcl_Trace;

   function Is_Null (Ptr : in Tcl_Trace) return Boolean;

   type Tcl_Var_Rec is private;
   type Tcl_Var is access all Tcl_Var_Rec;
   pragma Convention (C, Tcl_Var);

   Null_Tcl_Var : constant Tcl_Var;

   function Is_Null (Ptr : in Tcl_Var) return Boolean;

   --
   --  Flag values passed to Tcl_GetRegExpFromObj.
   --

   TCL_REG_BASIC : constant := 0;
   --  BREs {convenience}

   TCL_REG_EXTENDED : constant := 1;
   --  EREs

   TCL_REG_ADVF : constant := 2;
   --  advanced features in EREs

   TCL_REG_ADVANCED : constant := 3;
   --  AREs {which are also EREs}

   TCL_REG_QUOTE : constant := 4;
   --  no special characters, none

   TCL_REG_NOCASE : constant := 8;
   --  ignore case

   TCL_REG_NOSUB : constant := 16;
   --  don't care about subexpressions

   TCL_REG_EXPANDED : constant := 32;
   --  expanded format, white space &
   --  comments

   TCL_REG_NLSTOP : constant := 64;
   --  \n doesn't match . or {^ }

   TCL_REG_NLANCH : constant := 128;
   --  ^ matches after \n, $ before

   TCL_REG_NEWLINE : constant := 192;
   --  newlines are line terminators

   TCL_REG_CANMATCH : constant := 512;
   --  report details on partial/limited
   --  matches

   --
   --  The following flag is experimental and only intended for use by Expect.
   --  It
   --  will probably go away in a later release.
   --

   TCL_REG_BOSONLY : constant := 1024;
   --  prepend \A to pattern so it only
   --  matches at the beginning of the
   --  string.

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
      Start : C.long;    --  character offset of first character in match
      E_n_d : C.long;      --  character offset of first character after the
                           --  match.
   end record;

   type Tcl_RegExpIndices_Array is
     array (CArgv.CNatural range <>) of aliased Tcl_RegExpIndices_Rec;

   package Tcl_RegExpIndices_Pointer is new C.Pointers (
      Index => CArgv.CNatural,
      Element => Tcl_RegExpIndices_Rec,
      Element_Array => Tcl_RegExpIndices_Array,
      Default_Terminator => (0, 0));

   subtype Tcl_RegExpIndices is Tcl_RegExpIndices_Pointer.Pointer;

   type Tcl_RegExpInfo_Rec is record
      nsubs : C.int;          --  number of subexpressions in the
                              --  compiled expression
      matches : Tcl_RegExpIndices;
      --  array of nsubs match offset
      --  pairs
      extendStart : C.long;         --  The offset at which a subsequent
                                    --  match might begin.
      reserved : C.long;       --  Reserved for later use.
   end record;

   type Tcl_RegExpInfo is access all Tcl_RegExpInfo_Rec;
   pragma Convention (C, Tcl_RegExpInfo);

   Null_Tcl_RegExpInfo : constant Tcl_RegExpInfo := null;

   function Is_Null (Ptr : in Tcl_RegExpInfo) return Boolean;

   --
   --  Picky compilers complain if this typdef doesn't appear before the
   --  struct's reference in tclDecls.h.
   --

   type stat_Rec is private;
   type stat is access all stat_Rec;
   pragma Convention (C, stat);

   Null_stat : constant stat;

   function Is_Null (Ptr : in stat) return Boolean;

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
   --  TCL_ERROR                The command couldn't be completed successfully;
   --                   the interpreter's result describes what went wrong.
   --  TCL_RETURN               The command requests that the current procedure
   --                   return; the interpreter's result contains the
   --                   procedure's return value.
   --  TCL_BREAK                The command requests that the innermost loop
   --                   be exited; the interpreter's result is meaningless.
   --  TCL_CONTINUE             Go on to the next iteration of the current
   --  loop;
   --                   the interpreter's result is meaningless.
   --

   TCL_OK          : constant := 0;
   TCL_ERROR       : constant := 1;
   TCL_RETURN      : constant := 2;
   TCL_BREAK       : constant := 3;
   TCL_CONTINUE    : constant := 4;
   TCL_RESULT_SIZE : constant := 200;
   --
   --  Argument descriptors for math function callbacks in expressions:
   --

   type Tcl_ValueType is (TCL_INT, TCL_DOUBLE, TCL_EITHER);
   for Tcl_ValueType'Size use 32;

   type Tcl_Value_Rec is private;
   type Tcl_Value is access all Tcl_Value_Rec;
   pragma Convention (C, Tcl_Value);

   Null_Tcl_Value : constant Tcl_Value;

   function Is_Null (Ptr : in Tcl_Value) return Boolean;

   --
   --  Forward declaration of Tcl_Obj to prevent an error when the forward
   --  reference to Tcl_Obj is encountered in the procedure types declared
   --  below.
   --

   type Tcl_Obj_Rec is private;
   type Tcl_Obj is access all Tcl_Obj_Rec;
   pragma Convention (C, Tcl_Obj);

   Null_Tcl_Obj : constant Tcl_Obj;

   subtype CNatural is C.int range 0 .. C.int'Last;

   type Tcl_Obj_Array is array (CNatural range <>) of aliased Tcl_Obj;
   pragma Convention (C, Tcl_Obj_Array);

   function Is_Null (Ptr : in Tcl_Obj) return Boolean;
   type Tcl_Obj_Ptr is access all Tcl_Obj;
   pragma Convention (C, Tcl_Obj_Ptr);

   Null_Tcl_Obj_Ptr : constant Tcl_Obj_Ptr;

   --
   --  Procedure types defined by Tcl:
   --

   type Tcl_AppInitProc is access function
     (interp : in Tcl_Interp)
   return      C.int;
   pragma Convention (C, Tcl_AppInitProc);

   type Tcl_AsyncProc is access function
     (data   : in ClientData;
      interp : in Tcl_Interp;
      code   : in C.int)
   return      C.int;
   pragma Convention (C, Tcl_AsyncProc);

   type Tcl_ChannelProc is access procedure
  (data : in ClientData;
   mask : in C.int);
   pragma Convention (C, Tcl_ChannelProc);

   type Tcl_CloseProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_CloseProc);

   type Tcl_CmdDeleteProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_CmdDeleteProc);

   type Tcl_CmdProc is access function
     (data   : in ClientData;
      interp : in Tcl_Interp;
      argc   : in C.int;
      argv   : in CArgv.Chars_Ptr_Ptr)
   return      C.int;
   pragma Convention (C, Tcl_CmdProc);

   type Tcl_CmdTraceProc is access procedure
  (data          : in ClientData;
   interp        : in Tcl_Interp;
   level         : in C.int;
   command       : in C.Strings.chars_ptr;
   proc          : in Tcl_CmdProc;
   cmdclientdata : in ClientData;
   argc          : in C.int;
   argv          : in CArgv.Chars_Ptr_Ptr);
   pragma Convention (C, Tcl_CmdTraceProc);

   type Tcl_DupInternalRepProc is access procedure;
   pragma Convention (C, Tcl_DupInternalRepProc);

   type Tcl_EncodingConvertProc is access function
     (data        : in ClientData;
      src         : in C.Strings.chars_ptr;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : in Tcl_EncodingState;
      dst         : in C.Strings.chars_ptr;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
   return           C.int;
   pragma Convention (C, Tcl_EncodingConvertProc);

   type Tcl_EncodingFreeProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_EncodingFreeProc);

   type Tcl_EventProc is access function
     (evPtr : in Tcl_Event;
      flags : in C.int)
   return     C.int;
   pragma Convention (C, Tcl_EventProc);

   type Tcl_EventCheckProc is access procedure
  (data  : in ClientData;
   flags : in C.int);
   pragma Convention (C, Tcl_EventCheckProc);

   type Tcl_EventDeleteProc is access function
     (evPtr : in Tcl_Event;
      data  : in ClientData)
   return     C.int;
   pragma Convention (C, Tcl_EventDeleteProc);

   type Tcl_EventSetupProc is access procedure
  (data  : in ClientData;
   flags : in C.int);
   pragma Convention (C, Tcl_EventSetupProc);

   type Tcl_ExitProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_ExitProc);

   type Tcl_FileProc is access procedure
  (data : in ClientData;
   mask : in C.int);
   pragma Convention (C, Tcl_FileProc);

   type Tcl_FileFreeProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_FileFreeProc);

   type Tcl_FreeInternalRepProc is access procedure;
   pragma Convention (C, Tcl_FreeInternalRepProc);

   type Tcl_FreeProc is access procedure
  (blockPtr : in C.Strings.chars_ptr);
   pragma Convention (C, Tcl_FreeProc);

   type Tcl_IdleProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_IdleProc);

   type Tcl_InterpDeleteProc is access procedure
  (data   : in ClientData;
   interp : in Tcl_Interp);
   pragma Convention (C, Tcl_InterpDeleteProc);

   type Tcl_MathProc is access function
     (data      : in ClientData;
      interp    : in Tcl_Interp;
      args      : in Tcl_Value;
      resultPtr : in Tcl_Value)
   return         C.int;
   pragma Convention (C, Tcl_MathProc);

   type Tcl_NamespaceDeleteProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_NamespaceDeleteProc);

   type Tcl_ObjCmdProc is access function
     (data   : in ClientData;
      interp : in Tcl_Interp;
      objc   : in C.int)
   return      C.int;
   pragma Convention (C, Tcl_ObjCmdProc);

   type Tcl_PackageInitProc is access function
     (interp : in Tcl_Interp)
   return      C.int;
   pragma Convention (C, Tcl_PackageInitProc);

   type Tcl_PanicProc is access procedure
  (format  : in C.Strings.chars_ptr;
   String1 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String2 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String3 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String4 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String5 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String6 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String7 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String8 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
   String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Convention (C, Tcl_PanicProc);

   type Tcl_TcpAcceptProc is access procedure
  (callbackdata : in ClientData;
   chan         : in Tcl_Channel;
   address      : in System.Address;
   port         : in C.int);
   pragma Convention (C, Tcl_TcpAcceptProc);

   type Tcl_TimerProc is access procedure (data : in ClientData);
   pragma Convention (C, Tcl_TimerProc);

   type Tcl_SetFromAnyProc is access function
     (interp : in Tcl_Interp)
   return      C.int;
   pragma Convention (C, Tcl_SetFromAnyProc);

   type Tcl_UpdateStringProc is access procedure;
   pragma Convention (C, Tcl_UpdateStringProc);

   type Tcl_VarTraceProc is access function
     (data   : in ClientData;
      interp : in Tcl_Interp;
      part1  : in C.Strings.chars_ptr;
      part2  : in C.Strings.chars_ptr;
      flags  : in C.int)
   return      C.Strings.chars_ptr;
   pragma Convention (C, Tcl_VarTraceProc);

   type Tcl_CreateFileHandlerProc is access procedure
  (fd   : in C.int;
   mask : in C.int;
   proc : in Tcl_FileProc;
   data : in ClientData);
   pragma Convention (C, Tcl_CreateFileHandlerProc);

   type Tcl_DeleteFileHandlerProc is access procedure (fd : in C.int);
   pragma Convention (C, Tcl_DeleteFileHandlerProc);

   --
   --  The following structure represents a type of object, which is a
   --  particular internal representation for an object plus a set of
   --  procedures that provide standard operations on objects of that type.
   --

   type Tcl_ObjType_Rec is private;
   type Tcl_ObjType is access all Tcl_ObjType_Rec;
   pragma Convention (C, Tcl_ObjType);

   Null_Tcl_ObjType : constant Tcl_ObjType;

   function Is_Null (Ptr : in Tcl_ObjType) return Boolean;

   --
   --  One of the following structures exists for each object in the Tcl
   --  system. An object stores a value as either a string, some internal
   --  representation, or both.
   --

   --
   --  Macros to increment and decrement a Tcl_Obj's reference count, and to
   --  test whether an object is shared {i.e. has reference count > 1}.
   --  Note: clients should use Tcl_DecrRefCount {} when they are finished
   --  using
   --  an object, and should never call TclFreeObj {} directly. TclFreeObj {}
   --  is
   --  only defined and made public in tcl.h to support Tcl_DecrRefCount's
   --  macro
   --  definition. Note also that Tcl_DecrRefCount {} refers to the parameter
   --  "obj" twice. This means that you should avoid calling it with an
   --  expression that is expensive to compute or has side effects.
   --

   procedure Tcl_IncrRefCount (objPtr : in Tcl_Obj);
   pragma Import (C, Tcl_IncrRefCount, "Tcl_CallIncrRefCount");

   procedure Tcl_DecrRefCount (objPtr : in Tcl_Obj);
   pragma Import (C, Tcl_DecrRefCount, "Tcl_CallDecrRefCount");

   function Tcl_IsShared (objPtr : in Tcl_Obj) return C.int;
   pragma Import (C, Tcl_IsShared, "Tcl_CallIsShared");

   --
   --  Macros and definitions that help to debug the use of Tcl objects.
   --  When TCL_MEM_DEBUG is defined, the Tcl_New declarations are
   --  overridden to call debugging versions of the object creation procedures.
   --

   --  TCL_MEM_DEBUG

   --
   --  The following structure contains the state needed by
   --  Tcl_SaveResult.  No-one outside of Tcl should access any of these
   --  fields.  This structure is typically allocated on the stack.
   --

   type Tcl_SavedResult_Rec is private;
   type Tcl_SavedResult is access all Tcl_SavedResult_Rec;
   pragma Convention (C, Tcl_SavedResult);

   Null_Tcl_SavedResult : constant Tcl_SavedResult;

   function Is_Null (Ptr : in Tcl_SavedResult) return Boolean;

   --
   --  The following definitions support Tcl's namespace facility.
   --  Note: the first five fields must match exactly the fields in a
   --  Namespace structure {see tclInt.h}.
   --

   type Tcl_Namespace_Rec is private;
   type Tcl_Namespace is access all Tcl_Namespace_Rec;
   pragma Convention (C, Tcl_Namespace);

   Null_Tcl_Namespace : constant Tcl_Namespace;

   function Is_Null (Ptr : in Tcl_Namespace) return Boolean;

   --
   --  The following structure represents a call frame, or activation record.
   --  A call frame defines a naming context for a procedure call: its local
   --  scope {for local variables} and its namespace scope {used for non-local
   --  variables; often the global :: namespace}. A call frame can also define
   --  the naming context for a namespace eval or namespace inscope command:
   --  the namespace in which the command's code should execute. The
   --  Tcl_CallFrame structures exist only while procedures or namespace
   --  eval/inscope's are being executed, and provide a Tcl call stack.
   --
   --  A call frame is initialized and pushed using Tcl_PushCallFrame and
   --  popped using Tcl_PopCallFrame. Storage for a Tcl_CallFrame must be
   --  provided by the Tcl_PushCallFrame caller, and callers typically allocate
   --  them on the C call stack for efficiency. For this reason, Tcl_CallFrame
   --  is defined as a structure and not as an opaque token. However, most
   --  Tcl_CallFrame fields are hidden since applications should not access
   --  them directly; others are declared as "dummyX".
   --
   --  WARNING!! The structure definition must be kept consistent with the
   --  CallFrame structure in tclInt.h. If you change one, change the other.
   --

   type Tcl_CallFrame_Rec is private;
   type Tcl_CallFrame is access all Tcl_CallFrame_Rec;
   pragma Convention (C, Tcl_CallFrame);

   Null_Tcl_CallFrame : constant Tcl_CallFrame;

   function Is_Null (Ptr : in Tcl_CallFrame) return Boolean;

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

   type Tcl_CmdInfo_Rec is private;
   type Tcl_CmdInfo is access all Tcl_CmdInfo_Rec;
   pragma Convention (C, Tcl_CmdInfo);

   Null_Tcl_CmdInfo : constant Tcl_CmdInfo;

   function Is_Null (Ptr : in Tcl_CmdInfo) return Boolean;

   --
   --  The structure defined below is used to hold dynamic strings.  The only
   --  field that clients should use is the string field, and they should
   --  never modify it.
   --

   TCL_DSTRING_STATIC_SIZE : constant := 200;
   type Tcl_DString_Rec is private;
   type Tcl_DString is access all Tcl_DString_Rec;
   pragma Convention (C, Tcl_DString);

   Null_Tcl_DString : constant Tcl_DString;

   function Is_Null (Ptr : in Tcl_DString) return Boolean;

   function Tcl_DStringLength (dsPtr : in Tcl_DString) return C.int;
   pragma Import (C, Tcl_DStringLength, "Tcl_CallDStringLength");

   function Tcl_DStringValue
     (dsPtr : in Tcl_DString)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_DStringValue, "Tcl_CallDStringValue");

   procedure Tcl_DStringSetLength
     (dsPtr  : in Tcl_DString;
      length : in C.int);
   pragma Import (C, Tcl_DStringSetLength, "Tcl_DStringSetLength");

   procedure Tcl_DStringTrunc (dsPtr : in Tcl_DString; length : in C.int);
   pragma Import (C, Tcl_DStringTrunc, "Tcl_DStringSetLength");

   --
   --  Definitions for the maximum number of digits of precision that may
   --  be specified in the "tcl_precision" variable, and the number of
   --  bytes of buffer space required by Tcl_PrintDouble.
   --

   TCL_MAX_PREC     : constant := 17;
   TCL_DOUBLE_SPACE : constant := TCL_MAX_PREC + 10;
   --
   --  Definition for a number of bytes of buffer space sufficient to hold the
   --  string representation of an integer in base 10 {assuming the existence
   --  of 64-bit integers}.
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

   TCL_NO_EVAL     : constant := 65536;
   TCL_EVAL_GLOBAL : constant := 131072;
   TCL_EVAL_DIRECT : constant := 262144;
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

   TCL_GLOBAL_ONLY      : constant := 1;
   TCL_NAMESPACE_ONLY   : constant := 2;
   TCL_APPEND_VALUE     : constant := 4;
   TCL_LIST_ELEMENT     : constant := 8;
   TCL_TRACE_READS      : constant := 16;
   TCL_TRACE_WRITES     : constant := 32;
   TCL_TRACE_UNSETS     : constant := 64;
   TCL_TRACE_DESTROYED  : constant := 128;
   TCL_INTERP_DESTROYED : constant := 256;
   TCL_LEAVE_ERR_MSG    : constant := 512;
   TCL_TRACE_ARRAY      : constant := 2048;
   --
   --  The TCL_PARSE_PART1 flag is deprecated and has no effect.
   --  The part1 is now always parsed whenever the part2 is NULL.
   --  {This is to avoid a common error when converting code to
   --   use the new object based APIs and forgetting to give the
   --   flag}
   --

   TCL_PARSE_PART1 : constant := 1024;
   --
   --  Types for linked variables:
   --

   TCL_LINK_INT       : constant := 1;
   TCL_LINK_DOUBLE    : constant := 2;
   TCL_LINK_BOOLEAN   : constant := 3;
   TCL_LINK_STRING    : constant := 4;
   TCL_LINK_READ_ONLY : constant := 128;
   --
   --  Forward declaration of Tcl_HashTable.  Needed by some C++ compilers
   --  to prevent errors when the forward reference to Tcl_HashTable is
   --  encountered in the Tcl_HashEntry structure.
   --

   --
   --  Structure definition for an entry in a hash table.  No-one outside
   --  Tcl should access any of these fields directly;  use the macros
   --  defined below.
   --

   type Tcl_HashEntry_Rec is private;
   type Tcl_HashEntry is access all Tcl_HashEntry_Rec;
   pragma Convention (C, Tcl_HashEntry);

   Null_Tcl_HashEntry : constant Tcl_HashEntry;

   type Tcl_HashEntry_Array is
     array (CNatural range <>) of aliased Tcl_HashEntry;
   pragma Convention (C, Tcl_HashEntry_Array);

   function Is_Null (Ptr : in Tcl_HashEntry) return Boolean;

   --
   --  Structure definition for a hash table.  Must be in tcl.h so clients
   --  can allocate space for these structures, but clients should never
   --  access any fields in this structure.
   --

   TCL_SMALL_HASH_TABLE : constant := 4;
   type Tcl_HashTable_Rec is private;
   type Tcl_HashTable is access all Tcl_HashTable_Rec;
   pragma Convention (C, Tcl_HashTable);

   Null_Tcl_HashTable : constant Tcl_HashTable;

   function Is_Null (Ptr : in Tcl_HashTable) return Boolean;

   --
   --  Structure definition for information used to keep track of searches
   --  through hash tables:
   --

   type Tcl_HashSearch_Rec is private;
   type Tcl_HashSearch is access all Tcl_HashSearch_Rec;
   pragma Convention (C, Tcl_HashSearch);

   Null_Tcl_HashSearch : constant Tcl_HashSearch;

   function Is_Null (Ptr : in Tcl_HashSearch) return Boolean;

   --
   --  Acceptable key types for hash tables:
   --

   TCL_STRING_KEYS   : constant := 0;
   TCL_ONE_WORD_KEYS : constant := 1;
   --
   --  Macros for clients to use to access fields of hash entries:
   --

   function Tcl_GetHashValue
     (HashEntry : in Tcl_HashEntry)
      return      ClientData;
   pragma Import (C, Tcl_GetHashValue, "Tcl_CallGetHashValue");

   procedure Tcl_SetHashValue
     (HashEntry : in Tcl_HashEntry;
      value     : in ClientData);
   pragma Import (C, Tcl_SetHashValue, "Tcl_CallSetHashValue");

   function Tcl_GetHashKey
     (HashTable : in Tcl_HashTable;
      HashEntry : in Tcl_HashEntry)
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetHashKey, "Tcl_CallGetHashKey");

   --
   --  Macros to use for clients to use to invoke find and create procedures
   --  for hash tables:
   --

   function Tcl_FindHashEntry
     (HashTable : in Tcl_HashTable;
      key       : in C.Strings.chars_ptr)
      return      Tcl_HashEntry;
   pragma Import (C, Tcl_FindHashEntry, "Tcl_CallFindHashEntry");

   function Tcl_CreateHashEntry
     (HashTable : in Tcl_HashTable;
      key       : in C.Strings.chars_ptr;
      newPtr    : access C.int)
      return      Tcl_HashEntry;
   pragma Import (C, Tcl_CreateHashEntry, "Tcl_CallCreateHashEntry");

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
   --  The following structure defines a generic event for the Tcl event
   --  system.  These are the things that are queued in calls to Tcl_QueueEvent
   --  and serviced later by Tcl_DoOneEvent.  There can be many different
   --  kinds of events with different fields, corresponding to window events,
   --  timer events, etc.  The structure for a particular event consists of
   --  a Tcl_Event header followed by additional information specific to that
   --  event.
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
   --  The following structure keeps is used to hold a time value, either as
   --  an absolute time {the number of seconds from the epoch} or as an
   --  elapsed time. On Unix systems the epoch is Midnight Jan 1, 1970 GMT.
   --  On Macintosh systems the epoch is Midnight Jan 1, 1904 GMT.
   --

   type Tcl_Time_Rec is private;
   type Tcl_Time is access all Tcl_Time_Rec;
   pragma Convention (C, Tcl_Time);

   Null_Tcl_Time : constant Tcl_Time;

   function Is_Null (Ptr : in Tcl_Time) return Boolean;

   type Tcl_SetTimerProc is access procedure (timePtr : in Tcl_Time);
   pragma Convention (C, Tcl_SetTimerProc);

   type Tcl_WaitForEventProc is access function
     (timePtr : in Tcl_Time)
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
   --  disposition of the stdio handles.  TCL_STDIN, TCL_STDOUT, TCL_STDERR,
   --  are also used in Tcl_GetStdChannel.
   --

   TCL_STDIN        : constant := 2;
   TCL_STDOUT       : constant := 4;
   TCL_STDERR       : constant := 8;
   TCL_ENFORCE_MODE : constant := 16;
   --
   --  Bits passed to Tcl_DriverClose2Proc to indicate which side of a channel
   --  should be closed.
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
     (instancedata : in ClientData;
      mode         : in C.int)
   return            C.int;
   pragma Convention (C, Tcl_DriverBlockModeProc);

   type Tcl_DriverCloseProc is access function
     (instancedata : in ClientData;
      interp       : in Tcl_Interp)
   return            C.int;
   pragma Convention (C, Tcl_DriverCloseProc);

   type Tcl_DriverClose2Proc is access function
     (instancedata : in ClientData;
      interp       : in Tcl_Interp;
      flags        : in C.int)
   return            C.int;
   pragma Convention (C, Tcl_DriverClose2Proc);

   type Tcl_DriverInputProc is access function
     (instancedata : in ClientData;
      buf          : in C.Strings.chars_ptr;
      toRead       : in C.int;
      errorCodePtr : access C.int)
   return            C.int;
   pragma Convention (C, Tcl_DriverInputProc);

   type Tcl_DriverOutputProc is access function
     (instancedata : in ClientData;
      buf          : in C.Strings.chars_ptr;
      toWrite      : in C.int;
      errorCodePtr : access C.int)
   return            C.int;
   pragma Convention (C, Tcl_DriverOutputProc);

   type Tcl_DriverSeekProc is access function
     (instancedata : in ClientData;
      offset       : in C.long;
      mode         : in C.int;
      errorCodePtr : access C.int)
   return            C.int;
   pragma Convention (C, Tcl_DriverSeekProc);

   type Tcl_DriverSetOptionProc is access function
     (instancedata : in ClientData;
      interp       : in Tcl_Interp;
      optionName   : in C.Strings.chars_ptr;
      value        : in C.Strings.chars_ptr)
   return            C.int;
   pragma Convention (C, Tcl_DriverSetOptionProc);

   type Tcl_DriverGetOptionProc is access function
     (instancedata : in ClientData;
      interp       : in Tcl_Interp;
      optionName   : in C.Strings.chars_ptr;
      dsPtr        : in Tcl_DString)
   return            C.int;
   pragma Convention (C, Tcl_DriverGetOptionProc);

   type Tcl_DriverWatchProc is access procedure
  (instancedata : in ClientData;
   mask         : in C.int);
   pragma Convention (C, Tcl_DriverWatchProc);

   type Tcl_DriverGetHandleProc is access function
     (instancedata : in ClientData;
      direction    : in C.int;
      handleptr    : in ClientData)
   return            C.int;
   pragma Convention (C, Tcl_DriverGetHandleProc);

   --
   --  malloc and free, or they map them to procedures with all sorts
   --  of debugging hooks defined in tclCkalloc.c.
   --

   --  !TCL_MEM_DEBUG

   --
   --  If we are not using the debugging allocator, we should call the
   --  is using the same memory allocator both inside and outside of the
   --  Tcl library.
   --

   --  !TCL_MEM_DEBUG

   --
   --  Enum for different end of line translation and recognition modes.
   --

   type Tcl_EolTranslation is (
      TCL_TRANSLATE_AUTO,
   --  Eol == \r, \n and \r\n.
      TCL_TRANSLATE_CR,
   --  Eol == \r.
      TCL_TRANSLATE_LF,
   --  Eol == \n.
      TCL_TRANSLATE_CRLF
   --  Eol == \r\n.
     );
   for Tcl_EolTranslation'Size use 32;

   --
   --  struct Tcl_ChannelType:
   --
   --  One such structure exists for each type {kind} of channel.
   --  It collects together in one place all the functions that are
   --  part of the specific channel type.
   --

   type Tcl_ChannelType_Rec is private;
   type Tcl_ChannelType is access all Tcl_ChannelType_Rec;
   pragma Convention (C, Tcl_ChannelType);

   Null_Tcl_ChannelType : constant Tcl_ChannelType;

   function Is_Null (Ptr : in Tcl_ChannelType) return Boolean;

   --
   --  The following flags determine whether the blockModeProc above should
   --  set the channel into blocking or nonblocking mode. They are passed
   --  as arguments to the blockModeProc procedure in the above structure.
   --

   TCL_MODE_BLOCKING : constant := 0;
   --  Put channel into blocking mode.

   TCL_MODE_NONBLOCKING : constant := 1;
   --  Put channel into nonblocking
   --  mode.

   --
   --  Enum for different types of file paths.
   --

   type Tcl_PathType is (
      TCL_PATH_ABSOLUTE,
      TCL_PATH_RELATIVE,
      TCL_PATH_VOLUME_RELATIVE);
   for Tcl_PathType'Size use 32;

   --
   --  The following structure represents the Notifier functions that
   --  you can override with the Tcl_SetNotifier call.
   --

   type Tcl_NotifierProcs_Rec is private;
   type Tcl_NotifierProcs is access all Tcl_NotifierProcs_Rec;
   pragma Convention (C, Tcl_NotifierProcs);

   Null_Tcl_NotifierProcs : constant Tcl_NotifierProcs;

   function Is_Null (Ptr : in Tcl_NotifierProcs) return Boolean;

   --
   --  The following structure represents a user-defined encoding.  It collects
   --  together all the functions that are used by the specific encoding.
   --

   type Tcl_EncodingType_Rec is private;
   type Tcl_EncodingType is access all Tcl_EncodingType_Rec;
   pragma Convention (C, Tcl_EncodingType);

   Null_Tcl_EncodingType : constant Tcl_EncodingType;

   function Is_Null (Ptr : in Tcl_EncodingType) return Boolean;

   --
   --  The following definitions are used as values for the conversion control
   --  flags argument when converting text from one character set to another:
   --
   --  TCL_ENCODING_START:              Signifies that the source buffer is
   --  the first
   --                           block in a {potentially multi-block} input
   --                           stream.  Tells the conversion procedure to
   --                           reset to an initial state and perform any
   --                           initialization that needs to occur before the
   --                           first byte is converted.  If the source
   --                           buffer contains the entire input stream to be
   --                           converted, this flag should be set.
   --
   --  TCL_ENCODING_END:                Signifies that the source buffer is
   --  the last
   --                           block in a {potentially multi-block} input
   --                           stream.  Tells the conversion routine to
   --                           perform any finalization that needs to occur
   --                           after the last byte is converted and then to
   --                           reset to an initial state.  If the source
   --                           buffer contains the entire input stream to be
   --                           converted, this flag should be set.
   --
   --  TCL_ENCODING_STOPONERROR:        If set, then the converter will return
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
   --  Tcl parser.      This stuff should all move to tcl.h eventually.
   -- ----------------------------------------------------------------
   --

   --
   --  For each word of a command, and for each piece of a word such as a
   --  variable reference, one of the following structures is created to
   --  describe the token.
   --

   type Tcl_Token_Rec is private;
   type Tcl_Token is access all Tcl_Token_Rec;
   pragma Convention (C, Tcl_Token);

   Null_Tcl_Token : constant Tcl_Token;

   type Tcl_Token_Array is array (CNatural range <>) of aliased Tcl_Token;
   pragma Convention (C, Tcl_Token_Array);

   function Is_Null (Ptr : in Tcl_Token) return Boolean;

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
   --  TCL_TOKEN_COMMAND -              The token describes a command whose
   --  result
   --                           must be substituted into the word.  The
   --                           token includes the enclosing brackets.
   --                           NumComponents is always 0.
   --  TCL_TOKEN_VARIABLE -             The token describes a variable
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
   --  TCL_TOKEN_SUB_EXPR -             The token describes one subexpression
   --  of a
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
   --  TCL_TOKEN_OPERATOR -             The token describes one expression
   --  operator.
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
   --  A structure of the following type is filled in by Tcl_ParseCommand.
   --  It describes a single command parsed from an input string.
   --

   NUM_STATIC_TOKENS : constant := 20;
   type Tcl_Parse_Rec is private;
   type Tcl_Parse is access all Tcl_Parse_Rec;
   pragma Convention (C, Tcl_Parse);

   Null_Tcl_Parse : constant Tcl_Parse;

   function Is_Null (Ptr : in Tcl_Parse) return Boolean;

   --
   --  The following definitions are the error codes returned by the conversion
   --  routines:
   --
   --  TCL_OK:                  All characters were converted.
   --
   --  TCL_CONVERT_NOSPACE:             The output buffer would not have been
   --  large
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
   --  TCL_CONVERT_SYNTAX:              The source stream contained an invalid
   --                           character sequence.  This may occur if the
   --                           input stream has been damaged or if the input
   --                           encoding method was misidentified.  This error
   --                           is reported only if TCL_ENCODING_STOPONERROR
   --                           was specified.
   --
   --  TCL_CONVERT_UNKNOWN:             The source string contained a character
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
     (interp    : in Tcl_Interp;
      name      : in C.Strings.chars_ptr;
      bufferPtr : in Tcl_DString)
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_TranslateFileName, "Tcl_TranslateFileName");

   function Tcl_TildeSubst
     (interp    : in Tcl_Interp;
      name      : in C.Strings.chars_ptr;
      bufferPtr : in Tcl_DString)
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_TildeSubst, "Tcl_TranslateFileName");

   procedure Tcl_Panic
     (format  : in C.Strings.chars_ptr;
      String1 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr);
   pragma Import (C, Tcl_Panic, "Tcl_Panic");

   procedure panic
     (format  : in C.Strings.chars_ptr;
      String1 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String2 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String3 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String4 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String5 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String6 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String7 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String8 : in C.Strings.chars_ptr := C.Strings.Null_Ptr;
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr);
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
     (interp  : in Tcl_Interp;
      name    : in C.Strings.chars_ptr;
      version : in C.Strings.chars_ptr;
      data    : in ClientData);
   pragma Import (C, Tcl_PkgProvideEx, "Tcl_PkgProvideEx");

   --  1

   function Tcl_PkgRequireEx
     (interp        : in Tcl_Interp;
      name          : in C.Strings.chars_ptr;
      version       : in C.Strings.chars_ptr;
      exact         : in C.int;
      clientdataptr : in ClientData)
      return          C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgRequireEx, "Tcl_PkgRequireEx");

   --  2

   --  3

   --  4

   --  5

   --  6

   function Tcl_DbCkalloc
     (size : in C.unsigned;
      file : in C.Strings.chars_ptr;
      line : in C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_DbCkalloc, "Tcl_DbCkalloc");

   --  7

   function Tcl_DbCkfree
     (ptr  : in C.Strings.chars_ptr;
      file : in C.Strings.chars_ptr;
      line : in C.int)
      return C.int;
   pragma Import (C, Tcl_DbCkfree, "Tcl_DbCkfree");

   --  8

   function Tcl_DbCkrealloc
     (ptr  : in C.Strings.chars_ptr;
      size : in C.unsigned;
      file : in C.Strings.chars_ptr;
      line : in C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_DbCkrealloc, "Tcl_DbCkrealloc");

   --  UNIX

   --  9

   procedure Tcl_CreateFileHandler
     (fd   : in C.int;
      mask : in C.int;
      proc : in Tcl_FileProc;
      data : in ClientData);
   pragma Import (C, Tcl_CreateFileHandler, "Tcl_CreateFileHandler");

   --  UNIX

   --  UNIX

   --  10

   procedure Tcl_DeleteFileHandler (fd : in C.int);
   pragma Import (C, Tcl_DeleteFileHandler, "Tcl_DeleteFileHandler");

   --  UNIX

   --  11

   procedure Tcl_SetTimer (timePtr : in Tcl_Time);
   pragma Import (C, Tcl_SetTimer, "Tcl_SetTimer");

   --  12

   procedure Tcl_Sleep (ms : in C.int);
   pragma Import (C, Tcl_Sleep, "Tcl_Sleep");

   --  13

   function Tcl_WaitForEvent (timePtr : in Tcl_Time) return C.int;
   pragma Import (C, Tcl_WaitForEvent, "Tcl_WaitForEvent");

   --  14

   function Tcl_AppendAllObjTypes
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_AppendAllObjTypes, "Tcl_AppendAllObjTypes");

   --  15

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
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr);

   --  16

   procedure Tcl_AppendToObj
     (objPtr : in Tcl_Obj;
      bytes  : in C.Strings.chars_ptr;
      length : in C.int);
   pragma Import (C, Tcl_AppendToObj, "Tcl_AppendToObj");

   --  17

   function Tcl_ConcatObj
     (objc : in C.int;
      objv : in Tcl_Obj_Array)
      return Tcl_Obj;
   pragma Import (C, Tcl_ConcatObj, "Tcl_ConcatObj");

   --  18

   function Tcl_ConvertToType
     (interp  : in Tcl_Interp;
      objPtr  : in Tcl_Obj;
      typePtr : in Tcl_ObjType)
      return    C.int;
   pragma Import (C, Tcl_ConvertToType, "Tcl_ConvertToType");

   --  19

   procedure Tcl_DbDecrRefCount
     (objPtr : in Tcl_Obj;
      file   : in C.Strings.chars_ptr;
      line   : in C.int);
   pragma Import (C, Tcl_DbDecrRefCount, "Tcl_DbDecrRefCount");

   --  20

   procedure Tcl_DbIncrRefCount
     (objPtr : in Tcl_Obj;
      file   : in C.Strings.chars_ptr;
      line   : in C.int);
   pragma Import (C, Tcl_DbIncrRefCount, "Tcl_DbIncrRefCount");

   --  21

   function Tcl_DbIsShared
     (objPtr : in Tcl_Obj;
      file   : in C.Strings.chars_ptr;
      line   : in C.int)
      return   C.int;
   pragma Import (C, Tcl_DbIsShared, "Tcl_DbIsShared");

   --  22

   function Tcl_DbNewBooleanObj
     (boolValue : in C.int;
      file      : in C.Strings.chars_ptr;
      line      : in C.int)
      return      Tcl_Obj;
   pragma Import (C, Tcl_DbNewBooleanObj, "Tcl_DbNewBooleanObj");

   --  23

   function Tcl_DbNewByteArrayObj
     (bytes  : in C.Strings.chars_ptr;
      length : in C.int;
      file   : in C.Strings.chars_ptr;
      line   : in C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_DbNewByteArrayObj, "Tcl_DbNewByteArrayObj");

   --  24

   function Tcl_DbNewDoubleObj
     (doubleValue : in C.double;
      file        : in C.Strings.chars_ptr;
      line        : in C.int)
      return        Tcl_Obj;
   pragma Import (C, Tcl_DbNewDoubleObj, "Tcl_DbNewDoubleObj");

   --  25

   function Tcl_DbNewListObj
     (objc : in C.int;
      objv : in Tcl_Obj_Array;
      file : in C.Strings.chars_ptr;
      line : in C.int)
      return Tcl_Obj;
   pragma Import (C, Tcl_DbNewListObj, "Tcl_DbNewListObj");

   --  26

   function Tcl_DbNewLongObj
     (longValue : in C.long;
      file      : in C.Strings.chars_ptr;
      line      : in C.int)
      return      Tcl_Obj;
   pragma Import (C, Tcl_DbNewLongObj, "Tcl_DbNewLongObj");

   --  27

   function Tcl_DbNewObj
     (file : in C.Strings.chars_ptr;
      line : in C.int)
      return Tcl_Obj;
   pragma Import (C, Tcl_DbNewObj, "Tcl_DbNewObj");

   --  28

   function Tcl_DbNewStringObj
     (bytes  : in C.Strings.chars_ptr;
      length : in C.int;
      file   : in C.Strings.chars_ptr;
      line   : in C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_DbNewStringObj, "Tcl_DbNewStringObj");

   --  29

   function Tcl_DuplicateObj (objPtr : in Tcl_Obj) return Tcl_Obj;
   pragma Import (C, Tcl_DuplicateObj, "Tcl_DuplicateObj");

   --  30

   procedure TclFreeObj (objPtr : in Tcl_Obj);
   pragma Import (C, TclFreeObj, "TclFreeObj");

   --  31

   function Tcl_GetBoolean
     (interp  : in Tcl_Interp;
      str     : in C.Strings.chars_ptr;
      boolPtr : access C.int)
      return    C.int;
   pragma Import (C, Tcl_GetBoolean, "Tcl_GetBoolean");

   --  32

   function Tcl_GetBooleanFromObj
     (interp  : in Tcl_Interp;
      objPtr  : in Tcl_Obj;
      boolPtr : access C.int)
      return    C.int;
   pragma Import (C, Tcl_GetBooleanFromObj, "Tcl_GetBooleanFromObj");

   --  33

   function Tcl_GetByteArrayFromObj
     (objPtr    : in Tcl_Obj;
      lengthPtr : access C.int)
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetByteArrayFromObj, "Tcl_GetByteArrayFromObj");

   --  34

   function Tcl_GetDouble
     (interp    : in Tcl_Interp;
      str       : in C.Strings.chars_ptr;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tcl_GetDouble, "Tcl_GetDouble");

   --  35

   function Tcl_GetDoubleFromObj
     (interp    : in Tcl_Interp;
      objPtr    : in Tcl_Obj;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tcl_GetDoubleFromObj, "Tcl_GetDoubleFromObj");

   --  36

   function Tcl_GetIndexFromObj
     (interp   : in Tcl_Interp;
      objPtr   : in Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      msg      : in C.Strings.chars_ptr;
      flags    : in C.int;
      indexPtr : access C.int)
      return     C.int;
   pragma Import (C, Tcl_GetIndexFromObj, "Tcl_GetIndexFromObj");

   --  37

   function Tcl_GetInt
     (interp : in Tcl_Interp;
      str    : in C.Strings.chars_ptr;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tcl_GetInt, "Tcl_GetInt");

   --  38

   function Tcl_GetIntFromObj
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tcl_GetIntFromObj, "Tcl_GetIntFromObj");

   --  39

   function Tcl_GetLongFromObj
     (interp  : in Tcl_Interp;
      objPtr  : in Tcl_Obj;
      longPtr : access C.long)
      return    C.int;
   pragma Import (C, Tcl_GetLongFromObj, "Tcl_GetLongFromObj");

   --  40

   function Tcl_GetObjType
     (typeName : in C.Strings.chars_ptr)
      return     Tcl_ObjType;
   pragma Import (C, Tcl_GetObjType, "Tcl_GetObjType");

   function Tcl_GetObjTypeName
     (objPtr : in Tcl_Obj)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetObjTypeName, "Tcl_GetObjTypeName");

   --  41

   function Tcl_GetStringFromObj
     (objPtr    : in Tcl_Obj;
      lengthPtr : access C.int)
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetStringFromObj, "Tcl_GetStringFromObj");

   --  42

   procedure Tcl_InvalidateStringRep (objPtr : in Tcl_Obj);
   pragma Import (C, Tcl_InvalidateStringRep, "Tcl_InvalidateStringRep");

   --  43

   function Tcl_ListObjAppendList
     (interp      : in Tcl_Interp;
      listPtr     : in Tcl_Obj;
      elemListPtr : in Tcl_Obj)
      return        C.int;
   pragma Import (C, Tcl_ListObjAppendList, "Tcl_ListObjAppendList");

   --  44

   function Tcl_ListObjAppendElement
     (interp  : in Tcl_Interp;
      listPtr : in Tcl_Obj;
      objPtr  : in Tcl_Obj)
      return    C.int;
   pragma Import (C, Tcl_ListObjAppendElement, "Tcl_ListObjAppendElement");

   --  45

   function Tcl_ListObjGetElements
     (interp  : in Tcl_Interp;
      listPtr : in Tcl_Obj;
      objcPtr : access C.int;
      objv    : in Tcl_Obj_Array)
      return    C.int;
   pragma Import (C, Tcl_ListObjGetElements, "Tcl_ListObjGetElements");

   --  46

   function Tcl_ListObjIndex
     (interp    : in Tcl_Interp;
      listPtr   : in Tcl_Obj;
      index     : in C.int;
      objPtrPtr : access Tcl_Obj)
      return      C.int;
   pragma Import (C, Tcl_ListObjIndex, "Tcl_ListObjIndex");

   --  47

   function Tcl_ListObjLength
     (interp  : in Tcl_Interp;
      listPtr : in Tcl_Obj;
      intPtr  : access C.int)
      return    C.int;
   pragma Import (C, Tcl_ListObjLength, "Tcl_ListObjLength");

   --  48

   function Tcl_ListObjReplace
     (interp  : in Tcl_Interp;
      listPtr : in Tcl_Obj;
      first   : in C.int;
      count   : in C.int;
      objc    : in C.int;
      objv    : in Tcl_Obj_Array)
      return    C.int;
   pragma Import (C, Tcl_ListObjReplace, "Tcl_ListObjReplace");

   --  49

   function Tcl_NewBooleanObj (boolValue : in C.int) return Tcl_Obj;
   pragma Import (C, Tcl_NewBooleanObj, "Tcl_NewBooleanObj");

   --  50

   function Tcl_NewByteArrayObj
     (bytes  : in C.Strings.chars_ptr;
      length : in C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_NewByteArrayObj, "Tcl_NewByteArrayObj");

   --  51

   function Tcl_NewDoubleObj (doubleValue : in C.double) return Tcl_Obj;
   pragma Import (C, Tcl_NewDoubleObj, "Tcl_NewDoubleObj");

   --  52

   function Tcl_NewIntObj (intValue : in C.int) return Tcl_Obj;
   pragma Import (C, Tcl_NewIntObj, "Tcl_NewIntObj");

   --  53

   function Tcl_NewListObj
     (objc : in C.int;
      objv : in Tcl_Obj_Array)
      return Tcl_Obj;
   pragma Import (C, Tcl_NewListObj, "Tcl_NewListObj");

   --  54

   function Tcl_NewLongObj (longValue : in C.long) return Tcl_Obj;
   pragma Import (C, Tcl_NewLongObj, "Tcl_NewLongObj");

   --  55

   function Tcl_NewObj return Tcl_Obj;
   pragma Import (C, Tcl_NewObj, "Tcl_NewObj");

   --  56

   function Tcl_NewStringObj
     (bytes  : in C.Strings.chars_ptr;
      length : in C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_NewStringObj, "Tcl_NewStringObj");

   --  57

   procedure Tcl_SetBooleanObj (objPtr : in Tcl_Obj; boolValue : in C.int);
   pragma Import (C, Tcl_SetBooleanObj, "Tcl_SetBooleanObj");

   --  58

   function Tcl_SetByteArrayLength
     (objPtr : in Tcl_Obj;
      length : in C.int)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_SetByteArrayLength, "Tcl_SetByteArrayLength");

   --  59

   procedure Tcl_SetByteArrayObj
     (objPtr : in Tcl_Obj;
      bytes  : in C.Strings.chars_ptr;
      length : in C.int);
   pragma Import (C, Tcl_SetByteArrayObj, "Tcl_SetByteArrayObj");

   --  60

   procedure Tcl_SetDoubleObj
     (objPtr      : in Tcl_Obj;
      doubleValue : in C.double);
   pragma Import (C, Tcl_SetDoubleObj, "Tcl_SetDoubleObj");

   --  61

   procedure Tcl_SetIntObj (objPtr : in Tcl_Obj; intValue : in C.int);
   pragma Import (C, Tcl_SetIntObj, "Tcl_SetIntObj");

   --  62

   procedure Tcl_SetListObj
     (objPtr : in Tcl_Obj;
      objc   : in C.int;
      objv   : in Tcl_Obj_Array);
   pragma Import (C, Tcl_SetListObj, "Tcl_SetListObj");

   --  63

   procedure Tcl_SetLongObj (objPtr : in Tcl_Obj; longValue : in C.long);
   pragma Import (C, Tcl_SetLongObj, "Tcl_SetLongObj");

   --  64

   procedure Tcl_SetObjLength (objPtr : in Tcl_Obj; length : in C.int);
   pragma Import (C, Tcl_SetObjLength, "Tcl_SetObjLength");

   --  65

   procedure Tcl_SetStringObj
     (objPtr : in Tcl_Obj;
      bytes  : in C.Strings.chars_ptr;
      length : in C.int);
   pragma Import (C, Tcl_SetStringObj, "Tcl_SetStringObj");

   --  66

   procedure Tcl_AddErrorInfo
     (interp  : in Tcl_Interp;
      message : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_AddErrorInfo, "Tcl_AddErrorInfo");

   --  67

   procedure Tcl_AddObjErrorInfo
     (interp  : in Tcl_Interp;
      message : in C.Strings.chars_ptr;
      length  : in C.int);
   pragma Import (C, Tcl_AddObjErrorInfo, "Tcl_AddObjErrorInfo");

   --  68

   procedure Tcl_AllowExceptions (interp : in Tcl_Interp);
   pragma Import (C, Tcl_AllowExceptions, "Tcl_AllowExceptions");

   --  69

   procedure Tcl_AppendElement
     (interp : in Tcl_Interp;
      strng  : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_AppendElement, "Tcl_AppendElement");

   --  70

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
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr);

   --  71

   function Tcl_AsyncCreate
     (proc : in Tcl_AsyncProc;
      data : in ClientData)
      return Tcl_AsyncHandler;
   pragma Import (C, Tcl_AsyncCreate, "Tcl_AsyncCreate");

   --  72

   procedure Tcl_AsyncDelete (async : in Tcl_AsyncHandler);
   pragma Import (C, Tcl_AsyncDelete, "Tcl_AsyncDelete");

   --  73

   function Tcl_AsyncInvoke
     (interp : in Tcl_Interp;
      code   : in C.int)
      return   C.int;
   pragma Import (C, Tcl_AsyncInvoke, "Tcl_AsyncInvoke");

   --  74

   procedure Tcl_AsyncMark (async : in Tcl_AsyncHandler);
   pragma Import (C, Tcl_AsyncMark, "Tcl_AsyncMark");

   --  75

   function Tcl_AsyncReady return C.int;
   pragma Import (C, Tcl_AsyncReady, "Tcl_AsyncReady");

   --  76

   procedure Tcl_BackgroundError (interp : in Tcl_Interp);
   pragma Import (C, Tcl_BackgroundError, "Tcl_BackgroundError");

   --  77

   function Tcl_Backslash
     (src     : in C.Strings.chars_ptr;
      readPtr : access C.int)
      return    C.char;
   pragma Import (C, Tcl_Backslash, "Tcl_Backslash");

   --  78

   function Tcl_BadChannelOption
     (interp     : in Tcl_Interp;
      optionName : in C.Strings.chars_ptr;
      optionList : in C.Strings.chars_ptr)
      return       C.int;
   pragma Import (C, Tcl_BadChannelOption, "Tcl_BadChannelOption");

   --  79

   procedure Tcl_CallWhenDeleted
     (interp : in Tcl_Interp;
      proc   : in Tcl_InterpDeleteProc;
      data   : in ClientData);
   pragma Import (C, Tcl_CallWhenDeleted, "Tcl_CallWhenDeleted");

   --  80

   procedure Tcl_CancelIdleCall
     (idleProc : in Tcl_IdleProc;
      data     : in ClientData);
   pragma Import (C, Tcl_CancelIdleCall, "Tcl_CancelIdleCall");

   --  81

   function Tcl_Close
     (interp : in Tcl_Interp;
      chan   : in Tcl_Channel)
      return   C.int;
   pragma Import (C, Tcl_Close, "Tcl_Close");

   --  82

   function Tcl_CommandComplete (cmd : in C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_CommandComplete, "Tcl_CommandComplete");

   --  83

   function Tcl_Concat
     (argc : in C.int;
      argv : in CArgv.Chars_Ptr_Ptr)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_Concat, "Tcl_Concat");

   --  84

   function Tcl_ConvertElement
     (src   : in C.Strings.chars_ptr;
      dst   : in C.Strings.chars_ptr;
      flags : in C.int)
      return  C.int;
   pragma Import (C, Tcl_ConvertElement, "Tcl_ConvertElement");

   --  85

   function Tcl_ConvertCountedElement
     (src    : in C.Strings.chars_ptr;
      length : in C.int;
      dst    : in C.Strings.chars_ptr;
      flags  : in C.int)
      return   C.int;
   pragma Import (C, Tcl_ConvertCountedElement, "Tcl_ConvertCountedElement");

   --  86

   function Tcl_CreateAlias
     (slave     : in Tcl_Interp;
      slaveCmd  : in C.Strings.chars_ptr;
      target    : in Tcl_Interp;
      targetCmd : in C.Strings.chars_ptr;
      argc      : in C.int;
      argv      : in CArgv.Chars_Ptr_Ptr)
      return      C.int;
   pragma Import (C, Tcl_CreateAlias, "Tcl_CreateAlias");

   --  87

   function Tcl_CreateAliasObj
     (slave     : in Tcl_Interp;
      slaveCmd  : in C.Strings.chars_ptr;
      target    : in Tcl_Interp;
      targetCmd : in C.Strings.chars_ptr;
      objc      : in C.int;
      objv      : in Tcl_Obj_Array)
      return      C.int;
   pragma Import (C, Tcl_CreateAliasObj, "Tcl_CreateAliasObj");

   --  88

   function Tcl_CreateChannel
     (typePtr      : in Tcl_ChannelType;
      chanName     : in C.Strings.chars_ptr;
      instancedata : in ClientData;
      mask         : in C.int)
      return         Tcl_Channel;
   pragma Import (C, Tcl_CreateChannel, "Tcl_CreateChannel");

   --  89

   procedure Tcl_CreateChannelHandler
     (chan : in Tcl_Channel;
      mask : in C.int;
      proc : in Tcl_ChannelProc;
      data : in ClientData);
   pragma Import (C, Tcl_CreateChannelHandler, "Tcl_CreateChannelHandler");

   --  90

   procedure Tcl_CreateCloseHandler
     (chan : in Tcl_Channel;
      proc : in Tcl_CloseProc;
      data : in ClientData);
   pragma Import (C, Tcl_CreateCloseHandler, "Tcl_CreateCloseHandler");

   --  91

   function Tcl_CreateCommand
     (interp     : in Tcl_Interp;
      cmdName    : in C.Strings.chars_ptr;
      proc       : in Tcl_CmdProc;
      data       : in ClientData;
      deleteProc : in Tcl_CmdDeleteProc)
      return       Tcl_Command;
   pragma Import (C, Tcl_CreateCommand, "Tcl_CreateCommand");

   --  92

   procedure Tcl_CreateEventSource
     (setupProc : in Tcl_EventSetupProc;
      checkProc : in Tcl_EventCheckProc;
      data      : in ClientData);
   pragma Import (C, Tcl_CreateEventSource, "Tcl_CreateEventSource");

   --  93

   procedure Tcl_CreateExitHandler
     (proc : in Tcl_ExitProc;
      data : in ClientData);
   pragma Import (C, Tcl_CreateExitHandler, "Tcl_CreateExitHandler");

   --  94

   function Tcl_CreateInterp return Tcl_Interp;
   pragma Import (C, Tcl_CreateInterp, "Tcl_CreateInterp");

   --  95

   procedure Tcl_CreateMathFunc
     (interp   : in Tcl_Interp;
      name     : in C.Strings.chars_ptr;
      numArgs  : in C.int;
      argTypes : in Tcl_ValueType;
      proc     : in Tcl_MathProc;
      data     : in ClientData);
   pragma Import (C, Tcl_CreateMathFunc, "Tcl_CreateMathFunc");

   --  96

   function Tcl_CreateObjCommand
     (interp     : in Tcl_Interp;
      cmdName    : in C.Strings.chars_ptr;
      proc       : in Tcl_ObjCmdProc;
      data       : in ClientData;
      deleteProc : in Tcl_CmdDeleteProc)
      return       Tcl_Command;
   pragma Import (C, Tcl_CreateObjCommand, "Tcl_CreateObjCommand");

   --  97

   function Tcl_CreateSlave
     (interp    : in Tcl_Interp;
      slaveName : in C.Strings.chars_ptr;
      isSafe    : in C.int)
      return      Tcl_Interp;
   pragma Import (C, Tcl_CreateSlave, "Tcl_CreateSlave");

   --  98

   function Tcl_CreateTimerHandler
     (milliseconds : in C.int;
      proc         : in Tcl_TimerProc;
      data         : in ClientData)
      return         Tcl_TimerToken;
   pragma Import (C, Tcl_CreateTimerHandler, "Tcl_CreateTimerHandler");

   --  99

   function Tcl_CreateTrace
     (interp : in Tcl_Interp;
      level  : in C.int;
      proc   : in Tcl_CmdTraceProc;
      data   : in ClientData)
      return   Tcl_Trace;
   pragma Import (C, Tcl_CreateTrace, "Tcl_CreateTrace");

   --  100

   procedure Tcl_DeleteAssocData
     (interp : in Tcl_Interp;
      name   : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_DeleteAssocData, "Tcl_DeleteAssocData");

   --  101

   procedure Tcl_DeleteChannelHandler
     (chan : in Tcl_Channel;
      proc : in Tcl_ChannelProc;
      data : in ClientData);
   pragma Import (C, Tcl_DeleteChannelHandler, "Tcl_DeleteChannelHandler");

   --  102

   procedure Tcl_DeleteCloseHandler
     (chan : in Tcl_Channel;
      proc : in Tcl_CloseProc;
      data : in ClientData);
   pragma Import (C, Tcl_DeleteCloseHandler, "Tcl_DeleteCloseHandler");

   --  103

   function Tcl_DeleteCommand
     (interp  : in Tcl_Interp;
      cmdName : in C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_DeleteCommand, "Tcl_DeleteCommand");

   --  104

   function Tcl_DeleteCommandFromToken
     (interp  : in Tcl_Interp;
      command : in Tcl_Command)
      return    C.int;
   pragma Import
     (C,
      Tcl_DeleteCommandFromToken,
      "Tcl_DeleteCommandFromToken");

   --  105

   procedure Tcl_DeleteEvents
     (proc : in Tcl_EventDeleteProc;
      data : in ClientData);
   pragma Import (C, Tcl_DeleteEvents, "Tcl_DeleteEvents");

   --  106

   procedure Tcl_DeleteEventSource
     (setupProc : in Tcl_EventSetupProc;
      checkProc : in Tcl_EventCheckProc;
      data      : in ClientData);
   pragma Import (C, Tcl_DeleteEventSource, "Tcl_DeleteEventSource");

   --  107

   procedure Tcl_DeleteExitHandler
     (proc : in Tcl_ExitProc;
      data : in ClientData);
   pragma Import (C, Tcl_DeleteExitHandler, "Tcl_DeleteExitHandler");

   --  108

   procedure Tcl_DeleteHashEntry (entryPtr : in Tcl_HashEntry);
   pragma Import (C, Tcl_DeleteHashEntry, "Tcl_DeleteHashEntry");

   --  109

   procedure Tcl_DeleteHashTable (tablePtr : in Tcl_HashTable);
   pragma Import (C, Tcl_DeleteHashTable, "Tcl_DeleteHashTable");

   --  110

   procedure Tcl_DeleteInterp (interp : in Tcl_Interp);
   pragma Import (C, Tcl_DeleteInterp, "Tcl_DeleteInterp");

   --  UNIX

   --  111

   procedure Tcl_DetachPids (numPids : in C.int; pidPtr : in Tcl_Pid);
   pragma Import (C, Tcl_DetachPids, "Tcl_DetachPids");

   --  UNIX

   --  __WIN32__

   --  112

   procedure Tcl_DeleteTimerHandler (token : in Tcl_TimerToken);
   pragma Import (C, Tcl_DeleteTimerHandler, "Tcl_DeleteTimerHandler");

   --  113

   procedure Tcl_DeleteTrace (interp : in Tcl_Interp; trace : in Tcl_Trace);
   pragma Import (C, Tcl_DeleteTrace, "Tcl_DeleteTrace");

   --  114

   procedure Tcl_DontCallWhenDeleted
     (interp : in Tcl_Interp;
      proc   : in Tcl_InterpDeleteProc;
      data   : in ClientData);
   pragma Import (C, Tcl_DontCallWhenDeleted, "Tcl_DontCallWhenDeleted");

   --  115

   function Tcl_DoOneEvent (flags : in C.int) return C.int;
   pragma Import (C, Tcl_DoOneEvent, "Tcl_DoOneEvent");

   --  116

   procedure Tcl_DoWhenIdle (proc : in Tcl_IdleProc; data : in ClientData);
   pragma Import (C, Tcl_DoWhenIdle, "Tcl_DoWhenIdle");

   --  117

   function Tcl_DStringAppend
     (dsPtr  : in Tcl_DString;
      str    : in C.Strings.chars_ptr;
      length : in C.int)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_DStringAppend, "Tcl_DStringAppend");

   --  118

   function Tcl_DStringAppendElement
     (dsPtr : in Tcl_DString;
      strng : in C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_DStringAppendElement, "Tcl_DStringAppendElement");

   --  119

   procedure Tcl_DStringEndSublist (dsPtr : in Tcl_DString);
   pragma Import (C, Tcl_DStringEndSublist, "Tcl_DStringEndSublist");

   --  120

   procedure Tcl_DStringFree (dsPtr : in Tcl_DString);
   pragma Import (C, Tcl_DStringFree, "Tcl_DStringFree");

   --  121

   procedure Tcl_DStringGetResult
     (interp : in Tcl_Interp;
      dsPtr  : in Tcl_DString);
   pragma Import (C, Tcl_DStringGetResult, "Tcl_DStringGetResult");

   --  122

   procedure Tcl_DStringInit (dsPtr : in Tcl_DString);
   pragma Import (C, Tcl_DStringInit, "Tcl_DStringInit");

   --  123

   procedure Tcl_DStringResult
     (interp : in Tcl_Interp;
      dsPtr  : in Tcl_DString);
   pragma Import (C, Tcl_DStringResult, "Tcl_DStringResult");

   --  124

   --  125

   procedure Tcl_DStringStartSublist (dsPtr : in Tcl_DString);
   pragma Import (C, Tcl_DStringStartSublist, "Tcl_DStringStartSublist");

   --  126

   function Tcl_Eof (chan : in Tcl_Channel) return C.int;
   pragma Import (C, Tcl_Eof, "Tcl_Eof");

   --  127

   function Tcl_ErrnoId return C.Strings.chars_ptr;
   pragma Import (C, Tcl_ErrnoId, "Tcl_ErrnoId");

   --  128

   function Tcl_ErrnoMsg (err : in C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_ErrnoMsg, "Tcl_ErrnoMsg");

   --  129

   function Tcl_Eval
     (interp : in Tcl_Interp;
      strng  : in C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_Eval, "Tcl_Eval");

   --  130

   function Tcl_EvalFile
     (interp   : in Tcl_Interp;
      fileName : in C.Strings.chars_ptr)
      return     C.int;
   pragma Import (C, Tcl_EvalFile, "Tcl_EvalFile");

   --  131

   function Tcl_EvalObj
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_EvalObj, "Tcl_EvalObj");

   --  132

   procedure Tcl_EventuallyFree
     (data     : in ClientData;
      freeProc : in Tcl_FreeProc);
   pragma Import (C, Tcl_EventuallyFree, "Tcl_EventuallyFree");

   --  133

   procedure Tcl_Exit (status : in C.int);
   pragma Import (C, Tcl_Exit, "Tcl_Exit");

   --  134

   function Tcl_ExposeCommand
     (interp         : in Tcl_Interp;
      hiddenCmdToken : in C.Strings.chars_ptr;
      cmdName        : in C.Strings.chars_ptr)
      return           C.int;
   pragma Import (C, Tcl_ExposeCommand, "Tcl_ExposeCommand");

   --  135

   function Tcl_ExprBoolean
     (interp : in Tcl_Interp;
      str    : in C.Strings.chars_ptr;
      ptr    : access C.int)
      return   C.int;
   pragma Import (C, Tcl_ExprBoolean, "Tcl_ExprBoolean");

   --  136

   function Tcl_ExprBooleanObj
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj;
      ptr    : access C.int)
      return   C.int;
   pragma Import (C, Tcl_ExprBooleanObj, "Tcl_ExprBooleanObj");

   --  137

   function Tcl_ExprDouble
     (interp : in Tcl_Interp;
      str    : in C.Strings.chars_ptr;
      ptr    : access C.double)
      return   C.int;
   pragma Import (C, Tcl_ExprDouble, "Tcl_ExprDouble");

   --  138

   function Tcl_ExprDoubleObj
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj;
      ptr    : access C.double)
      return   C.int;
   pragma Import (C, Tcl_ExprDoubleObj, "Tcl_ExprDoubleObj");

   --  139

   function Tcl_ExprLong
     (interp : in Tcl_Interp;
      str    : in C.Strings.chars_ptr;
      ptr    : access C.long)
      return   C.int;
   pragma Import (C, Tcl_ExprLong, "Tcl_ExprLong");

   --  140

   function Tcl_ExprLongObj
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj;
      ptr    : access C.long)
      return   C.int;
   pragma Import (C, Tcl_ExprLongObj, "Tcl_ExprLongObj");

   --  141

   function Tcl_ExprObj
     (interp       : in Tcl_Interp;
      objPtr       : in Tcl_Obj;
      resultPtrPtr : access Tcl_Obj)
      return         C.int;
   pragma Import (C, Tcl_ExprObj, "Tcl_ExprObj");

   --  142

   function Tcl_ExprString
     (interp : in Tcl_Interp;
      strng  : in C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_ExprString, "Tcl_ExprString");

   --  143

   procedure Tcl_Finalize;
   pragma Import (C, Tcl_Finalize, "Tcl_Finalize");

   --  144

   procedure Tcl_FindExecutable (argv0 : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_FindExecutable, "Tcl_FindExecutable");

   --  145

   function Tcl_FirstHashEntry
     (tablePtr  : in Tcl_HashTable;
      searchPtr : in Tcl_HashSearch)
      return      Tcl_HashEntry;
   pragma Import (C, Tcl_FirstHashEntry, "Tcl_FirstHashEntry");

   --  146

   function Tcl_Flush (chan : in Tcl_Channel) return C.int;
   pragma Import (C, Tcl_Flush, "Tcl_Flush");

   --  147

   procedure Tcl_FreeResult (interp : in Tcl_Interp);
   pragma Import (C, Tcl_FreeResult, "Tcl_FreeResult");

   --  148

   function Tcl_GetAlias
     (interp          : in Tcl_Interp;
      slaveCmd        : in C.Strings.chars_ptr;
      targetInterpPtr : in Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      argcPtr         : access C.int;
      argvPtr         : access CArgv.Chars_Ptr_Ptr)
      return            C.int;
   pragma Import (C, Tcl_GetAlias, "Tcl_GetAlias");

   --  149

   function Tcl_GetAliasObj
     (interp          : in Tcl_Interp;
      slaveCmd        : in C.Strings.chars_ptr;
      targetInterpPtr : in Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      objcPtr         : access C.int;
      objv            : in Tcl_Obj_Array)
      return            C.int;
   pragma Import (C, Tcl_GetAliasObj, "Tcl_GetAliasObj");

   --  150

   function Tcl_GetAssocData
     (interp  : in Tcl_Interp;
      name    : in C.Strings.chars_ptr;
      procPtr : in Tcl_InterpDeleteProc)
      return    ClientData;
   pragma Import (C, Tcl_GetAssocData, "Tcl_GetAssocData");

   --  151

   function Tcl_GetChannel
     (interp   : in Tcl_Interp;
      chanName : in C.Strings.chars_ptr;
      modePtr  : access C.int)
      return     Tcl_Channel;
   pragma Import (C, Tcl_GetChannel, "Tcl_GetChannel");

   --  152

   function Tcl_GetChannelBufferSize (chan : in Tcl_Channel) return C.int;
   pragma Import (C, Tcl_GetChannelBufferSize, "Tcl_GetChannelBufferSize");

   --  153

   function Tcl_GetChannelHandle
     (chan      : in Tcl_Channel;
      direction : in C.int;
      handleptr : in ClientData)
      return      C.int;
   pragma Import (C, Tcl_GetChannelHandle, "Tcl_GetChannelHandle");

   --  154

   function Tcl_GetChannelInstanceData
     (chan : in Tcl_Channel)
      return ClientData;
   pragma Import
     (C,
      Tcl_GetChannelInstanceData,
      "Tcl_GetChannelInstanceData");

   --  155

   function Tcl_GetChannelMode (chan : in Tcl_Channel) return C.int;
   pragma Import (C, Tcl_GetChannelMode, "Tcl_GetChannelMode");

   --  156

   function Tcl_GetChannelName
     (chan : in Tcl_Channel)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetChannelName, "Tcl_GetChannelName");

   --  157

   function Tcl_GetChannelOption
     (interp     : in Tcl_Interp;
      chan       : in Tcl_Channel;
      optionName : in C.Strings.chars_ptr;
      dsPtr      : in Tcl_DString)
      return       C.int;
   pragma Import (C, Tcl_GetChannelOption, "Tcl_GetChannelOption");

   --  158

   function Tcl_GetChannelType
     (chan : in Tcl_Channel)
      return Tcl_ChannelType;
   pragma Import (C, Tcl_GetChannelType, "Tcl_GetChannelType");

   --  159

   function Tcl_GetCommandInfo
     (interp  : in Tcl_Interp;
      cmdName : in C.Strings.chars_ptr;
      infoPtr : in Tcl_CmdInfo)
      return    C.int;
   pragma Import (C, Tcl_GetCommandInfo, "Tcl_GetCommandInfo");

   --  160

   function Tcl_GetCommandName
     (interp  : in Tcl_Interp;
      command : in Tcl_Command)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetCommandName, "Tcl_GetCommandName");

   --  161

   function Tcl_GetErrno return C.int;
   pragma Import (C, Tcl_GetErrno, "Tcl_GetErrno");

   --  162

   function Tcl_GetHostName return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetHostName, "Tcl_GetHostName");

   function Tcl_GetObjId (objPtr : in Tcl_Obj) return C.int;
   pragma Import (C, Tcl_GetObjId, "Tcl_GetObjId");

   --  163

   function Tcl_GetInterpPath
     (askInterp   : in Tcl_Interp;
      slaveInterp : in Tcl_Interp)
      return        C.int;
   pragma Import (C, Tcl_GetInterpPath, "Tcl_GetInterpPath");

   --  164

   function Tcl_GetMaster (interp : in Tcl_Interp) return Tcl_Interp;
   pragma Import (C, Tcl_GetMaster, "Tcl_GetMaster");

   --  165

   function Tcl_GetNameOfExecutable return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetNameOfExecutable, "Tcl_GetNameOfExecutable");

   --  166

   function Tcl_GetObjResult (interp : in Tcl_Interp) return Tcl_Obj;
   pragma Import (C, Tcl_GetObjResult, "Tcl_GetObjResult");

   --  UNIX

   --  167

   procedure Tcl_GetOpenFile
     (interp     : in Tcl_Interp;
      str        : in C.Strings.chars_ptr;
      forWriting : in C.int;
      checkUsage : in C.int;
      fileptr    : in ClientData);
   pragma Import (C, Tcl_GetOpenFile, "Tcl_GetOpenFile");

   --  UNIX

   --  168

   function Tcl_GetPathType
     (path : in C.Strings.chars_ptr)
      return Tcl_PathType;
   pragma Import (C, Tcl_GetPathType, "Tcl_GetPathType");

   function Tcl_GetRefCount (objPtr : in Tcl_Obj) return C.int;
   pragma Import (C, Tcl_GetRefCount, "Tcl_GetRefCount");

   function Tcl_GetResult
     (interp : in Tcl_Interp)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetResult, "Tcl_GetStringResult");

   --  169

   function Tcl_Gets
     (chan  : in Tcl_Channel;
      dsPtr : in Tcl_DString)
      return  C.int;
   pragma Import (C, Tcl_Gets, "Tcl_Gets");

   --  170

   function Tcl_GetsObj
     (chan   : in Tcl_Channel;
      objPtr : in Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_GetsObj, "Tcl_GetsObj");

   --  171

   function Tcl_GetServiceMode return C.int;
   pragma Import (C, Tcl_GetServiceMode, "Tcl_GetServiceMode");

   --  172

   function Tcl_GetSlave
     (interp    : in Tcl_Interp;
      slaveName : in C.Strings.chars_ptr)
      return      Tcl_Interp;
   pragma Import (C, Tcl_GetSlave, "Tcl_GetSlave");

   --  173

   function Tcl_GetStdChannel (typ : in C.int) return Tcl_Channel;
   pragma Import (C, Tcl_GetStdChannel, "Tcl_GetStdChannel");

   --  174

   function Tcl_GetStringResult
     (interp : in Tcl_Interp)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetStringResult, "Tcl_GetStringResult");

   --  175

   function Tcl_GetVar
     (interp  : in Tcl_Interp;
      varName : in C.Strings.chars_ptr;
      flags   : in C.int)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetVar, "Tcl_GetVar");

   --  176

   function Tcl_GetVar2
     (interp : in Tcl_Interp;
      part1  : in C.Strings.chars_ptr;
      part2  : in C.Strings.chars_ptr;
      flags  : in C.int)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetVar2, "Tcl_GetVar2");

   --  177

   function Tcl_GlobalEval
     (interp  : in Tcl_Interp;
      command : in C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_GlobalEval, "Tcl_GlobalEval");

   --  178

   function Tcl_GlobalEvalObj
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_GlobalEvalObj, "Tcl_GlobalEvalObj");

   --  179

   function Tcl_HideCommand
     (interp         : in Tcl_Interp;
      cmdName        : in C.Strings.chars_ptr;
      hiddenCmdToken : in C.Strings.chars_ptr)
      return           C.int;
   pragma Import (C, Tcl_HideCommand, "Tcl_HideCommand");

   --  180

   function Tcl_Init (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tcl_Init, "Tcl_Init");

   --  181

   procedure Tcl_InitHashTable
     (tablePtr : in Tcl_HashTable;
      keyType  : in C.int);
   pragma Import (C, Tcl_InitHashTable, "Tcl_InitHashTable");

   --  182

   function Tcl_InputBlocked (chan : in Tcl_Channel) return C.int;
   pragma Import (C, Tcl_InputBlocked, "Tcl_InputBlocked");

   --  183

   function Tcl_InputBuffered (chan : in Tcl_Channel) return C.int;
   pragma Import (C, Tcl_InputBuffered, "Tcl_InputBuffered");

   --  184

   function Tcl_InterpDeleted (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tcl_InterpDeleted, "Tcl_InterpDeleted");

   --  185

   function Tcl_IsSafe (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tcl_IsSafe, "Tcl_IsSafe");

   --  186

   function Tcl_JoinPath
     (argc      : in C.int;
      argv      : in CArgv.Chars_Ptr_Ptr;
      resultPtr : in Tcl_DString)
      return      C.Strings.chars_ptr;
   pragma Import (C, Tcl_JoinPath, "Tcl_JoinPath");

   --  187

   function Tcl_LinkVar
     (interp  : in Tcl_Interp;
      varName : in C.Strings.chars_ptr;
      addr    : in System.Address;
      typ     : in C.int)
      return    C.int;
   pragma Import (C, Tcl_LinkVar, "Tcl_LinkVar");

   --  Slot 188 is reserved

   --  189

   function Tcl_MakeFileChannel
     (handle : in ClientData;
      mode   : in C.int)
      return   Tcl_Channel;
   pragma Import (C, Tcl_MakeFileChannel, "Tcl_MakeFileChannel");

   --  190

   function Tcl_MakeSafe (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tcl_MakeSafe, "Tcl_MakeSafe");

   --  191

   function Tcl_MakeTcpClientChannel
     (tcpsocket : in ClientData)
      return      Tcl_Channel;
   pragma Import (C, Tcl_MakeTcpClientChannel, "Tcl_MakeTcpClientChannel");

   --  192

   function Tcl_Merge
     (argc : in C.int;
      argv : in CArgv.Chars_Ptr_Ptr)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_Merge, "Tcl_Merge");

   --  193

   function Tcl_NextHashEntry
     (searchPtr : in Tcl_HashSearch)
      return      Tcl_HashEntry;
   pragma Import (C, Tcl_NextHashEntry, "Tcl_NextHashEntry");

   --  194

   procedure Tcl_NotifyChannel (channel : in Tcl_Channel; mask : in C.int);
   pragma Import (C, Tcl_NotifyChannel, "Tcl_NotifyChannel");

   --  195

   function Tcl_ObjGetVar2
     (interp   : in Tcl_Interp;
      part1Ptr : in Tcl_Obj;
      part2Ptr : in Tcl_Obj;
      flags    : in C.int)
      return     Tcl_Obj;
   pragma Import (C, Tcl_ObjGetVar2, "Tcl_ObjGetVar2");

   --  196

   function Tcl_ObjSetVar2
     (interp      : in Tcl_Interp;
      part1Ptr    : in Tcl_Obj;
      part2Ptr    : in Tcl_Obj;
      newValuePtr : in Tcl_Obj;
      flags       : in C.int)
      return        Tcl_Obj;
   pragma Import (C, Tcl_ObjSetVar2, "Tcl_ObjSetVar2");

   --  UNIX

   --  197

   function Tcl_OpenCommandChannel
     (interp : in Tcl_Interp;
      argc   : in C.int;
      argv   : in CArgv.Chars_Ptr_Ptr;
      flags  : in C.int)
      return   Tcl_Channel;
   pragma Import (C, Tcl_OpenCommandChannel, "Tcl_OpenCommandChannel");

   --  UNIX

   --  __WIN32__

   --  198

   function Tcl_OpenFileChannel
     (interp      : in Tcl_Interp;
      fileName    : in C.Strings.chars_ptr;
      modeString  : in C.Strings.chars_ptr;
      permissions : in C.int)
      return        Tcl_Channel;
   pragma Import (C, Tcl_OpenFileChannel, "Tcl_OpenFileChannel");

   --  199

   function Tcl_OpenTcpClient
     (interp  : in Tcl_Interp;
      port    : in C.int;
      address : in System.Address;
      myaddr  : in System.Address;
      myport  : in C.int;
      async   : in C.int)
      return    Tcl_Channel;
   pragma Import (C, Tcl_OpenTcpClient, "Tcl_OpenTcpClient");

   --  200

   function Tcl_OpenTcpServer
     (interp       : in Tcl_Interp;
      port         : in C.int;
      host         : in C.Strings.chars_ptr;
      acceptProc   : in Tcl_TcpAcceptProc;
      callbackdata : in ClientData)
      return         Tcl_Channel;
   pragma Import (C, Tcl_OpenTcpServer, "Tcl_OpenTcpServer");

   --  201

   procedure Tcl_Preserve (data : in ClientData);
   pragma Import (C, Tcl_Preserve, "Tcl_Preserve");

   --  202

   procedure Tcl_PrintDouble
     (interp : in Tcl_Interp;
      value  : in C.double;
      dst    : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_PrintDouble, "Tcl_PrintDouble");

   procedure Tcl_PrintObj (Ptr : in Tcl_Obj);
   pragma Import (C, Tcl_PrintObj, "Tcl_PrintObj");

   --  203

   function Tcl_PutEnv (strng : in C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_PutEnv, "Tcl_PutEnv");

   --  204

   function Tcl_PosixError
     (interp : in Tcl_Interp)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tcl_PosixError, "Tcl_PosixError");

   --  205

   procedure Tcl_QueueEvent
     (evPtr    : in Tcl_Event;
      position : in Tcl_QueuePosition);
   pragma Import (C, Tcl_QueueEvent, "Tcl_QueueEvent");

   --  206

   function Tcl_Read
     (chan   : in Tcl_Channel;
      bufPtr : in C.Strings.chars_ptr;
      toRead : in C.int)
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
     (interp : in Tcl_Interp;
      cmd    : in C.Strings.chars_ptr;
      flags  : in C.int)
      return   C.int;
   pragma Import (C, Tcl_RecordAndEval, "Tcl_RecordAndEval");

   --  209

   function Tcl_RecordAndEvalObj
     (interp : in Tcl_Interp;
      cmdPtr : in Tcl_Obj;
      flags  : in C.int)
      return   C.int;
   pragma Import (C, Tcl_RecordAndEvalObj, "Tcl_RecordAndEvalObj");

   --  210

   procedure Tcl_RegisterChannel
     (interp : in Tcl_Interp;
      chan   : in Tcl_Channel);
   pragma Import (C, Tcl_RegisterChannel, "Tcl_RegisterChannel");

   --  211

   procedure Tcl_RegisterObjType (typePtr : in Tcl_ObjType);
   pragma Import (C, Tcl_RegisterObjType, "Tcl_RegisterObjType");

   --  212

   function Tcl_RegExpCompile
     (interp : in Tcl_Interp;
      strng  : in C.Strings.chars_ptr)
      return   Tcl_RegExp;
   pragma Import (C, Tcl_RegExpCompile, "Tcl_RegExpCompile");

   --  213

   function Tcl_RegExpExec
     (interp : in Tcl_Interp;
      regexp : in Tcl_RegExp;
      str    : in C.Strings.chars_ptr;
      start  : in C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_RegExpExec, "Tcl_RegExpExec");

   --  214

   function Tcl_RegExpMatch
     (interp  : in Tcl_Interp;
      str     : in C.Strings.chars_ptr;
      pattern : in C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_RegExpMatch, "Tcl_RegExpMatch");

   --  215

   procedure Tcl_RegExpRange
     (regexp   : in Tcl_RegExp;
      index    : in C.int;
      startPtr : in CArgv.Chars_Ptr_Ptr;
      endPtr   : in CArgv.Chars_Ptr_Ptr);
   pragma Import (C, Tcl_RegExpRange, "Tcl_RegExpRange");

   --  216

   procedure Tcl_Release (data : in ClientData);
   pragma Import (C, Tcl_Release, "Tcl_Release");

   --  217

   procedure Tcl_ResetResult (interp : in Tcl_Interp);
   pragma Import (C, Tcl_ResetResult, "Tcl_ResetResult");

   --  218

   function Tcl_ScanElement
     (str     : in C.Strings.chars_ptr;
      flagPtr : access C.int)
      return    C.int;
   pragma Import (C, Tcl_ScanElement, "Tcl_ScanElement");

   --  219

   function Tcl_ScanCountedElement
     (str     : in C.Strings.chars_ptr;
      length  : in C.int;
      flagPtr : access C.int)
      return    C.int;
   pragma Import (C, Tcl_ScanCountedElement, "Tcl_ScanCountedElement");

   --  220

   function Tcl_Seek
     (chan   : in Tcl_Channel;
      offset : in C.int;
      mode   : in C.int)
      return   C.int;
   pragma Import (C, Tcl_Seek, "Tcl_Seek");

   --  221

   function Tcl_ServiceAll return C.int;
   pragma Import (C, Tcl_ServiceAll, "Tcl_ServiceAll");

   --  222

   function Tcl_ServiceEvent (flags : in C.int) return C.int;
   pragma Import (C, Tcl_ServiceEvent, "Tcl_ServiceEvent");

   --  223

   procedure Tcl_SetAssocData
     (interp : in Tcl_Interp;
      name   : in C.Strings.chars_ptr;
      proc   : in Tcl_InterpDeleteProc;
      data   : in ClientData);
   pragma Import (C, Tcl_SetAssocData, "Tcl_SetAssocData");

   --  224

   procedure Tcl_SetChannelBufferSize
     (chan : in Tcl_Channel;
      sz   : in C.int);
   pragma Import (C, Tcl_SetChannelBufferSize, "Tcl_SetChannelBufferSize");

   --  225

   function Tcl_SetChannelOption
     (interp     : in Tcl_Interp;
      chan       : in Tcl_Channel;
      optionName : in C.Strings.chars_ptr;
      newValue   : in C.Strings.chars_ptr)
      return       C.int;
   pragma Import (C, Tcl_SetChannelOption, "Tcl_SetChannelOption");

   --  226

   function Tcl_SetCommandInfo
     (interp  : in Tcl_Interp;
      cmdName : in C.Strings.chars_ptr;
      infoPtr : in Tcl_CmdInfo)
      return    C.int;
   pragma Import (C, Tcl_SetCommandInfo, "Tcl_SetCommandInfo");

   --  227

   procedure Tcl_SetErrno (err : in C.int);
   pragma Import (C, Tcl_SetErrno, "Tcl_SetErrno");

   --  228

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
      String9 : in C.Strings.chars_ptr := C.Strings.Null_Ptr);

   --  229

   procedure Tcl_SetMaxBlockTime (timePtr : in Tcl_Time);
   pragma Import (C, Tcl_SetMaxBlockTime, "Tcl_SetMaxBlockTime");

   --  230

   procedure Tcl_SetPanicProc (panicProc : in Tcl_PanicProc);
   pragma Import (C, Tcl_SetPanicProc, "Tcl_SetPanicProc");

   --  231

   function Tcl_SetRecursionLimit
     (interp : in Tcl_Interp;
      depth  : in C.int)
      return   C.int;
   pragma Import (C, Tcl_SetRecursionLimit, "Tcl_SetRecursionLimit");

   --  232

   procedure Tcl_SetResult
     (interp   : in Tcl_Interp;
      str      : in C.Strings.chars_ptr;
      freeProc : in Tcl_FreeProc);

   procedure Tcl_SetResult
     (interp   : in Tcl_Interp;
      str      : in C.Strings.chars_ptr;
      freeProc : in C.int);
   pragma Import (C, Tcl_Setresult, "Tcl_SetResult");

   --  233

   function Tcl_SetServiceMode (mode : in C.int) return C.int;
   pragma Import (C, Tcl_SetServiceMode, "Tcl_SetServiceMode");

   --  234

   procedure Tcl_SetObjErrorCode
     (interp      : in Tcl_Interp;
      errorObjPtr : in Tcl_Obj);
   pragma Import (C, Tcl_SetObjErrorCode, "Tcl_SetObjErrorCode");

   --  235

   procedure Tcl_SetObjResult
     (interp       : in Tcl_Interp;
      resultObjPtr : in Tcl_Obj);
   pragma Import (C, Tcl_SetObjResult, "Tcl_SetObjResult");

   --  236

   procedure Tcl_SetStdChannel (channel : in Tcl_Channel; typ : in C.int);
   pragma Import (C, Tcl_SetStdChannel, "Tcl_SetStdChannel");

   --  237

   function Tcl_SetVar
     (interp   : in Tcl_Interp;
      varName  : in C.Strings.chars_ptr;
      newValue : in C.Strings.chars_ptr;
      flags    : in C.int)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_SetVar, "Tcl_SetVar");

   --  238

   function Tcl_SetVar2
     (interp   : in Tcl_Interp;
      part1    : in C.Strings.chars_ptr;
      part2    : in C.Strings.chars_ptr;
      newValue : in C.Strings.chars_ptr;
      flags    : in C.int)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_SetVar2, "Tcl_SetVar2");

   --  239

   function Tcl_SignalId (sig : in C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_SignalId, "Tcl_SignalId");

   --  240

   function Tcl_SignalMsg (sig : in C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_SignalMsg, "Tcl_SignalMsg");

   --  241

   procedure Tcl_SourceRCFile (interp : in Tcl_Interp);
   pragma Import (C, Tcl_SourceRCFile, "Tcl_SourceRCFile");

   --  242

   function Tcl_SplitList
     (interp  : in Tcl_Interp;
      listStr : in C.Strings.chars_ptr;
      argcPtr : access C.int;
      argvPtr : access CArgv.Chars_Ptr_Ptr)
      return    C.int;
   pragma Import (C, Tcl_SplitList, "Tcl_SplitList");

   --  243

   procedure Tcl_SplitPath
     (path    : in C.Strings.chars_ptr;
      argcPtr : access C.int;
      argvPtr : access CArgv.Chars_Ptr_Ptr);
   pragma Import (C, Tcl_SplitPath, "Tcl_SplitPath");

   --  244

   procedure Tcl_StaticPackage
     (interp       : in Tcl_Interp;
      pkgName      : in C.Strings.chars_ptr;
      initProc     : in Tcl_PackageInitProc;
      safeInitProc : in Tcl_PackageInitProc);
   pragma Import (C, Tcl_StaticPackage, "Tcl_StaticPackage");

   --  245

   function Tcl_StringMatch
     (str     : in C.Strings.chars_ptr;
      pattern : in C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_StringMatch, "Tcl_StringMatch");

   --  246

   function Tcl_Tell (chan : in Tcl_Channel) return C.int;
   pragma Import (C, Tcl_Tell, "Tcl_Tell");

   --  247

   procedure Tcl_TraceVar
     (interp  : in Tcl_Interp;
      varName : in C.Strings.chars_ptr;
      flags   : in C.int;
      proc    : in Tcl_VarTraceProc;
      data    : in ClientData);
   pragma Import (C, Tcl_TraceVar, "Tcl_TraceVar");

   --  248

   procedure Tcl_TraceVar2
     (interp : in Tcl_Interp;
      part1  : in C.Strings.chars_ptr;
      part2  : in C.Strings.chars_ptr;
      flags  : in C.int;
      proc   : in Tcl_VarTraceProc;
      data   : in ClientData);
   pragma Import (C, Tcl_TraceVar2, "Tcl_TraceVar2");

   --  249

   --  250

   function Tcl_Ungets
     (chan   : in Tcl_Channel;
      str    : in C.Strings.chars_ptr;
      len    : in C.int;
      atHead : in C.int)
      return   C.int;
   pragma Import (C, Tcl_Ungets, "Tcl_Ungets");

   --  251

   procedure Tcl_UnlinkVar
     (interp  : in Tcl_Interp;
      varName : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_UnlinkVar, "Tcl_UnlinkVar");

   --  252

   function Tcl_UnregisterChannel
     (interp : in Tcl_Interp;
      chan   : in Tcl_Channel)
      return   C.int;
   pragma Import (C, Tcl_UnregisterChannel, "Tcl_UnregisterChannel");

   --  253

   function Tcl_UnsetVar
     (interp  : in Tcl_Interp;
      varName : in C.Strings.chars_ptr;
      flags   : in C.int)
      return    C.int;
   pragma Import (C, Tcl_UnsetVar, "Tcl_UnsetVar");

   --  254

   function Tcl_UnsetVar2
     (interp : in Tcl_Interp;
      part1  : in C.Strings.chars_ptr;
      part2  : in C.Strings.chars_ptr;
      flags  : in C.int)
      return   C.int;
   pragma Import (C, Tcl_UnsetVar2, "Tcl_UnsetVar2");

   --  255

   procedure Tcl_UntraceVar
     (interp  : in Tcl_Interp;
      varName : in C.Strings.chars_ptr;
      flags   : in C.int;
      proc    : in Tcl_VarTraceProc;
      data    : in ClientData);
   pragma Import (C, Tcl_UntraceVar, "Tcl_UntraceVar");

   --  256

   procedure Tcl_UntraceVar2
     (interp : in Tcl_Interp;
      part1  : in C.Strings.chars_ptr;
      part2  : in C.Strings.chars_ptr;
      flags  : in C.int;
      proc   : in Tcl_VarTraceProc;
      data   : in ClientData);
   pragma Import (C, Tcl_UntraceVar2, "Tcl_UntraceVar2");

   --  257

   procedure Tcl_UpdateLinkedVar
     (interp  : in Tcl_Interp;
      varName : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_UpdateLinkedVar, "Tcl_UpdateLinkedVar");

   --  258

   function Tcl_UpVar
     (interp    : in Tcl_Interp;
      frameName : in C.Strings.chars_ptr;
      varName   : in C.Strings.chars_ptr;
      localName : in C.Strings.chars_ptr;
      flags     : in C.int)
      return      C.int;
   pragma Import (C, Tcl_UpVar, "Tcl_UpVar");

   --  259

   function Tcl_UpVar2
     (interp    : in Tcl_Interp;
      frameName : in C.Strings.chars_ptr;
      part1     : in C.Strings.chars_ptr;
      part2     : in C.Strings.chars_ptr;
      localName : in C.Strings.chars_ptr;
      flags     : in C.int)
      return      C.int;
   pragma Import (C, Tcl_UpVar2, "Tcl_UpVar2");

   --  260

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
      return    C.int;

   --  261

   function Tcl_VarTraceInfo
     (interp         : in Tcl_Interp;
      varName        : in C.Strings.chars_ptr;
      flags          : in C.int;
      procPtr        : in Tcl_VarTraceProc;
      prevclientdata : in ClientData)
      return           ClientData;
   pragma Import (C, Tcl_VarTraceInfo, "Tcl_VarTraceInfo");

   --  262

   function Tcl_VarTraceInfo2
     (interp         : in Tcl_Interp;
      part1          : in C.Strings.chars_ptr;
      part2          : in C.Strings.chars_ptr;
      flags          : in C.int;
      procPtr        : in Tcl_VarTraceProc;
      prevclientdata : in ClientData)
      return           ClientData;
   pragma Import (C, Tcl_VarTraceInfo2, "Tcl_VarTraceInfo2");

   --  263

   function Tcl_Write
     (chan : in Tcl_Channel;
      s    : in C.Strings.chars_ptr;
      slen : in C.int)
      return C.int;
   pragma Import (C, Tcl_Write, "Tcl_Write");

   --  264

   procedure Tcl_WrongNumArgs
     (interp  : in Tcl_Interp;
      objc    : in C.int;
      objv    : in Tcl_Obj_Array;
      message : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_WrongNumArgs, "Tcl_WrongNumArgs");

   --  265

   function Tcl_DumpActiveMemory
     (fileName : in C.Strings.chars_ptr)
      return     C.int;
   pragma Import (C, Tcl_DumpActiveMemory, "Tcl_DumpActiveMemory");

   --  266

   procedure Tcl_ValidateAllMemory
     (file : in C.Strings.chars_ptr;
      line : in C.int);
   pragma Import (C, Tcl_ValidateAllMemory, "Tcl_ValidateAllMemory");

   --  267

   --  268

   --  269

   function Tcl_HashStats
     (tablePtr : in Tcl_HashTable)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_HashStats, "Tcl_HashStats");

   --  270

   function Tcl_ParseVar
     (interp  : in Tcl_Interp;
      str     : in C.Strings.chars_ptr;
      termPtr : in CArgv.Chars_Ptr_Ptr)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_ParseVar, "Tcl_ParseVar");

   --  271

   function Tcl_PkgPresent
     (interp  : in Tcl_Interp;
      name    : in C.Strings.chars_ptr;
      version : in C.Strings.chars_ptr;
      exact   : in C.int)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgPresent, "Tcl_PkgPresent");

   --  272

   function Tcl_PkgPresentEx
     (interp        : in Tcl_Interp;
      name          : in C.Strings.chars_ptr;
      version       : in C.Strings.chars_ptr;
      exact         : in C.int;
      clientdataptr : in ClientData)
      return          C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgPresentEx, "Tcl_PkgPresentEx");

   --  273

   function Tcl_PkgProvide
     (interp  : in Tcl_Interp;
      name    : in C.Strings.chars_ptr;
      version : in C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_PkgProvide, "Tcl_PkgProvide");

   --  274

   function Tcl_PkgRequire
     (interp  : in Tcl_Interp;
      name    : in C.Strings.chars_ptr;
      version : in C.Strings.chars_ptr;
      exact   : in C.int)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tcl_PkgRequire, "Tcl_PkgRequire");

   --  275

   --  276

   --  277

   function Tcl_WaitPid
     (pid     : in Tcl_Pid;
      statPtr : access C.int;
      options : in C.int)
      return    Tcl_Pid;
   pragma Import (C, Tcl_WaitPid, "Tcl_WaitPid");

   --  UNIX

   --  278

   --  UNIX

   --  __WIN32__

   --  279

   procedure Tcl_GetVersion
     (major      : access C.int;
      minor      : access C.int;
      patchLevel : access C.int;
      typ        : access C.int);
   pragma Import (C, Tcl_GetVersion, "Tcl_GetVersion");

   --  280

   procedure Tcl_InitMemory (interp : in Tcl_Interp);
   pragma Import (C, Tcl_InitMemory, "Tcl_InitMemory");

   --  281

   function Tcl_StackChannel
     (interp       : in Tcl_Interp;
      typePtr      : in Tcl_ChannelType;
      instancedata : in ClientData;
      mask         : in C.int;
      prevChan     : in Tcl_Channel)
      return         Tcl_Channel;
   pragma Import (C, Tcl_StackChannel, "Tcl_StackChannel");

   --  282

   procedure Tcl_UnstackChannel
     (interp : in Tcl_Interp;
      chan   : in Tcl_Channel);
   pragma Import (C, Tcl_UnstackChannel, "Tcl_UnstackChannel");

   --  283

   function Tcl_GetStackedChannel
     (chan : in Tcl_Channel)
      return Tcl_Channel;
   pragma Import (C, Tcl_GetStackedChannel, "Tcl_GetStackedChannel");

   --  Slot 284 is reserved

   --  Slot 285 is reserved

   --  286

   procedure Tcl_AppendObjToObj
     (objPtr       : in Tcl_Obj;
      appendObjPtr : in Tcl_Obj);
   pragma Import (C, Tcl_AppendObjToObj, "Tcl_AppendObjToObj");

   --  287

   function Tcl_CreateEncoding
     (typePtr : in Tcl_EncodingType)
      return    Tcl_Encoding;
   pragma Import (C, Tcl_CreateEncoding, "Tcl_CreateEncoding");

   --  288

   procedure Tcl_CreateThreadExitHandler
     (proc : in Tcl_ExitProc;
      data : in ClientData);
   pragma Import
     (C,
      Tcl_CreateThreadExitHandler,
      "Tcl_CreateThreadExitHandler");

   --  289

   procedure Tcl_DeleteThreadExitHandler
     (proc : in Tcl_ExitProc;
      data : in ClientData);
   pragma Import
     (C,
      Tcl_DeleteThreadExitHandler,
      "Tcl_DeleteThreadExitHandler");

   --  290

   procedure Tcl_DiscardResult (statePtr : in Tcl_SavedResult);
   pragma Import (C, Tcl_DiscardResult, "Tcl_DiscardResult");

   --  291

   function Tcl_EvalEx
     (interp   : in Tcl_Interp;
      script   : in C.Strings.chars_ptr;
      numBytes : in C.int;
      flags    : in C.int)
      return     C.int;
   pragma Import (C, Tcl_EvalEx, "Tcl_EvalEx");

   --  292

   function Tcl_EvalObjv
     (interp : in Tcl_Interp;
      objc   : in C.int;
      objv   : in Tcl_Obj_Array;
      flags  : in C.int)
      return   C.int;
   pragma Import (C, Tcl_EvalObjv, "Tcl_EvalObjv");

   --  293

   function Tcl_EvalObjEx
     (interp : in Tcl_Interp;
      objPtr : in Tcl_Obj;
      flags  : in C.int)
      return   C.int;
   pragma Import (C, Tcl_EvalObjEx, "Tcl_EvalObjEx");

   --  294

   procedure Tcl_ExitThread (status : in C.int);
   pragma Import (C, Tcl_ExitThread, "Tcl_ExitThread");

   --  295

   function Tcl_ExternalToUtf
     (interp      : in Tcl_Interp;
      encoding    : in Tcl_Encoding;
      src         : in C.Strings.chars_ptr;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : in Tcl_EncodingState;
      dst         : in C.Strings.chars_ptr;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
      return        C.int;
   pragma Import (C, Tcl_ExternalToUtf, "Tcl_ExternalToUtf");

   --  296

   function Tcl_ExternalToUtfDString
     (encoding : in Tcl_Encoding;
      src      : in C.Strings.chars_ptr;
      srcLen   : in C.int;
      dsPtr    : in Tcl_DString)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_ExternalToUtfDString, "Tcl_ExternalToUtfDString");

   --  297

   procedure Tcl_FinalizeThread;
   pragma Import (C, Tcl_FinalizeThread, "Tcl_FinalizeThread");

   --  298

   procedure Tcl_FinalizeNotifier (data : in ClientData);
   pragma Import (C, Tcl_FinalizeNotifier, "Tcl_FinalizeNotifier");

   --  299

   procedure Tcl_FreeEncoding (encoding : in Tcl_Encoding);
   pragma Import (C, Tcl_FreeEncoding, "Tcl_FreeEncoding");

   --  300

   function Tcl_GetCurrentThread return Tcl_ThreadId;
   pragma Import (C, Tcl_GetCurrentThread, "Tcl_GetCurrentThread");

   --  301

   function Tcl_GetEncoding
     (interp : in Tcl_Interp;
      name   : in C.Strings.chars_ptr)
      return   Tcl_Encoding;
   pragma Import (C, Tcl_GetEncoding, "Tcl_GetEncoding");

   --  302

   function Tcl_GetEncodingName
     (encoding : in Tcl_Encoding)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetEncodingName, "Tcl_GetEncodingName");

   --  303

   procedure Tcl_GetEncodingNames (interp : in Tcl_Interp);
   pragma Import (C, Tcl_GetEncodingNames, "Tcl_GetEncodingNames");

   --  304

   function Tcl_GetIndexFromObjStruct
     (interp   : in Tcl_Interp;
      objPtr   : in Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      offset   : in C.int;
      msg      : in C.Strings.chars_ptr;
      flags    : in C.int;
      indexPtr : access C.int)
      return     C.int;
   pragma Import (C, Tcl_GetIndexFromObjStruct, "Tcl_GetIndexFromObjStruct");

   --  305

   procedure Tcl_GetThreadData
     (keyPtr : in Tcl_ThreadDataKey;
      size   : in C.int);
   pragma Import (C, Tcl_GetThreadData, "Tcl_GetThreadData");

   --  306

   function Tcl_GetVar2Ex
     (interp : in Tcl_Interp;
      part1  : in C.Strings.chars_ptr;
      part2  : in C.Strings.chars_ptr;
      flags  : in C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_GetVar2Ex, "Tcl_GetVar2Ex");

   --  307

   function Tcl_InitNotifier return ClientData;
   pragma Import (C, Tcl_InitNotifier, "Tcl_InitNotifier");

   --  308

   procedure Tcl_MutexLock (mutexPtr : in Tcl_Mutex);
   pragma Import (C, Tcl_MutexLock, "Tcl_MutexLock");

   --  309

   procedure Tcl_MutexUnlock (mutexPtr : in Tcl_Mutex);
   pragma Import (C, Tcl_MutexUnlock, "Tcl_MutexUnlock");

   --  310

   procedure Tcl_ConditionNotify (condPtr : in Tcl_Condition);
   pragma Import (C, Tcl_ConditionNotify, "Tcl_ConditionNotify");

   --  311

   procedure Tcl_ConditionWait
     (condPtr  : in Tcl_Condition;
      mutexPtr : in Tcl_Mutex;
      timePtr  : in Tcl_Time);
   pragma Import (C, Tcl_ConditionWait, "Tcl_ConditionWait");

   --  312

   function Tcl_NumUtfChars
     (src  : in C.Strings.chars_ptr;
      len  : in C.int)
      return C.int;
   pragma Import (C, Tcl_NumUtfChars, "Tcl_NumUtfChars");

   --  313

   function Tcl_ReadChars
     (channel     : in Tcl_Channel;
      objPtr      : in Tcl_Obj;
      charsToRead : in C.int;
      appendFlag  : in C.int)
      return        C.int;
   pragma Import (C, Tcl_ReadChars, "Tcl_ReadChars");

   --  314

   procedure Tcl_RestoreResult
     (interp   : in Tcl_Interp;
      statePtr : in Tcl_SavedResult);
   pragma Import (C, Tcl_RestoreResult, "Tcl_RestoreResult");

   --  315

   procedure Tcl_SaveResult
     (interp   : in Tcl_Interp;
      statePtr : in Tcl_SavedResult);
   pragma Import (C, Tcl_SaveResult, "Tcl_SaveResult");

   --  316

   function Tcl_SetSystemEncoding
     (interp : in Tcl_Interp;
      name   : in C.Strings.chars_ptr)
      return   C.int;
   pragma Import (C, Tcl_SetSystemEncoding, "Tcl_SetSystemEncoding");

   --  317

   function Tcl_SetVar2Ex
     (interp      : in Tcl_Interp;
      part1       : in C.Strings.chars_ptr;
      part2       : in C.Strings.chars_ptr;
      newValuePtr : in Tcl_Obj;
      flags       : in C.int)
      return        Tcl_Obj;
   pragma Import (C, Tcl_SetVar2Ex, "Tcl_SetVar2Ex");

   --  318

   procedure Tcl_ThreadAlert (threadId : in Tcl_ThreadId);
   pragma Import (C, Tcl_ThreadAlert, "Tcl_ThreadAlert");

   --  319

   procedure Tcl_ThreadQueueEvent
     (threadId : in Tcl_ThreadId;
      evPtr    : in Tcl_Event;
      position : in Tcl_QueuePosition);
   pragma Import (C, Tcl_ThreadQueueEvent, "Tcl_ThreadQueueEvent");

   --  320

   function Tcl_UniCharAtIndex
     (src   : in C.Strings.chars_ptr;
      index : in C.int)
      return  Tcl_UniChar;
   pragma Import (C, Tcl_UniCharAtIndex, "Tcl_UniCharAtIndex");

   --  321

   function Tcl_UniCharToLower (ch : in C.int) return Tcl_UniChar;
   pragma Import (C, Tcl_UniCharToLower, "Tcl_UniCharToLower");

   --  322

   function Tcl_UniCharToTitle (ch : in C.int) return Tcl_UniChar;
   pragma Import (C, Tcl_UniCharToTitle, "Tcl_UniCharToTitle");

   --  323

   function Tcl_UniCharToUpper (ch : in C.int) return Tcl_UniChar;
   pragma Import (C, Tcl_UniCharToUpper, "Tcl_UniCharToUpper");

   --  324

   function Tcl_UniCharToUtf
     (ch   : in C.int;
      buf  : in C.Strings.chars_ptr)
      return C.int;
   pragma Import (C, Tcl_UniCharToUtf, "Tcl_UniCharToUtf");

   --  325

   function Tcl_UtfAtIndex
     (src   : in C.Strings.chars_ptr;
      index : in C.int)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfAtIndex, "Tcl_UtfAtIndex");

   --  326

   function Tcl_UtfCharComplete
     (src  : in C.Strings.chars_ptr;
      len  : in C.int)
      return C.int;
   pragma Import (C, Tcl_UtfCharComplete, "Tcl_UtfCharComplete");

   --  327

   function Tcl_UtfBackslash
     (src     : in C.Strings.chars_ptr;
      readPtr : access C.int;
      dst     : in C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_UtfBackslash, "Tcl_UtfBackslash");

   --  328

   function Tcl_UtfFindFirst
     (src  : in C.Strings.chars_ptr;
      ch   : in C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfFindFirst, "Tcl_UtfFindFirst");

   --  329

   function Tcl_UtfFindLast
     (src  : in C.Strings.chars_ptr;
      ch   : in C.int)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfFindLast, "Tcl_UtfFindLast");

   --  330

   function Tcl_UtfNext
     (src  : in C.Strings.chars_ptr)
      return C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfNext, "Tcl_UtfNext");

   --  331

   function Tcl_UtfPrev
     (src   : in C.Strings.chars_ptr;
      start : in C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfPrev, "Tcl_UtfPrev");

   --  332

   function Tcl_UtfToExternal
     (interp      : in Tcl_Interp;
      encoding    : in Tcl_Encoding;
      src         : in C.Strings.chars_ptr;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : in Tcl_EncodingState;
      dst         : in C.Strings.chars_ptr;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;
      dstWrotePtr : access C.int;
      dstCharsPtr : access C.int)
      return        C.int;
   pragma Import (C, Tcl_UtfToExternal, "Tcl_UtfToExternal");

   --  333

   function Tcl_UtfToExternalDString
     (encoding : in Tcl_Encoding;
      src      : in C.Strings.chars_ptr;
      srcLen   : in C.int;
      dsPtr    : in Tcl_DString)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_UtfToExternalDString, "Tcl_UtfToExternalDString");

   --  334

   function Tcl_UtfToLower (src : in C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_UtfToLower, "Tcl_UtfToLower");

   --  335

   function Tcl_UtfToTitle (src : in C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_UtfToTitle, "Tcl_UtfToTitle");

   --  336

   function Tcl_UtfToUniChar
     (src   : in C.Strings.chars_ptr;
      chPtr : in Tcl_UniChar)
      return  C.int;
   pragma Import (C, Tcl_UtfToUniChar, "Tcl_UtfToUniChar");

   --  337

   function Tcl_UtfToUpper (src : in C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_UtfToUpper, "Tcl_UtfToUpper");

   --  338

   function Tcl_WriteChars
     (chan   : in Tcl_Channel;
      src    : in C.Strings.chars_ptr;
      srcLen : in C.int)
      return   C.int;
   pragma Import (C, Tcl_WriteChars, "Tcl_WriteChars");

   --  339

   function Tcl_WriteObj
     (chan   : in Tcl_Channel;
      objPtr : in Tcl_Obj)
      return   C.int;
   pragma Import (C, Tcl_WriteObj, "Tcl_WriteObj");

   --  340

   function Tcl_GetString (objPtr : in Tcl_Obj) return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetString, "Tcl_GetString");

   --  341

   function Tcl_GetDefaultEncodingDir return C.Strings.chars_ptr;
   pragma Import (C, Tcl_GetDefaultEncodingDir, "Tcl_GetDefaultEncodingDir");

   --  342

   procedure Tcl_SetDefaultEncodingDir (path : in C.Strings.chars_ptr);
   pragma Import (C, Tcl_SetDefaultEncodingDir, "Tcl_SetDefaultEncodingDir");

   --  343

   procedure Tcl_AlertNotifier (data : in ClientData);
   pragma Import (C, Tcl_AlertNotifier, "Tcl_AlertNotifier");

   --  344

   procedure Tcl_ServiceModeHook (mode : in C.int);
   pragma Import (C, Tcl_ServiceModeHook, "Tcl_ServiceModeHook");

   --  345

   function Tcl_UniCharIsAlnum (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsAlnum, "Tcl_UniCharIsAlnum");

   --  346

   function Tcl_UniCharIsAlpha (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsAlpha, "Tcl_UniCharIsAlpha");

   --  347

   function Tcl_UniCharIsDigit (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsDigit, "Tcl_UniCharIsDigit");

   --  348

   function Tcl_UniCharIsLower (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsLower, "Tcl_UniCharIsLower");

   --  349

   function Tcl_UniCharIsSpace (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsSpace, "Tcl_UniCharIsSpace");

   --  350

   function Tcl_UniCharIsUpper (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsUpper, "Tcl_UniCharIsUpper");

   --  351

   function Tcl_UniCharIsWordChar (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsWordChar, "Tcl_UniCharIsWordChar");

   --  352

   function Tcl_UniCharLen (str : in Tcl_UniChar) return C.int;
   pragma Import (C, Tcl_UniCharLen, "Tcl_UniCharLen");

   --  353

   function Tcl_UniCharNcmp
     (cs   : in Tcl_UniChar;
      ct   : in Tcl_UniChar;
      n    : in C.unsigned_long)
      return C.int;
   pragma Import (C, Tcl_UniCharNcmp, "Tcl_UniCharNcmp");

   --  354

   function Tcl_UniCharToUtfDString
     (strng    : in Tcl_UniChar;
      numChars : in C.int;
      dsPtr    : in Tcl_DString)
      return     C.Strings.chars_ptr;
   pragma Import (C, Tcl_UniCharToUtfDString, "Tcl_UniCharToUtfDString");

   --  355

   function Tcl_UtfToUniCharDString
     (strng  : in C.Strings.chars_ptr;
      length : in C.int;
      dsPtr  : in Tcl_DString)
      return   Tcl_UniChar;
   pragma Import (C, Tcl_UtfToUniCharDString, "Tcl_UtfToUniCharDString");

   --  356

   function Tcl_GetRegExpFromObj
     (interp : in Tcl_Interp;
      patObj : in Tcl_Obj;
      flags  : in C.int)
      return   Tcl_RegExp;
   pragma Import (C, Tcl_GetRegExpFromObj, "Tcl_GetRegExpFromObj");

   --  357

   function Tcl_EvalTokens
     (interp   : in Tcl_Interp;
      tokenPtr : in Tcl_Token;
      count    : in C.int)
      return     Tcl_Obj;
   pragma Import (C, Tcl_EvalTokens, "Tcl_EvalTokens");

   --  358

   procedure Tcl_FreeParse (parsePtr : in Tcl_Parse);
   pragma Import (C, Tcl_FreeParse, "Tcl_FreeParse");

   --  359

   procedure Tcl_LogCommandInfo
     (interp  : in Tcl_Interp;
      script  : in C.Strings.chars_ptr;
      command : in C.Strings.chars_ptr;
      length  : in C.int);
   pragma Import (C, Tcl_LogCommandInfo, "Tcl_LogCommandInfo");

   --  360

   function Tcl_ParseBraces
     (interp   : in Tcl_Interp;
      strng    : in C.Strings.chars_ptr;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
      return     C.int;
   pragma Import (C, Tcl_ParseBraces, "Tcl_ParseBraces");

   --  361

   function Tcl_ParseCommand
     (interp   : in Tcl_Interp;
      strng    : in C.Strings.chars_ptr;
      numBytes : in C.int;
      nested   : in C.int;
      parsePtr : in Tcl_Parse)
      return     C.int;
   pragma Import (C, Tcl_ParseCommand, "Tcl_ParseCommand");

   --  362

   function Tcl_ParseExpr
     (interp   : in Tcl_Interp;
      strng    : in C.Strings.chars_ptr;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse)
      return     C.int;
   pragma Import (C, Tcl_ParseExpr, "Tcl_ParseExpr");

   --  363

   function Tcl_ParseQuotedString
     (interp   : in Tcl_Interp;
      strng    : in C.Strings.chars_ptr;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
      return     C.int;
   pragma Import (C, Tcl_ParseQuotedString, "Tcl_ParseQuotedString");

   --  364

   function Tcl_ParseVarName
     (interp   : in Tcl_Interp;
      strng    : in C.Strings.chars_ptr;
      numBytes : in C.int;
      parsePtr : in Tcl_Parse;
      append   : in C.int)
      return     C.int;
   pragma Import (C, Tcl_ParseVarName, "Tcl_ParseVarName");

   --  365

   --  366

   function Tcl_Chdir (dirName : in C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_Chdir, "Tcl_Chdir");

   --  367

   function Tcl_Access
     (path : in C.Strings.chars_ptr;
      mode : in C.int)
      return C.int;
   pragma Import (C, Tcl_Access, "Tcl_Access");

   --  368

   function Tcl_Stat (path : in C.Strings.chars_ptr) return C.int;
   pragma Import (C, Tcl_Stat, "Tcl_Stat");

   --  369

   function Tcl_UtfNcmp
     (s1   : in C.Strings.chars_ptr;
      s2   : in C.Strings.chars_ptr;
      n    : in C.unsigned_long)
      return C.int;
   pragma Import (C, Tcl_UtfNcmp, "Tcl_UtfNcmp");

   --  370

   function Tcl_UtfNcasecmp
     (s1   : in C.Strings.chars_ptr;
      s2   : in C.Strings.chars_ptr;
      n    : in C.unsigned_long)
      return C.int;
   pragma Import (C, Tcl_UtfNcasecmp, "Tcl_UtfNcasecmp");

   --  371

   function Tcl_StringCaseMatch
     (str     : in C.Strings.chars_ptr;
      pattern : in C.Strings.chars_ptr;
      nocase  : in C.int)
      return    C.int;
   pragma Import (C, Tcl_StringCaseMatch, "Tcl_StringCaseMatch");

   --  372

   function Tcl_UniCharIsControl (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsControl, "Tcl_UniCharIsControl");

   --  373

   function Tcl_UniCharIsGraph (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsGraph, "Tcl_UniCharIsGraph");

   --  374

   function Tcl_UniCharIsPrint (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsPrint, "Tcl_UniCharIsPrint");

   --  375

   function Tcl_UniCharIsPunct (ch : in C.int) return C.int;
   pragma Import (C, Tcl_UniCharIsPunct, "Tcl_UniCharIsPunct");

   --  376

   function Tcl_RegExpExecObj
     (interp   : in Tcl_Interp;
      regexp   : in Tcl_RegExp;
      objPtr   : in Tcl_Obj;
      offset   : in C.int;
      nmatches : in C.int;
      flags    : in C.int)
      return     C.int;
   pragma Import (C, Tcl_RegExpExecObj, "Tcl_RegExpExecObj");

   --  377

   procedure Tcl_RegExpGetInfo
     (regexp  : in Tcl_RegExp;
      infoPtr : in Tcl_RegExpInfo);
   pragma Import (C, Tcl_RegExpGetInfo, "Tcl_RegExpGetInfo");

   --  378

   function Tcl_NewUnicodeObj
     (unicode  : in Tcl_UniChar;
      numChars : in C.int)
      return     Tcl_Obj;
   pragma Import (C, Tcl_NewUnicodeObj, "Tcl_NewUnicodeObj");

   --  379

   procedure Tcl_SetUnicodeObj
     (objPtr   : in Tcl_Obj;
      unicode  : in Tcl_UniChar;
      numChars : in C.int);
   pragma Import (C, Tcl_SetUnicodeObj, "Tcl_SetUnicodeObj");

   --  380

   function Tcl_GetCharLength (objPtr : in Tcl_Obj) return C.int;
   pragma Import (C, Tcl_GetCharLength, "Tcl_GetCharLength");

   --  381

   function Tcl_GetUniChar
     (objPtr : in Tcl_Obj;
      index  : in C.int)
      return   Tcl_UniChar;
   pragma Import (C, Tcl_GetUniChar, "Tcl_GetUniChar");

   --  382

   function Tcl_GetUnicode (objPtr : in Tcl_Obj) return Tcl_UniChar;
   pragma Import (C, Tcl_GetUnicode, "Tcl_GetUnicode");

   --  383

   function Tcl_GetRange
     (objPtr : in Tcl_Obj;
      first  : in C.int;
      last   : in C.int)
      return   Tcl_Obj;
   pragma Import (C, Tcl_GetRange, "Tcl_GetRange");

   --  384

   procedure Tcl_AppendUnicodeToObj
     (objPtr  : in Tcl_Obj;
      unicode : in Tcl_UniChar;
      length  : in C.int);
   pragma Import (C, Tcl_AppendUnicodeToObj, "Tcl_AppendUnicodeToObj");

   --  385

   function Tcl_RegExpMatchObj
     (interp     : in Tcl_Interp;
      stringObj  : in Tcl_Obj;
      patternObj : in Tcl_Obj)
      return       C.int;
   pragma Import (C, Tcl_RegExpMatchObj, "Tcl_RegExpMatchObj");

   --  386

   procedure Tcl_SetNotifier (notifierProcPtr : in Tcl_NotifierProcs);
   pragma Import (C, Tcl_SetNotifier, "Tcl_SetNotifier");

   --  387

   function Tcl_GetAllocMutex return Tcl_Mutex;
   pragma Import (C, Tcl_GetAllocMutex, "Tcl_GetAllocMutex");

   --  388

   function Tcl_GetChannelNames (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tcl_GetChannelNames, "Tcl_GetChannelNames");

   --  389

   function Tcl_GetChannelNamesEx
     (interp  : in Tcl_Interp;
      pattern : in C.Strings.chars_ptr)
      return    C.int;
   pragma Import (C, Tcl_GetChannelNamesEx, "Tcl_GetChannelNamesEx");

   --  390

   function Tcl_ProcObjCmd
     (data   : in ClientData;
      interp : in Tcl_Interp;
      objc   : in C.int;
      objv   : in Tcl_Obj_Array)
      return   C.int;
   pragma Import (C, Tcl_ProcObjCmd, "Tcl_ProcObjCmd");

   --  defined {USE_TCL_STUBS} && !defined {USE_TCL_STUB_PROCS}

   --  !END!: Do not edit above this line.

   --
   --  Public functions that are not accessible via the stubs table.
   --

   procedure Tcl_Main
     (argc        : in C.int;
      argv        : in CArgv.Chars_Ptr_Ptr;
      appInitProc : in Tcl_AppInitProc);
   pragma Import (C, Tcl_Main, "Tcl_Main");

   --
   --  Convenience declaration of Tcl_AppInit for backwards compatibility.
   --  This function is not *implemented * by the tcl library, so the storage
   --  class is neither DLLEXPORT nor DLLIMPORT
   --

   function Tcl_AppInit (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tcl_AppInit, "Tcl_AppInit");

   --
   --  end block for C++
   --

private
   type Tcl_AsyncHandler_rec is null record;
   Null_Tcl_AsyncHandler : constant Tcl_AsyncHandler := null;

   type Tcl_CallFrame_rec is record
      nsPtr   : Tcl_Namespace;
      dummy1  : C.int;
      dummy2  : C.int;
      dummy3  : C.Strings.chars_ptr;
      dummy4  : C.Strings.chars_ptr;
      dummy5  : C.Strings.chars_ptr;
      dummy6  : C.int;
      dummy7  : C.Strings.chars_ptr;
      dummy8  : C.Strings.chars_ptr;
      dummy9  : C.int;
      dummy10 : C.Strings.chars_ptr;
   end record;

   Null_Tcl_CallFrame : constant Tcl_CallFrame := null;

   type Tcl_Channel_rec is null record;
   Null_Tcl_Channel : constant Tcl_Channel := null;

   type Tcl_ChannelType_rec is record
      typeName : C.Strings.chars_ptr;
      --  The name of the channel type in Tcl
      --  commands. This storage is owned by
      --  channel type.
      blockModeProc : Tcl_DriverBlockModeProc;
      --  Set blocking mode for the
      --  raw channel. May be NULL.
      closeProc : Tcl_DriverCloseProc;
      --  Procedure to call to close the
      --  channel, or TCL_CLOSE2PROC if the
      --  close2Proc should be used
      --  instead.
      inputProc : Tcl_DriverInputProc;
      --  Procedure to call for input
      --  on channel.
      outputProc : Tcl_DriverOutputProc;
      --  Procedure to call for output
      --  on channel.
      seekProc : Tcl_DriverSeekProc;
      --  Procedure to call to seek
      --  on the channel. May be NULL.
      setOptionProc : Tcl_DriverSetOptionProc;
      --  Set an option on a channel.
      getOptionProc : Tcl_DriverGetOptionProc;
      --  Get an option from a channel.
      watchProc : Tcl_DriverWatchProc;
      --  Set up the notifier to watch
      --  for events on this channel.
      getHandleProc : Tcl_DriverGetHandleProc;
      --  Get an OS handle from the channel
      --  or NULL if not supported.
      close2Proc : Tcl_DriverClose2Proc;
      --  Procedure to call to close the
      --  channel if the device supports
      --  closing the read & write sides
      --  independently.
   end record;

   Null_Tcl_ChannelType : constant Tcl_ChannelType := null;

   type Tcl_CmdInfo_rec is record
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

   Null_Tcl_CmdInfo : constant Tcl_CmdInfo := null;

   type Tcl_Command_rec is null record;
   Null_Tcl_Command : constant Tcl_Command := null;

   type Tcl_Condition_rec is null record;
   Null_Tcl_Condition : constant Tcl_Condition := null;

   type Tcl_DString_rec is record
      strng : C.Strings.chars_ptr;
      --  Points to beginning of string:  either
      --  staticSpace below or a malloced array.
      length : C.int;
      --  Number of non-NULL characters in the
      --  string.
      spaceAvl : C.int;
      --  Total number of bytes available for the
      --  string and its terminating NULL char.
      staticSpace : C.char_array (0 .. 199);
      --  Space to use in common case where string
      --  is small.
   end record;

   Null_Tcl_DString : constant Tcl_DString := null;

   type Tcl_Encoding_rec is null record;
   Null_Tcl_Encoding : constant Tcl_Encoding := null;

   type Tcl_EncodingState_rec is null record;
   Null_Tcl_EncodingState : constant Tcl_EncodingState := null;

   type Tcl_EncodingType_rec is record
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

   Null_Tcl_EncodingType : constant Tcl_EncodingType := null;

   type Tcl_Event_rec is null record;
   Null_Tcl_Event : constant Tcl_Event := null;

   type Tcl_HashEntry_rec is record
      nextPtr : Tcl_HashEntry;
      --  Pointer to next entry in this
      --  hash bucket, or NULL for end of
      --  chain.
      --  Pointer to table containing entry.
      bucketPtr : Tcl_HashEntry;
      --  Pointer to bucket that points to
      --  first entry in this entry's chain:
      --  used for deleting the entry.
      data : ClientData;
      --  Application stores something here
      --  with Tcl_SetHashValue.
      key : C.char_array (0 .. 3);
      --  MUST BE LAST FIELD IN RECORD!!
   end record;

   Null_Tcl_HashEntry : constant Tcl_HashEntry := null;

   type Tcl_HashSearch_rec is record
      tablePtr : Tcl_HashTable;
      --  Table being searched.
      nextIndex : C.int;
      --  Index of next bucket to be
      --  enumerated after present one.
      nextEntryPtr : Tcl_HashEntry;
      --  Next entry to be enumerated in the
      --  the current bucket.
   end record;

   Null_Tcl_HashSearch : constant Tcl_HashSearch := null;

   type Tcl_HashTable_rec is record
      buckets : Tcl_HashEntry;
      --  Pointer to bucket array.  Each
      --  element points to first entry in
      --  bucket's hash chain, or NULL.
      staticBuckets : Tcl_HashEntry_Array (0 .. 3);
      --  Bucket array used for small tables
      --  {to avoid mallocs and frees}.
      numBuckets : C.int;
      --  Total number of buckets allocated
      --  at **bucketPtr.
      numEntries : C.int;
      --  Total number of entries present
      --  in table.
      rebuildSize : C.int;
      --  Enlarge table when numEntries gets
      --  to be this large.
      downShift : C.int;
      --  Shift count used in hashing
      --  function.  Designed to use high-
      --  order bits of randomized keys.
      mask : C.int;
      --  Mask value used in hashing
      --  function.
      keyType : C.int;
      --  Type of keys used in this table.
      --  It's either TCL_STRING_KEYS,
      --  TCL_ONE_WORD_KEYS, or an integer
      --  giving the number of ints that
      --  is the size of the key.
      --
      findProc   : Tcl_HashEntry;
      createProc : Tcl_HashEntry;
   end record;

   Null_Tcl_HashTable : constant Tcl_HashTable := null;

   type Tcl_Interp_rec is record
      result : C.Strings.chars_ptr;
      --  If the last command returned a string
      --  result, this points to it.
      freeProc : Tcl_FreeProc;
      --  Zero means the string result is
      --  statically allocated. TCL_DYNAMIC means
      --  the address of procedure to invoke to
      --  free the result. Tcl_Eval must free it
      --  before executing next command.
      errorLine : C.int;
      --  When TCL_ERROR is returned, this gives
      --  the line number within the command where
      --  the error occurred {1 if first line}.
   end record;

   Null_Tcl_Interp : constant Tcl_Interp := null;

   type Tcl_Mutex_rec is null record;
   Null_Tcl_Mutex : constant Tcl_Mutex := null;

   type Tcl_Namespace_rec is record
      name : C.Strings.chars_ptr;
      --  The namespace's name within its parent
      --  namespace. This contains no ::'s. The
      --  name of the global namespace is ""
      --  although "::" is an synonym.
      fullName : C.Strings.chars_ptr;
      --  The namespace's fully qualified name.
      --  This starts with ::.
      data : ClientData;
      --  Arbitrary value associated with this
      --  namespace.
      deleteProc : Tcl_NamespaceDeleteProc;
      --  Procedure invoked when deleting the
      --  namespace to, e.g., free clientData.
      parentPtr : Tcl_Namespace;
      --  Points to the namespace that contains
      --  this one. NULL if this is the global
      --  namespace.
   end record;

   Null_Tcl_Namespace : constant Tcl_Namespace := null;

   type Tcl_NotifierProcs_rec is record
      setTimerProc          : Tcl_SetTimerProc;
      waitForEventProc      : Tcl_WaitForEventProc;
      createFileHandlerProc : Tcl_CreateFileHandlerProc;
      deleteFileHandlerProc : Tcl_DeleteFileHandlerProc;
   end record;

   Null_Tcl_NotifierProcs : constant Tcl_NotifierProcs := null;

   type Tcl_Obj_rec is null record;
   Null_Tcl_Obj : constant Tcl_Obj := null;

   Null_Tcl_Obj_Ptr : constant Tcl_Obj_Ptr := null;

   type Tcl_ObjType_rec is record
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

   Null_Tcl_ObjType : constant Tcl_ObjType := null;

   type Tcl_Parse_rec is record
      commentStart : C.Strings.chars_ptr;
      --  Pointer to # that begins the first of
      --  one or more comments preceding the
      --  command.
      commentSize : C.int;
      --  Number of bytes in comments {up through
      --  newline character that terminates the
      --  last comment}.  If there were no
      --  comments, this field is 0.
      commandStart : C.Strings.chars_ptr;
      --  First character in first word of command.
      commandSize : C.int;
      --  Number of bytes in command, including
      --  first character of first word, up
      --  through the terminating newline,
      --  close bracket, or semicolon.
      numWords : C.int;
      --  Total number of words in command.  May
      --  be 0.
      tokenPtr : Tcl_Token;
      --  Pointer to first token representing
      --  the words of the command.  Initially
      --  points to staticTokens, but may change
      --  to point to malloc-ed space if command
      --  exceeds space in staticTokens.
      numTokens : C.int;
      --  Total number of tokens in command.
      tokensAvailable : C.int;
      --  Total number of tokens available at
      --  *tokenPtr.
      errorType : C.int;
      --  One of the parsing error types defined
      --  above.
      --
      --  The fields below are intended only for the private use of the
      --  parser.     They should not be used by procedures that invoke
      --  Tcl_ParseCommand.
      --
      strng : C.Strings.chars_ptr;
      --  The original command string passed to
      --  Tcl_ParseCommand.
      e_n_d : C.Strings.chars_ptr;
      --  Points to the character just after the
      --  last one in the command string.
      interp : Tcl_Interp;
      --  Interpreter to use for error reporting,
      --  or NULL.
      term : C.Strings.chars_ptr;
      --  Points to character in string that
      --  terminated most recent token.  Filled in
      --  by ParseTokens.  If an error occurs,
      --  points to beginning of region where the
      --  error occurred {e.g. the open brace if
      --  the close brace is missing}.
      incomplete : C.int;
      --  This field is set to 1 by Tcl_ParseCommand
      --  if the command appears to be incomplete.
      --  This information is used by
      --  Tcl_CommandComplete.
      staticTokens : Tcl_Token_Array (0 .. 19);
      --  Initial space for tokens for command.
      --  This space should be large enough to
      --  accommodate most commands; dynamic
      --  space is allocated for very large
      --  commands that don't fit here.
   end record;

   Null_Tcl_Parse : constant Tcl_Parse := null;

   type Tcl_Pid_rec is null record;
   Null_Tcl_Pid : constant Tcl_Pid := null;

   type Tcl_RegExp_rec is null record;
   Null_Tcl_RegExp : constant Tcl_RegExp := null;

   type Tcl_SavedResult_rec is record
      result       : C.Strings.chars_ptr;
      freeProc     : Tcl_FreeProc;
      objResultPtr : Tcl_Obj;
      appendResult : C.Strings.chars_ptr;
      appendAvl    : C.int;
      appendUsed   : C.int;
      resultSpace  : C.char_array (0 .. 200 + 1 - 1);
   end record;

   Null_Tcl_SavedResult : constant Tcl_SavedResult := null;

   type Tcl_ThreadDataKey_rec is null record;
   Null_Tcl_ThreadDataKey : constant Tcl_ThreadDataKey := null;

   type Tcl_ThreadId_rec is null record;
   Null_Tcl_ThreadId : constant Tcl_ThreadId := null;

   type Tcl_Time_rec is record
      sec : C.long;
      --  Seconds.
      usec : C.long;
      --  Microseconds.
   end record;

   Null_Tcl_Time : constant Tcl_Time := null;

   type Tcl_TimerToken_rec is null record;
   Null_Tcl_TimerToken : constant Tcl_TimerToken := null;

   type Tcl_Token_rec is record
      typ : C.int;
      --  Type of token, such as TCL_TOKEN_WORD;
      --  see below for valid types.
      start : C.Strings.chars_ptr;
      --  First character in token.
      size : C.int;
      --  Number of bytes in token.
      numComponents : C.int;
      --  If this token is composed of other
      --  tokens, this field tells how many of
      --  them there are {including components of
      --  components, etc.}.  The component tokens
      --  immediately follow this one.
   end record;

   Null_Tcl_Token : constant Tcl_Token := null;

   type Tcl_Trace_rec is null record;
   Null_Tcl_Trace : constant Tcl_Trace := null;

   type Tcl_Value_rec is record
      typ : Tcl_ValueType;
      --  Indicates intValue or doubleValue is
      --  valid, or both.
      intValue : C.long;
      --  Integer value.
      doubleValue : C.double;
      --  Double-precision floating value.
   end record;

   Null_Tcl_Value : constant Tcl_Value := null;

   type Tcl_Var_rec is null record;
   Null_Tcl_Var : constant Tcl_Var := null;

   type stat_rec is null record;
   Null_stat : constant stat := null;

end Tcl;
