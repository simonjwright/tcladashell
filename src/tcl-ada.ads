--------------------------------------------------------------------
--
--  tcl-ada.ads -- This package provides the "thin" binding to Tcl.
--
--  Copyright (c) 1995-2000 Terry J. Westley
--  Copyright (c) 2006, 2009, 2011, 2014, 2019
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
--
--  This package is made up primarily of
--
--  1) extra overloaded subprograms which use the Ada String type in
--     place of the C string type (Interfaces.C.Strings.Chars_Ptr).
--
--  2) extra overloaded procedures for tcl.h functions which return
--     TCL_OK or TCL_ERROR so that Ada programmers can call them as
--     procedures rather than functions. The exception Tcl_Error_Exception
--     is raised when the Tcl function returns TCL_ERROR.
--
--  3) generic packages that allow use of a data type for ClientData
--     rather than a reference to a void or int type as C does.
--
--------------------------------------------------------------------

with CArgv;

package Tcl.Ada is

   package C renames Interfaces.C;

   Tcl_Error_Exception : exception;

   procedure Assert (Interp : not null Tcl_Interp; Return_Code : C.int);
   --  Raises Tcl_Error_Exception if Return_Code = TCL_ERROR

   --------------------------------------------------------------------
   --
   --   Generics for all subprograms which require Client Data
   --
   --------------------------------------------------------------------

   generic
      type ClientData is private;
   package Generic_AssocData is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_InterpDeleteProc is access procedure (Data   : ClientData;
                                                     Interp : Tcl_Interp);
      pragma Convention (C, Tcl_InterpDeleteProc);

      function Tcl_GetAssocData
        (interp  : Tcl_Interp;
         name    : C.Strings.chars_ptr;
         procPtr : access Tcl_InterpDeleteProc)   -- can be null
        return    ClientData;
      pragma Import (C, Tcl_GetAssocData, "Tcl_GetAssocData");

      function Tcl_GetAssocData
        (interp  : Tcl_Interp;
         name    : String;
         procPtr : access Tcl_InterpDeleteProc)   -- can be null
        return    ClientData;

      procedure Tcl_SetAssocData
        (interp : Tcl_Interp;
         name   : C.Strings.chars_ptr;
         proc   : Tcl_InterpDeleteProc;
         data   : ClientData);
      pragma Import (C, Tcl_SetAssocData, "Tcl_SetAssocData");

      procedure Tcl_SetAssocData
        (interp : Tcl_Interp;
         name   : String;
         proc   : Tcl_InterpDeleteProc;
         data   : ClientData);

   end Generic_AssocData;

   generic
      type ClientData is private;
   package Generic_AsyncEvents is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_AsyncProc is access function
        (data   : ClientData;
         interp : Tcl_Interp;
         code   : C.int)
        return      C.int;
      pragma Convention (C, Tcl_AsyncProc);

      function Tcl_AsyncCreate
        (proc : not null Tcl_AsyncProc;
         data : ClientData)
        return Tcl_AsyncHandler;
      pragma Import (C, Tcl_AsyncCreate, "Tcl_AsyncCreate");

   end Generic_AsyncEvents;

   generic
      type ClientData is private;
   package Generic_CallWhenDeleted is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_InterpDeleteProc is access procedure
        (data   : ClientData;
         interp : Tcl_Interp);
      pragma Convention (C, Tcl_InterpDeleteProc);

      procedure Tcl_CallWhenDeleted
        (interp : not null Tcl_Interp;
         proc   : not null Tcl_InterpDeleteProc;
         data   : ClientData);
      pragma Import (C, Tcl_CallWhenDeleted, "Tcl_CallWhenDeleted");

      procedure Tcl_DontCallWhenDeleted
        (interp : not null Tcl_Interp;
         proc   : not null Tcl_InterpDeleteProc;
         data   : ClientData);
      pragma Import (C, Tcl_DontCallWhenDeleted, "Tcl_DontCallWhenDeleted");

   end Generic_CallWhenDeleted;

   generic
      type ClientData is private;
   package Generic_Channel is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_MakeFileChannel
        (handle : ClientData;
         mode   : C.int)
        return   Tcl_Channel;
      pragma Import (C, Tcl_MakeFileChannel, "Tcl_MakeFileChannel");

   end Generic_Channel;

   generic
      type ClientData is private;
   package Generic_ChannelDriver is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_CreateChannel
        (typePtr      : not null Tcl_ChannelType;
         chanName     : C.Strings.chars_ptr;
         instancedata : ClientData;
         mask         : C.int)
        return         Tcl_Channel;
      pragma Import (C, Tcl_CreateChannel, "Tcl_CreateChannel");

      function Tcl_CreateChannel
        (typePtr      : not null Tcl_ChannelType;
         chanName     : String;
         instancedata : ClientData;
         mask         : C.int)
        return         Tcl_Channel;

      function Tcl_GetChannelHandle
        (chan      : not null Tcl_Channel;
         direction : C.int;
         handleptr : ClientData)
        return      C.int;
      pragma Import (C, Tcl_GetChannelHandle, "Tcl_GetChannelHandle");

      function Tcl_GetChannelInstanceData
        (chan : not null Tcl_Channel)
        return ClientData;
      pragma Import
        (C,
         Tcl_GetChannelInstanceData,
         "Tcl_GetChannelInstanceData");

   end Generic_ChannelDriver;

   generic
      type ClientData is private;
   package Generic_ChannelHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_ChannelProc is access procedure
        (data : ClientData;
         mask : C.int);
      pragma Convention (C, Tcl_ChannelProc);

      procedure Tcl_CreateChannelHandler
        (chan : not null Tcl_Channel;
         mask : C.int;
         proc : not null Tcl_ChannelProc;
         data : ClientData);
      pragma Import (C, Tcl_CreateChannelHandler, "Tcl_CreateChannelHandler");

      procedure Tcl_DeleteChannelHandler
        (chan : not null Tcl_Channel;
         proc : not null Tcl_ChannelProc;
         data : ClientData);
      pragma Import (C, Tcl_DeleteChannelHandler, "Tcl_DeleteChannelHandler");

   end Generic_ChannelHandler;

   generic
      type ClientData is private;
   package Generic_CloseHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_CloseProc is access procedure (data : ClientData);
      pragma Convention (C, Tcl_CloseProc);

      procedure Tcl_CreateCloseHandler
        (chan : not null Tcl_Channel;
         proc : not null Tcl_CloseProc;
         data : ClientData);
      pragma Import (C, Tcl_CreateCloseHandler, "Tcl_CreateCloseHandler");

      procedure Tcl_DeleteCloseHandler
        (chan : not null Tcl_Channel;
         proc : not null Tcl_CloseProc;
         data : ClientData);
      pragma Import (C, Tcl_DeleteCloseHandler, "Tcl_DeleteCloseHandler");

   end Generic_CloseHandler;

   generic
      type ClientData is private;
   package Generic_Command is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_CmdProc is access function
        (data   : ClientData;
         interp : Tcl_Interp;
         argc   : C.int;
         argv   : CArgv.Chars_Ptr_Ptr)
        return      C.int;
      pragma Convention (C, Tcl_CmdProc);

      type Tcl_CmdDeleteProc is access procedure (data : ClientData);
      pragma Convention (C, Tcl_CmdDeleteProc);

      function Tcl_CreateCommand
        (interp     : not null Tcl_Interp;
         cmdName    : C.Strings.chars_ptr;
         proc       : not null Tcl_CmdProc;
         data       : ClientData;
         deleteProc : Tcl_CmdDeleteProc)    -- can be null
        return       Tcl_Command;
      pragma Import (C, Tcl_CreateCommand, "Tcl_CreateCommand");

      function Tcl_CreateCommand
        (interp     : not null Tcl_Interp;
         cmdName    : String;
         proc       : not null Tcl_CmdProc;
         data       : ClientData;
         deleteProc : Tcl_CmdDeleteProc)    -- can be null
        return       Tcl_Command;

   end Generic_Command;

   generic
      type ClientData is private;
   package Generic_EventQueueAndNotifier is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_EventSetupProc is access procedure
        (data  : ClientData;
         flags : C.int);
      pragma Convention (C, Tcl_EventSetupProc);

      type Tcl_EventCheckProc is access procedure
        (data  : ClientData;
         flags : C.int);
      pragma Convention (C, Tcl_EventCheckProc);

      type Tcl_FileProc is access procedure
        (data : ClientData;
         mask : C.int);
      pragma Convention (C, Tcl_FileProc);

      type Tcl_EventDeleteProc is access function
        (evPtr : Tcl_Event;
         data  : ClientData)
        return     C.int;
      pragma Convention (C, Tcl_EventDeleteProc);

      procedure Tcl_AlertNotifier (data : ClientData);
      pragma Import (C, Tcl_AlertNotifier, "Tcl_AlertNotifier");

      procedure Tcl_CreateEventSource
        (setupProc : not null Tcl_EventSetupProc;
         checkProc : not null Tcl_EventCheckProc;
         data      : ClientData);
      pragma Import (C, Tcl_CreateEventSource, "Tcl_CreateEventSource");

      procedure Tcl_CreateFileHandler
        (fd   : C.int;
         mask : C.int;
         proc : not null Tcl_FileProc;
         data : ClientData);
      pragma Import (C, Tcl_CreateFileHandler, "Tcl_CreateFileHandler");

      procedure Tcl_DeleteEventSource
        (setupProc : not null Tcl_EventSetupProc;
         checkProc : not null Tcl_EventCheckProc;
         data      : ClientData);
      pragma Import (C, Tcl_DeleteEventSource, "Tcl_DeleteEventSource");

      procedure Tcl_DeleteEvents
        (proc : not null Tcl_EventDeleteProc;
         data : ClientData);
      pragma Import (C, Tcl_DeleteEvents, "Tcl_DeleteEvents");

      procedure Tcl_FinalizeNotifier (data : ClientData);
      pragma Import (C, Tcl_FinalizeNotifier, "Tcl_FinalizeNotifier");

      function Tcl_InitNotifier return ClientData;
      pragma Import (C, Tcl_InitNotifier, "Tcl_InitNotifier");

   end Generic_EventQueueAndNotifier;

   generic
      type ClientData is private;
   package Generic_ExitHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_ExitProc is access procedure (data : ClientData);
      pragma Convention (C, Tcl_ExitProc);

      procedure Tcl_CreateExitHandler
        (proc : not null Tcl_ExitProc;
         data : ClientData);
      pragma Import (C, Tcl_CreateExitHandler, "Tcl_CreateExitHandler");

      procedure Tcl_CreateThreadExitHandler
        (proc : not null Tcl_ExitProc;
         data : ClientData);
      pragma Import
        (C,
         Tcl_CreateThreadExitHandler,
         "Tcl_CreateThreadExitHandler");

      procedure Tcl_DeleteExitHandler
        (proc : not null Tcl_ExitProc;
         data : ClientData);
      pragma Import (C, Tcl_DeleteExitHandler, "Tcl_DeleteExitHandler");

      procedure Tcl_DeleteThreadExitHandler
        (proc : not null Tcl_ExitProc;
         data : ClientData);
      pragma Import
        (C,
         Tcl_DeleteThreadExitHandler,
         "Tcl_DeleteThreadExitHandler");

   end Generic_ExitHandler;

   generic
      type ClientData is private;
   package Generic_FileHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_FileProc is access procedure
        (data : ClientData;
         mask : C.int);
      pragma Convention (C, Tcl_FileProc);

      procedure Tcl_CreateFileHandler
        (fd   : C.int;
         mask : C.int;
         proc : not null Tcl_FileProc;
         data : ClientData);
      pragma Import (C, Tcl_CreateFileHandler, "Tcl_CreateFileHandler");

   end Generic_FileHandler;

   generic
      type ClientData is private;
   package Generic_GetOpenFile is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_GetOpenFile
        (interp     : not null Tcl_Interp;
         str        : C.Strings.chars_ptr;
         forWriting : C.int;
         checkUsage : C.int;
         fileptr    : ClientData)
        return       C.int;
      pragma Import (C, Tcl_GetOpenFile, "Tcl_GetOpenFile");

      function Tcl_GetOpenFile
        (interp     : not null  Tcl_Interp;
         str        : String;
         forWriting : C.int;
         checkUsage : C.int;
         fileptr    : ClientData)
        return       C.int;

      procedure Tcl_GetOpenFile
        (interp     : not null Tcl_Interp;
         str        : String;
         forWriting : C.int;
         checkUsage : C.int;
         fileptr    : ClientData);

   end Generic_GetOpenFile;

   generic
      type ClientData is private;
   package Generic_Hash is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_GetHashValue
        (HashEntry : not null Tcl_HashEntry)
        return      ClientData;

      procedure Tcl_SetHashValue
        (HashEntry : not null Tcl_HashEntry;
         value     : ClientData);

   end Generic_Hash;

   generic
      type ClientData is private;
   package Generic_Idle is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_IdleProc is access procedure (data : ClientData);
      pragma Convention (C, Tcl_IdleProc);

      procedure Tcl_CancelIdleCall
        (idleProc : not null Tcl_IdleProc;
         data     : ClientData);
      pragma Import (C, Tcl_CancelIdleCall, "Tcl_CancelIdleCall");

      procedure Tcl_DoWhenIdle
        (proc : not null Tcl_IdleProc;
         data : ClientData);
      pragma Import (C, Tcl_DoWhenIdle, "Tcl_DoWhenIdle");

   end Generic_Idle;

   generic
      type ClientData is private;
   package Generic_ManageStorage is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_FreeProc is access procedure
        (blockPtr : C.Strings.chars_ptr);
      pragma Convention (C, Tcl_FreeProc);

      procedure Tcl_EventuallyFree
        (data     : ClientData;
         freeProc : not null Tcl_FreeProc);
      pragma Import (C, Tcl_EventuallyFree, "Tcl_EventuallyFree");

      procedure Tcl_Preserve (data : ClientData);
      pragma Import (C, Tcl_Preserve, "Tcl_Preserve");

      procedure Tcl_Release (data : ClientData);
      pragma Import (C, Tcl_Release, "Tcl_Release");

   end Generic_ManageStorage;

   generic
      type ClientData is private;
   package Generic_MathFunc is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_MathProc is access function
        (data      : ClientData;
         interp    : Tcl_Interp;
         args      : Tcl_Value;
         resultPtr : Tcl_Value)
        return         C.int;
      pragma Convention (C, Tcl_MathProc);

      procedure Tcl_CreateMathFunc
        (interp   : not null Tcl_Interp;
         name     : C.Strings.chars_ptr;
         numArgs  : C.int;
         argTypes : Tcl_ValueType;
         proc     : not null Tcl_MathProc;
         data     : ClientData);
      pragma Import (C, Tcl_CreateMathFunc, "Tcl_CreateMathFunc");

      procedure Tcl_CreateMathFunc
        (interp   : not null Tcl_Interp;
         name     : String;
         numArgs  : C.int;
         argTypes : Tcl_ValueType;
         proc     : not null Tcl_MathProc;
         data     : ClientData);

   end Generic_MathFunc;

   generic
      type ClientData is private;
   package Generic_ObjCommand is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_ObjCmdProc is access function
        (data   : ClientData;
         interp : Tcl_Interp;
         objc   : C.int)
        return      C.int;
      pragma Convention (C, Tcl_ObjCmdProc);

      type Tcl_CmdDeleteProc is access procedure (data : ClientData);
      pragma Convention (C, Tcl_CmdDeleteProc);

      function Tcl_CreateObjCommand
        (interp     : not null Tcl_Interp;
         cmdName    : C.Strings.chars_ptr;
         proc       : not null Tcl_ObjCmdProc;
         data       : ClientData;
         deleteProc : Tcl_CmdDeleteProc)
        return       Tcl_Command;
      pragma Import (C, Tcl_CreateObjCommand, "Tcl_CreateObjCommand");

      function Tcl_CreateObjCommand
        (interp     : not null Tcl_Interp;
         cmdName    : String;
         proc       : not null Tcl_ObjCmdProc;
         data       : ClientData;
         deleteProc : Tcl_CmdDeleteProc)
        return       Tcl_Command;

   end Generic_ObjCommand;

   generic
      type ClientData is private;
   package Generic_PkgRequire is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_PkgPresentEx
        (interp        : not null Tcl_Interp;
         name          : C.Strings.chars_ptr;
         version       : C.Strings.chars_ptr;
         exact         : C.int;
         clientdataptr : access ClientData)    -- can be null
        return          C.Strings.chars_ptr;
      pragma Import (C, Tcl_PkgPresentEx, "Tcl_PkgPresentEx");

      function Tcl_PkgPresentEx
        (interp        : not null Tcl_Interp;
         name          : String;
         version       : String;
         exact         : C.int;
         clientdataptr : access ClientData)    -- can be null
        return          String;

      function Tcl_PkgProvideEx
        (interp  : not null Tcl_Interp;
         name    : C.Strings.chars_ptr;
         version : C.Strings.chars_ptr;
         data    : ClientData)
        return    C.int;
      pragma Import (C, Tcl_PkgProvideEx, "Tcl_PkgProvideEx");

      function Tcl_PkgProvideEx
        (interp  : not null Tcl_Interp;
         name    : String;
         version : String;
         data    : ClientData)
        return    C.int;

      procedure Tcl_PkgProvideEx
        (interp  : not null Tcl_Interp;
         name    : String;
         version : String;
         data    : ClientData);

      function Tcl_PkgRequireEx
        (interp        : not null Tcl_Interp;
         name          : C.Strings.chars_ptr;
         version       : C.Strings.chars_ptr;
         exact         : C.int;
         clientdataptr : access ClientData)    -- can be null
        return          C.Strings.chars_ptr;
      pragma Import (C, Tcl_PkgRequireEx, "Tcl_PkgRequireEx");

      function Tcl_PkgRequireEx
        (interp        : not null Tcl_Interp;
         name          : String;
         version       : String;
         exact         : C.int;
         clientdataptr : access ClientData)    -- can be null
        return          String;

   end Generic_PkgRequire;

   generic
      type ClientData is private;
   package Generic_StackChannel is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      --  XXX outdated?
      function Tcl_StackChannel
        (interp       : not null Tcl_Interp;
         typePtr      : not null Tcl_ChannelType;
         instancedata : ClientData;
         mask         : C.int;
         prevChan     : not null Tcl_Channel)
        return         Tcl_Channel;
      pragma Import (C, Tcl_StackChannel, "Tcl_StackChannel");

   end Generic_StackChannel;

   generic
      type ClientData is private;
   package Generic_TcpChannel is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_TcpAcceptProc is access procedure
        (callbackdata : ClientData;
         chan         : Tcl_Channel;
         address      : System.Address;
         port         : C.int);
      pragma Convention (C, Tcl_TcpAcceptProc);

      function Tcl_MakeTcpClientChannel
        (tcpsocket : ClientData)
        return      Tcl_Channel;
      pragma Import (C, Tcl_MakeTcpClientChannel, "Tcl_MakeTcpClientChannel");

      function Tcl_OpenTcpServer
        (interp       : not null Tcl_Interp;
         port         : C.int;
         host         : C.Strings.chars_ptr;
         acceptProc   : not null Tcl_TcpAcceptProc;
         callbackdata : ClientData)
        return         Tcl_Channel;
      pragma Import (C, Tcl_OpenTcpServer, "Tcl_OpenTcpServer");

      function Tcl_OpenTcpServer
        (interp       : not null Tcl_Interp;
         port         : C.int;
         host         : String;
         acceptProc   : not null Tcl_TcpAcceptProc;
         callbackdata : ClientData)
        return         Tcl_Channel;

   end Generic_TcpChannel;

   generic
      type ClientData is private;
   package Generic_TimerHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_TimerProc is access procedure (data : ClientData);
      pragma Convention (C, Tcl_TimerProc);

      function Tcl_CreateTimerHandler
        (milliseconds : C.int;
         proc         : not null Tcl_TimerProc;
         data         : ClientData)
        return         Tcl_TimerToken;
      pragma Import (C, Tcl_CreateTimerHandler, "Tcl_CreateTimerHandler");

   end Generic_TimerHandler;

   generic
      type ClientData is private;
   package Generic_Trace is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

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

      function Tcl_CreateTrace
        (interp : not null Tcl_Interp;
         level  : C.int;
         proc   : not null Tcl_CmdTraceProc;
         data   : ClientData)
        return   Tcl_Trace;
      pragma Import (C, Tcl_CreateTrace, "Tcl_CreateTrace");

   end Generic_Trace;

   generic
      type ClientData is private;
   package Generic_TraceVar is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_VarTraceProc is access function
        (data   : ClientData;
         interp : Tcl_Interp;
         part1  : C.Strings.chars_ptr;
         part2  : C.Strings.chars_ptr;
         flags  : C.int)
        return      C.Strings.chars_ptr;
      pragma Convention (C, Tcl_VarTraceProc);

      function Tcl_TraceVar
        (interp  : not null Tcl_Interp;
         varName : C.Strings.chars_ptr;
         flags   : C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : ClientData)
        return    C.int;
      pragma Import (C, Tcl_TraceVar, "Tcl_TraceVar");

      function Tcl_TraceVar
        (interp  : not null Tcl_Interp;
         varName : String;
         flags   : C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : ClientData)
        return    C.int;

      procedure Tcl_TraceVar
        (interp  : not null Tcl_Interp;
         varName : String;
         flags   : C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : ClientData);

      function Tcl_TraceVar2
        (interp : not null Tcl_Interp;
         part1  : C.Strings.chars_ptr;
         part2  : C.Strings.chars_ptr;
         flags  : C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : ClientData)
        return   C.int;
      pragma Import (C, Tcl_TraceVar2, "Tcl_TraceVar2");

      function Tcl_TraceVar2
        (interp : not null Tcl_Interp;
         part1  : String;
         part2  : String;
         flags  : C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : ClientData)
        return   C.int;

      procedure Tcl_TraceVar2
        (interp : not null Tcl_Interp;
         part1  : String;
         part2  : String;
         flags  : C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : ClientData);

      procedure Tcl_UntraceVar
        (interp  : not null Tcl_Interp;
         varName : C.Strings.chars_ptr;
         flags   : C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : ClientData);
      pragma Import (C, Tcl_UntraceVar, "Tcl_UntraceVar");

      procedure Tcl_UntraceVar
        (interp  : not null Tcl_Interp;
         varName : String;
         flags   : C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : ClientData);

      procedure Tcl_UntraceVar2
        (interp : not null Tcl_Interp;
         part1  : C.Strings.chars_ptr;
         part2  : C.Strings.chars_ptr;
         flags  : C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : ClientData);
      pragma Import (C, Tcl_UntraceVar2, "Tcl_UntraceVar2");

      procedure Tcl_UntraceVar2
        (interp : not null Tcl_Interp;
         part1  : String;
         part2  : String;
         flags  : C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : ClientData);

      function Tcl_VarTraceInfo
        (interp         : not null Tcl_Interp;
         varName        : C.Strings.chars_ptr;
         flags          : C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : ClientData)
        return           ClientData;
      pragma Import (C, Tcl_VarTraceInfo, "Tcl_VarTraceInfo");

      function Tcl_VarTraceInfo
        (interp         : not null Tcl_Interp;
         varName        : String;
         flags          : C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : ClientData)
        return           ClientData;

      function Tcl_VarTraceInfo2
        (interp         : not null Tcl_Interp;
         part1          : C.Strings.chars_ptr;
         part2          : C.Strings.chars_ptr;
         flags          : C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : ClientData)
        return           ClientData;
      pragma Import (C, Tcl_VarTraceInfo2, "Tcl_VarTraceInfo2");

      function Tcl_VarTraceInfo2
        (interp         : not null Tcl_Interp;
         part1          : String;
         part2          : String;
         flags          : C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : ClientData)
        return           ClientData;

   end Generic_TraceVar;

   function Tcl_DStringValue (dsPtr : not null Tcl_DString) return String;

   function Tcl_DbCkalloc
     (size : C.unsigned;
      file : String;
      line : C.int)
     return String;

   function Tcl_DbCkfree
     (ptr  : String;
      file : String;
      line : C.int)
     return C.int;

   function Tcl_DbCkrealloc
     (ptr  : String;
      size : C.unsigned;
      file : String;
      line : C.int)
     return String;

   procedure Tcl_AppendStringsToObj
     (objPtr  : not null Tcl_Obj;
      String1 : String := "";
      String2 : String := "";
      String3 : String := "";
      String4 : String := "";
      String5 : String := "";
      String6 : String := "";
      String7 : String := "";
      String8 : String := "";
      String9 : String := "");

   procedure Tcl_AppendToObj
     (objPtr : not null Tcl_Obj;
      bytes  : String;
      length : C.int);

   procedure Tcl_DbDecrRefCount
     (objPtr : not null Tcl_Obj;
      file   : String;
      line   : C.int);

   procedure Tcl_DbIncrRefCount
     (objPtr : not null Tcl_Obj;
      file   : String;
      line   : C.int);

   function Tcl_DbIsShared
     (objPtr : not null Tcl_Obj;
      file   : String;
      line   : C.int)
     return   C.int;

   function Tcl_DbNewBooleanObj
     (boolValue : C.int;
      file      : String;
      line      : C.int)
     return      Tcl_Obj;

   function Tcl_DbNewByteArrayObj
     (bytes  : String;
      length : C.int;
      file   : String;
      line   : C.int)
     return   Tcl_Obj;

   function Tcl_DbNewDoubleObj
     (doubleValue : C.double;
      file        : String;
      line        : C.int)
     return        Tcl_Obj;

   function Tcl_DbNewListObj
     (objc : C.int;
      objv : Tcl_Obj_Array;
      file : String;
      line : C.int)
     return Tcl_Obj;

   function Tcl_DbNewLongObj
     (longValue : C.long;
      file      : String;
      line      : C.int)
     return      Tcl_Obj;

   function Tcl_DbNewObj (file : String; line : C.int) return Tcl_Obj;

   function Tcl_DbNewStringObj
     (bytes  : String;
      length : C.int;
      file   : String;
      line   : C.int)
     return   Tcl_Obj;

   function Tcl_GetBoolean
     (interp  : not null Tcl_Interp;
      str     : String;
      boolPtr : not null access C.int)
     return    C.int;

   procedure Tcl_GetBoolean
     (interp  : not null Tcl_Interp;
      str     : String;
      boolPtr : not null access C.int);

   function Tcl_GetByteArrayFromObj
     (objPtr    : not null Tcl_Obj;
      lengthPtr : access C.int)    -- can be null
     return      String;

   function Tcl_GetDouble
     (interp    : not null Tcl_Interp;
      str       : String;
      doublePtr : not null access C.double)
     return      C.int;

   procedure Tcl_GetDouble
     (interp    : not null Tcl_Interp;
      str       : String;
      doublePtr : not null access C.double);

   function Tcl_GetIndexFromObj
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : CArgv.Chars_Ptr_Ptr;
      msg      : String;
      flags    : C.int;
      indexPtr : not null access C.int)
     return     C.int;

   procedure Tcl_GetIndexFromObj
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : CArgv.Chars_Ptr_Ptr;
      msg      : String;
      flags    : C.int;
      indexPtr : not null access C.int);

   function Tcl_GetInt
     (interp : not null Tcl_Interp;
      str    : String;
      intPtr : not null access C.int)
     return   C.int;

   procedure Tcl_GetInt
     (interp : not null Tcl_Interp;
      str    : String;
      intPtr : not null access C.int);

   function Tcl_GetObjType (typeName : String) return Tcl_ObjType;

   function Tcl_GetObjTypeName (objPtr : not null Tcl_Obj) return String;

   function Tcl_GetStringFromObj
     (objPtr    : not null Tcl_Obj;
      lengthPtr : access C.int)    -- can be null
     return      String;

   function Tcl_NewByteArrayObj
     (bytes  : String;
      length : C.int)
     return   Tcl_Obj;

   function Tcl_NewStringObj
     (bytes  : String;
      length : C.int)
     return   Tcl_Obj;

   function Tcl_SetByteArrayLength
     (objPtr : not null Tcl_Obj;
      length : C.int)
     return   String;

   procedure Tcl_SetByteArrayObj
     (objPtr : not null Tcl_Obj;
      bytes  : String;
      length : C.int);

   procedure Tcl_SetStringObj
     (objPtr : not null Tcl_Obj;
      bytes  : String;
      length : C.int);

   procedure Tcl_AddErrorInfo (interp : not null Tcl_Interp;
                               message : String);

   procedure Tcl_AddObjErrorInfo
     (interp  : not null Tcl_Interp;
      message : String;
      length  : C.int);

   procedure Tcl_AppendElement (interp : not null Tcl_Interp;
                                strng : String);

   procedure Tcl_AppendResult
     (interp  : not null Tcl_Interp;
      String1 : String := "";
      String2 : String := "";
      String3 : String := "";
      String4 : String := "";
      String5 : String := "";
      String6 : String := "";
      String7 : String := "";
      String8 : String := "";
      String9 : String := "");

   function Tcl_Backslash
     (src     : String;
      readPtr : access C.int)    -- can be null
     return    C.char;

   function Tcl_CommandComplete (cmd : String) return C.int;

   function Tcl_Concat
     (argc : C.int;
      argv : CArgv.Chars_Ptr_Ptr)
     return String;

   function Tcl_ConvertElement
     (src   : String;
      dst   : String;
      flags : C.int)
     return  C.int;

   function Tcl_ConvertCountedElement
     (src    : String;
      length : C.int;
      dst    : String;
      flags  : C.int)
     return   C.int;

   function Tcl_CreateAlias
     (slave     : not null Tcl_Interp;
      slaveCmd  : String;
      target    : not null Tcl_Interp;
      targetCmd : String;
      argc      : C.int;
      argv      : CArgv.Chars_Ptr_Ptr)
     return      C.int;

   function Tcl_CreateAliasObj
     (slave     : not null Tcl_Interp;
      slaveCmd  : String;
      target    : not null Tcl_Interp;
      targetCmd : String;
      objc      : C.int;
      objv      : Tcl_Obj_Array)
     return      C.int;

   function Tcl_CreateSlave
     (interp    : not null Tcl_Interp;
      slaveName : String;
      isSafe    : C.int)
     return      Tcl_Interp;

   function Tcl_DStringAppend
     (dsPtr  : not null Tcl_DString;
      str    : String;
      length : C.int)
     return   String;

   function Tcl_DStringAppendElement
     (dsPtr : not null Tcl_DString;
      strng : String)
     return  String;

   function Tcl_ErrnoId return C.Strings.chars_ptr;

   function Tcl_ErrnoMsg (err : C.int) return String;

   function Tcl_Eval
     (interp : not null Tcl_Interp;
      strng  : String)
     return   C.int;

   procedure Tcl_Eval (interp : not null Tcl_Interp; strng : String);

   function Tcl_EvalFile
     (interp   : not null Tcl_Interp;
      fileName : String)
     return     C.int;

   procedure Tcl_EvalFile (interp : not null Tcl_Interp;
                           fileName : String);

   function Tcl_ExposeCommand
     (interp         : not null Tcl_Interp;
      hiddenCmdToken : String;
      cmdName        : String)
     return           C.int;

   procedure Tcl_ExposeCommand
     (interp         : not null Tcl_Interp;
      hiddenCmdToken : String;
      cmdName        : String);

   function Tcl_ExprBoolean
     (interp : not null Tcl_Interp;
      str    : String)
     return   Boolean;

   function Tcl_ExprDouble
     (interp : not null Tcl_Interp;
      str    : String)
     return   C.double;

   function Tcl_ExprLong
     (interp : not null Tcl_Interp;
      str    : String)
     return   C.long;

   function Tcl_ExprString
     (interp : not null Tcl_Interp;
      strng  : String)
     return   String;

   procedure Tcl_FindExecutable (argv0 : String);

   function Tcl_GetAlias
     (interp          : not null Tcl_Interp;
      slaveCmd        : String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : CArgv.Chars_Ptr_Ptr;
      argcPtr         : not null access C.int;
      argvPtr         : not null access CArgv.Chars_Ptr_Ptr)
     return            C.int;

   procedure Tcl_GetAlias
     (interp          : not null Tcl_Interp;
      slaveCmd        : String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : CArgv.Chars_Ptr_Ptr;
      argcPtr         : not null access C.int;
      argvPtr         : not null access CArgv.Chars_Ptr_Ptr);

   function Tcl_GetAliasObj
     (interp          : not null Tcl_Interp;
      slaveCmd        : String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : CArgv.Chars_Ptr_Ptr;
      objcPtr         : not null access C.int;
      objv            : Tcl_Obj_Array)
     return            C.int;

   procedure Tcl_GetAliasObj
     (interp          : not null Tcl_Interp;
      slaveCmd        : String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : CArgv.Chars_Ptr_Ptr;
      objcPtr         : not null access C.int;
      objv            : Tcl_Obj_Array);

   function Tcl_GetHostName return C.Strings.chars_ptr;

   function Tcl_GetNameOfExecutable return C.Strings.chars_ptr;

   function Tcl_GetPathType (path : String) return Tcl_PathType;

   function Tcl_GetResult (interp : not null Tcl_Interp) return String;

   function Tcl_GetSlave
     (interp    : not null Tcl_Interp;
      slaveName : String)
     return      Tcl_Interp;

   function Tcl_GetStringResult (interp : not null Tcl_Interp) return String;

   function Tcl_GetVar
     (interp  : not null Tcl_Interp;
      varName : String;
      flags   : C.int := TCL_GLOBAL_ONLY)
     return    String;

   function Tcl_GetVar2
     (interp : not null Tcl_Interp;
      part1  : String;
      part2  : String;
      flags  : C.int := TCL_GLOBAL_ONLY)
     return   String;

   function Tcl_GlobalEval
     (interp  : not null Tcl_Interp;
      command : String)
     return    C.int;

   procedure Tcl_GlobalEval (interp : not null Tcl_Interp;
                             command : String);

   function Tcl_HideCommand
     (interp         : not null Tcl_Interp;
      cmdName        : String;
      hiddenCmdToken : String)
     return           C.int;

   procedure Tcl_HideCommand
     (interp         : not null Tcl_Interp;
      cmdName        : String;
      hiddenCmdToken : String);

   function Tcl_JoinPath
     (argc      : C.int;
      argv      : CArgv.Chars_Ptr_Ptr;
      resultPtr : not null Tcl_DString)
     return      String;

   function Tcl_LinkVar
     (interp  : not null Tcl_Interp;
      varName : String;
      addr    : System.Address;
      typ     : C.int)
     return    C.int;

   procedure Tcl_LinkVar
     (interp  : not null Tcl_Interp;
      varName : String;
      addr    : System.Address;
      typ     : C.int);

   function Tcl_Merge
     (argc : C.int;
      argv : CArgv.Chars_Ptr_Ptr)
     return String;

   procedure Tcl_PrintDouble
     (interp : not null Tcl_Interp;
      value  : C.double;
      dst    : String);

   function Tcl_PutEnv (strng : String) return C.int;

   function Tcl_PosixError (interp : not null Tcl_Interp) return String;

   function Tcl_RecordAndEval
     (interp : not null Tcl_Interp;
      cmd    : String;
      flags  : C.int)
     return   C.int;

   procedure Tcl_RecordAndEval
     (interp : not null Tcl_Interp;
      cmd    : String;
      flags  : C.int);

   function Tcl_RegExpCompile
     (interp : not null Tcl_Interp;
      strng  : String)
     return   Tcl_RegExp;

   function Tcl_RegExpExec
     (interp : not null Tcl_Interp;
      regexp : not null Tcl_RegExp;
      str    : String;
      start  : String)
     return   C.int;

   procedure Tcl_RegExpExec
     (interp : not null Tcl_Interp;
      regexp : not null Tcl_RegExp;
      str    : String;
      start  : String);

   function Tcl_RegExpMatch
     (interp  : not null Tcl_Interp;
      str     : String;
      pattern : String)
     return    C.int;

   procedure Tcl_RegExpMatch
     (interp  : not null Tcl_Interp;
      str     : String;
      pattern : String);

   function Tcl_ScanElement
     (str     : String;
      flagPtr : access C.int)
     return    C.int;

   function Tcl_ScanCountedElement
     (str     : String;
      length  : C.int;
      flagPtr : access C.int)
     return    C.int;

   procedure Tcl_SetErrorCode
     (interp  : not null Tcl_Interp;
      String1 : String := "";
      String2 : String := "";
      String3 : String := "";
      String4 : String := "";
      String5 : String := "";
      String6 : String := "";
      String7 : String := "";
      String8 : String := "";
      String9 : String := "");

   procedure Tcl_SetResult (interp : not null Tcl_Interp; str : String);

   function Tcl_SetVar
     (interp   : not null Tcl_Interp;
      varName  : String;
      newValue : String;
      flags    : C.int := TCL_GLOBAL_ONLY)
     return     String;

   procedure Tcl_SetVar
     (interp   : not null Tcl_Interp;
      varName  : String;
      newValue : String;
      flags    : C.int := TCL_GLOBAL_ONLY);

   function Tcl_SetVar2
     (interp   : not null Tcl_Interp;
      part1    : String;
      part2    : String;
      newValue : String;
      flags    : C.int := TCL_GLOBAL_ONLY)
     return     String;

   procedure Tcl_SetVar2
     (interp   : not null Tcl_Interp;
      part1    : String;
      part2    : String;
      newValue : String;
      flags    : C.int := TCL_GLOBAL_ONLY);

   function Tcl_SignalId (sig : C.int) return String;

   function Tcl_SignalMsg (sig : C.int) return String;

   function Tcl_SplitList
     (interp  : not null Tcl_Interp;
      listStr : String;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr)
     return    C.int;

   procedure Tcl_SplitList
     (interp  : not null Tcl_Interp;
      listStr : String;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr);

   procedure Tcl_SplitPath
     (path    : String;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr);

   procedure Tcl_StaticPackage
     (interp       : not null Tcl_Interp;
      pkgName      : String;
      initProc     : not null Tcl_PackageInitProc;
      safeInitProc : Tcl_PackageInitProc);    -- can be null

   function Tcl_StringMatch
     (str     : String;
      pattern : String)
     return    C.int;

   function Tcl_Ungets
     (chan   : not null Tcl_Channel;
      str    : String;
      len    : C.int;
      atHead : C.int)
     return   C.int;

   procedure Tcl_UnlinkVar (interp : not null Tcl_Interp;
                            varName : String);

   function Tcl_UnsetVar
     (interp  : not null Tcl_Interp;
      varName : String;
      flags   : C.int := TCL_GLOBAL_ONLY)
     return    C.int;

   procedure Tcl_UnsetVar
     (interp  : not null Tcl_Interp;
      varName : String;
      flags   : C.int := TCL_GLOBAL_ONLY);

   function Tcl_UnsetVar2
     (interp : not null Tcl_Interp;
      part1  : String;
      part2  : String;
      flags  : C.int := TCL_GLOBAL_ONLY)
     return   C.int;

   procedure Tcl_UnsetVar2
     (interp : not null Tcl_Interp;
      part1  : String;
      part2  : String;
      flags  : C.int := TCL_GLOBAL_ONLY);

   procedure Tcl_UpdateLinkedVar
     (interp  : not null Tcl_Interp;
      varName : String);

   function Tcl_UpVar
     (interp    : not null Tcl_Interp;
      frameName : String;
      varName   : String;
      localName : String;
      flags     : C.int)
     return      C.int;

   procedure Tcl_UpVar
     (interp    : not null Tcl_Interp;
      frameName : String;
      varName   : String;
      localName : String;
      flags     : C.int);

   function Tcl_UpVar2
     (interp    : not null Tcl_Interp;
      frameName : String;
      part1     : String;
      part2     : String;
      localName : String;
      flags     : C.int)
     return      C.int;

   procedure Tcl_UpVar2
     (interp    : not null Tcl_Interp;
      frameName : String;
      part1     : String;
      part2     : String;
      localName : String;
      flags     : C.int);

   function Tcl_VarEval
     (interp  : not null Tcl_Interp;
      String1 : String := "";
      String2 : String := "";
      String3 : String := "";
      String4 : String := "";
      String5 : String := "";
      String6 : String := "";
      String7 : String := "";
      String8 : String := "";
      String9 : String := "")
     return    C.int;

   procedure Tcl_VarEval
     (interp  : not null Tcl_Interp;
      String1 : String := "";
      String2 : String := "";
      String3 : String := "";
      String4 : String := "";
      String5 : String := "";
      String6 : String := "";
      String7 : String := "";
      String8 : String := "";
      String9 : String := "");

   procedure Tcl_WrongNumArgs
     (interp  : not null Tcl_Interp;
      objc    : C.int;
      objv    : Tcl_Obj_Array;
      message : String);

   function Tcl_DumpActiveMemory (fileName : String) return C.int;

   procedure Tcl_ValidateAllMemory (file : String; line : C.int);

   function Tcl_ParseVar
     (interp  : not null Tcl_Interp;
      str     : String;
      termPtr : CArgv.Chars_Ptr_Ptr)
     return    String;

   function Tcl_EvalEx
     (interp   : not null Tcl_Interp;
      script   : String;
      numBytes : C.int;
      flags    : C.int)
     return     C.int;

   procedure Tcl_EvalEx
     (interp   : not null Tcl_Interp;
      script   : String;
      numBytes : C.int;
      flags    : C.int);

   function Tcl_ExternalToUtf
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : String;
      srcLen      : C.int;
      flags       : C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : String;
      dstLen      : C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int)    -- can be null
     return        C.int;

   procedure Tcl_ExternalToUtf
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : String;
      srcLen      : C.int;
      flags       : C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : String;
      dstLen      : C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int);   -- can be null

   function Tcl_ExternalToUtfDString
     (encoding : not null Tcl_Encoding;
      src      : String;
      srcLen   : C.int;
      dsPtr    : not null Tcl_DString)
     return     String;

   function Tcl_GetEncoding
     (interp : not null Tcl_Interp;
      name   : String)
     return   Tcl_Encoding;

   function Tcl_GetEncodingName
     (encoding : not null Tcl_Encoding) return String;

   function Tcl_GetIndexFromObjStruct
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : CArgv.Chars_Ptr_Ptr;
      offset   : C.int;
      msg      : String;
      flags    : C.int;
      indexPtr : not null access C.int)
     return     C.int;

   procedure Tcl_GetIndexFromObjStruct
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : CArgv.Chars_Ptr_Ptr;
      offset   : C.int;
      msg      : String;
      flags    : C.int;
      indexPtr : not null access C.int);

   function Tcl_GetVar2Ex
     (interp : not null Tcl_Interp;
      part1  : String;
      part2  : String;
      flags  : C.int := TCL_GLOBAL_ONLY)
     return   Tcl_Obj;

   function Tcl_NumUtfChars (src : String; len : C.int) return C.int;

   function Tcl_SetSystemEncoding
     (interp : not null Tcl_Interp;
      name   : String)
     return   C.int;

   procedure Tcl_SetSystemEncoding
     (interp : not null Tcl_Interp;
      name   : String);

   function Tcl_SetVar2Ex
     (interp      : not null Tcl_Interp;
      part1       : String;
      part2       : String;
      newValuePtr : not null Tcl_Obj;
      flags       : C.int := TCL_GLOBAL_ONLY)
     return        Tcl_Obj;

   function Tcl_UniCharAtIndex
     (src   : String;
      index : C.int)
     return  Tcl_UniChar;

   function Tcl_UniCharToUtf (ch : C.int; buf : String) return C.int;

   function Tcl_UtfAtIndex
     (src   : String;
      index : C.int)
     return  String;

   function Tcl_UtfCharComplete
     (src  : String;
      len  : C.int)
     return C.int;

   function Tcl_UtfBackslash
     (src     : String;
      readPtr : access C.int;    -- can be null
      dst     : String)
     return    C.int;

   function Tcl_UtfFindFirst (src : String; ch : C.int) return String;

   function Tcl_UtfFindLast (src : String; ch : C.int) return String;

   function Tcl_UtfNext (src : String) return String;

   function Tcl_UtfPrev (src : String; start : String) return String;

   function Tcl_UtfToExternal
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : String;
      srcLen      : C.int;
      flags       : C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : String;
      dstLen      : C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int)    -- can be null
     return        C.int;

   procedure Tcl_UtfToExternal
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : String;
      srcLen      : C.int;
      flags       : C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : String;
      dstLen      : C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int);   -- can be null

   function Tcl_UtfToExternalDString
     (encoding : not null Tcl_Encoding;
      src      : String;
      srcLen   : C.int;
      dsPtr    : not null Tcl_DString)
     return     String;

   function Tcl_UtfToLower (src : String) return C.int;

   function Tcl_UtfToTitle (src : String) return C.int;

   function Tcl_UtfToUniChar
     (src   : String;
      chPtr : access Tcl_UniChar)
     return  C.int;

   function Tcl_UtfToUpper (src : String) return C.int;

   function Tcl_GetString (objPtr : not null Tcl_Obj) return String;

   function Tcl_GetDefaultEncodingDir return C.Strings.chars_ptr;

   procedure Tcl_SetDefaultEncodingDir (path : String);

   function Tcl_UniCharToUtfDString
     (strng    : String;
      numChars : C.int;
      dsPtr    : not null Tcl_DString)
     return      String;

   function Tcl_UtfToUniCharDString
     (strng  : String;
      length : C.int;
      dsPtr  : not null Tcl_DString)
     return    String;

   procedure Tcl_LogCommandInfo
     (interp  : not null Tcl_Interp;
      script  : String;
      command : String;
      length  : C.int);

   function Tcl_ParseBraces
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int;
      termPtr  : CArgv.Chars_Ptr_Ptr)
     return     C.int;

   procedure Tcl_ParseBraces
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int;
      termPtr  : CArgv.Chars_Ptr_Ptr);

   function Tcl_ParseCommand
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      nested   : C.int;
      parsePtr : not null Tcl_Parse)
     return     C.int;

   procedure Tcl_ParseCommand
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      nested   : C.int;
      parsePtr : not null Tcl_Parse);

   function Tcl_ParseExpr
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse)
     return     C.int;

   procedure Tcl_ParseExpr
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse);

   function Tcl_ParseQuotedString
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int;
      termPtr  : CArgv.Chars_Ptr_Ptr)
     return     C.int;

   procedure Tcl_ParseQuotedString
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int;
      termPtr  : CArgv.Chars_Ptr_Ptr);

   function Tcl_ParseVarName
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int)
     return     C.int;

   procedure Tcl_ParseVarName
     (interp   : not null Tcl_Interp;
      strng    : String;
      numBytes : C.int;
      parsePtr : not null Tcl_Parse;
      append   : C.int);

   function Tcl_Chdir (dirName : String) return C.int;

   function Tcl_Access (path : String; mode : C.int) return C.int;

   function Tcl_Stat (path : String) return C.int;

   function Tcl_UtfNcmp
     (s1   : String;
      s2   : String;
      n    : C.unsigned_long)
     return C.int;

   function Tcl_UtfNcasecmp
     (s1   : String;
      s2   : String;
      n    : C.unsigned_long)
     return C.int;

   function Tcl_StringCaseMatch
     (str     : String;
      pattern : String;
      nocase  : C.int)
     return    C.int;

end Tcl.Ada;
