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

   procedure Assert (Interp : not null Tcl_Interp; Return_Code : in C.int);
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

      type Tcl_InterpDeleteProc is access procedure (Data   : in ClientData;
                                                     Interp : in Tcl_Interp);
      pragma Convention (C, Tcl_InterpDeleteProc);

      function Tcl_GetAssocData
        (interp  : in Tcl_Interp;
         name    : in C.Strings.chars_ptr;
         procPtr : access Tcl_InterpDeleteProc)   -- can be null
        return    ClientData;
      pragma Import (C, Tcl_GetAssocData, "Tcl_GetAssocData");

      function Tcl_GetAssocData
        (interp  : in Tcl_Interp;
         name    : in String;
         procPtr : access Tcl_InterpDeleteProc)   -- can be null
        return    ClientData;

      procedure Tcl_SetAssocData
        (interp : in Tcl_Interp;
         name   : in C.Strings.chars_ptr;
         proc   : in Tcl_InterpDeleteProc;
         data   : in ClientData);
      pragma Import (C, Tcl_SetAssocData, "Tcl_SetAssocData");

      procedure Tcl_SetAssocData
        (interp : in Tcl_Interp;
         name   : in String;
         proc   : in Tcl_InterpDeleteProc;
         data   : in ClientData);

   end Generic_AssocData;

   generic
      type ClientData is private;
   package Generic_AsyncEvents is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_AsyncProc is access function
        (data   : in ClientData;
         interp : in Tcl_Interp;
         code   : in C.int)
        return      C.int;
      pragma Convention (C, Tcl_AsyncProc);

      function Tcl_AsyncCreate
        (proc : not null Tcl_AsyncProc;
         data : in ClientData)
        return Tcl_AsyncHandler;
      pragma Import (C, Tcl_AsyncCreate, "Tcl_AsyncCreate");

   end Generic_AsyncEvents;

   generic
      type ClientData is private;
   package Generic_CallWhenDeleted is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_InterpDeleteProc is access procedure
        (data   : in ClientData;
         interp : in Tcl_Interp);
      pragma Convention (C, Tcl_InterpDeleteProc);

      procedure Tcl_CallWhenDeleted
        (interp : not null Tcl_Interp;
         proc   : not null Tcl_InterpDeleteProc;
         data   : in ClientData);
      pragma Import (C, Tcl_CallWhenDeleted, "Tcl_CallWhenDeleted");

      procedure Tcl_DontCallWhenDeleted
        (interp : not null Tcl_Interp;
         proc   : not null Tcl_InterpDeleteProc;
         data   : in ClientData);
      pragma Import (C, Tcl_DontCallWhenDeleted, "Tcl_DontCallWhenDeleted");

   end Generic_CallWhenDeleted;

   generic
      type ClientData is private;
   package Generic_Channel is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_MakeFileChannel
        (handle : in ClientData;
         mode   : in C.int)
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
         chanName     : in C.Strings.chars_ptr;
         instancedata : in ClientData;
         mask         : in C.int)
        return         Tcl_Channel;
      pragma Import (C, Tcl_CreateChannel, "Tcl_CreateChannel");

      function Tcl_CreateChannel
        (typePtr      : not null Tcl_ChannelType;
         chanName     : in String;
         instancedata : in ClientData;
         mask         : in C.int)
        return         Tcl_Channel;

      function Tcl_GetChannelHandle
        (chan      : not null Tcl_Channel;
         direction : in C.int;
         handleptr : in ClientData)
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
        (data : in ClientData;
         mask : in C.int);
      pragma Convention (C, Tcl_ChannelProc);

      procedure Tcl_CreateChannelHandler
        (chan : not null Tcl_Channel;
         mask : in C.int;
         proc : not null Tcl_ChannelProc;
         data : in ClientData);
      pragma Import (C, Tcl_CreateChannelHandler, "Tcl_CreateChannelHandler");

      procedure Tcl_DeleteChannelHandler
        (chan : not null Tcl_Channel;
         proc : not null Tcl_ChannelProc;
         data : in ClientData);
      pragma Import (C, Tcl_DeleteChannelHandler, "Tcl_DeleteChannelHandler");

   end Generic_ChannelHandler;

   generic
      type ClientData is private;
   package Generic_CloseHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_CloseProc is access procedure (data : in ClientData);
      pragma Convention (C, Tcl_CloseProc);

      procedure Tcl_CreateCloseHandler
        (chan : not null Tcl_Channel;
         proc : not null Tcl_CloseProc;
         data : in ClientData);
      pragma Import (C, Tcl_CreateCloseHandler, "Tcl_CreateCloseHandler");

      procedure Tcl_DeleteCloseHandler
        (chan : not null Tcl_Channel;
         proc : not null Tcl_CloseProc;
         data : in ClientData);
      pragma Import (C, Tcl_DeleteCloseHandler, "Tcl_DeleteCloseHandler");

   end Generic_CloseHandler;

   generic
      type ClientData is private;
   package Generic_Command is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_CmdProc is access function
        (data   : in ClientData;
         interp : in Tcl_Interp;
         argc   : in C.int;
         argv   : in CArgv.Chars_Ptr_Ptr)
        return      C.int;
      pragma Convention (C, Tcl_CmdProc);

      type Tcl_CmdDeleteProc is access procedure (data : in ClientData);
      pragma Convention (C, Tcl_CmdDeleteProc);

      function Tcl_CreateCommand
        (interp     : not null Tcl_Interp;
         cmdName    : in C.Strings.chars_ptr;
         proc       : not null Tcl_CmdProc;
         data       : in ClientData;
         deleteProc : in Tcl_CmdDeleteProc)    -- can be null
        return       Tcl_Command;
      pragma Import (C, Tcl_CreateCommand, "Tcl_CreateCommand");

      function Tcl_CreateCommand
        (interp     : not null Tcl_Interp;
         cmdName    : in String;
         proc       : not null Tcl_CmdProc;
         data       : in ClientData;
         deleteProc : in Tcl_CmdDeleteProc)    -- can be null
        return       Tcl_Command;

   end Generic_Command;

   generic
      type ClientData is private;
   package Generic_EventQueueAndNotifier is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_EventSetupProc is access procedure
        (data  : in ClientData;
         flags : in C.int);
      pragma Convention (C, Tcl_EventSetupProc);

      type Tcl_EventCheckProc is access procedure
        (data  : in ClientData;
         flags : in C.int);
      pragma Convention (C, Tcl_EventCheckProc);

      type Tcl_FileProc is access procedure
        (data : in ClientData;
         mask : in C.int);
      pragma Convention (C, Tcl_FileProc);

      type Tcl_EventDeleteProc is access function
        (evPtr : in Tcl_Event;
         data  : in ClientData)
        return     C.int;
      pragma Convention (C, Tcl_EventDeleteProc);

      procedure Tcl_AlertNotifier (data : in ClientData);
      pragma Import (C, Tcl_AlertNotifier, "Tcl_AlertNotifier");

      procedure Tcl_CreateEventSource
        (setupProc : not null Tcl_EventSetupProc;
         checkProc : not null Tcl_EventCheckProc;
         data      : in ClientData);
      pragma Import (C, Tcl_CreateEventSource, "Tcl_CreateEventSource");

      procedure Tcl_CreateFileHandler
        (fd   : in C.int;
         mask : in C.int;
         proc : not null Tcl_FileProc;
         data : in ClientData);
      pragma Import (C, Tcl_CreateFileHandler, "Tcl_CreateFileHandler");

      procedure Tcl_DeleteEventSource
        (setupProc : not null Tcl_EventSetupProc;
         checkProc : not null Tcl_EventCheckProc;
         data      : in ClientData);
      pragma Import (C, Tcl_DeleteEventSource, "Tcl_DeleteEventSource");

      procedure Tcl_DeleteEvents
        (proc : not null Tcl_EventDeleteProc;
         data : in ClientData);
      pragma Import (C, Tcl_DeleteEvents, "Tcl_DeleteEvents");

      procedure Tcl_FinalizeNotifier (data : in ClientData);
      pragma Import (C, Tcl_FinalizeNotifier, "Tcl_FinalizeNotifier");

      function Tcl_InitNotifier return ClientData;
      pragma Import (C, Tcl_InitNotifier, "Tcl_InitNotifier");

   end Generic_EventQueueAndNotifier;

   generic
      type ClientData is private;
   package Generic_ExitHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_ExitProc is access procedure (data : in ClientData);
      pragma Convention (C, Tcl_ExitProc);

      procedure Tcl_CreateExitHandler
        (proc : not null Tcl_ExitProc;
         data : in ClientData);
      pragma Import (C, Tcl_CreateExitHandler, "Tcl_CreateExitHandler");

      procedure Tcl_CreateThreadExitHandler
        (proc : not null Tcl_ExitProc;
         data : in ClientData);
      pragma Import
        (C,
         Tcl_CreateThreadExitHandler,
         "Tcl_CreateThreadExitHandler");

      procedure Tcl_DeleteExitHandler
        (proc : not null Tcl_ExitProc;
         data : in ClientData);
      pragma Import (C, Tcl_DeleteExitHandler, "Tcl_DeleteExitHandler");

      procedure Tcl_DeleteThreadExitHandler
        (proc : not null Tcl_ExitProc;
         data : in ClientData);
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
        (data : in ClientData;
         mask : in C.int);
      pragma Convention (C, Tcl_FileProc);

      procedure Tcl_CreateFileHandler
        (fd   : in C.int;
         mask : in C.int;
         proc : not null Tcl_FileProc;
         data : in ClientData);
      pragma Import (C, Tcl_CreateFileHandler, "Tcl_CreateFileHandler");

   end Generic_FileHandler;

   generic
      type ClientData is private;
   package Generic_GetOpenFile is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_GetOpenFile
        (interp     : not null Tcl_Interp;
         str        : in C.Strings.chars_ptr;
         forWriting : in C.int;
         checkUsage : in C.int;
         fileptr    : in ClientData)
        return       C.int;
      pragma Import (C, Tcl_GetOpenFile, "Tcl_GetOpenFile");

      function Tcl_GetOpenFile
        (interp     : not null  Tcl_Interp;
         str        : in String;
         forWriting : in C.int;
         checkUsage : in C.int;
         fileptr    : in ClientData)
        return       C.int;

      procedure Tcl_GetOpenFile
        (interp     : not null Tcl_Interp;
         str        : in String;
         forWriting : in C.int;
         checkUsage : in C.int;
         fileptr    : in ClientData);

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
         value     : in ClientData);

   end Generic_Hash;

   generic
      type ClientData is private;
   package Generic_Idle is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_IdleProc is access procedure (data : in ClientData);
      pragma Convention (C, Tcl_IdleProc);

      procedure Tcl_CancelIdleCall
        (idleProc : not null Tcl_IdleProc;
         data     : in ClientData);
      pragma Import (C, Tcl_CancelIdleCall, "Tcl_CancelIdleCall");

      procedure Tcl_DoWhenIdle
        (proc : not null Tcl_IdleProc;
         data : in ClientData);
      pragma Import (C, Tcl_DoWhenIdle, "Tcl_DoWhenIdle");

   end Generic_Idle;

   generic
      type ClientData is private;
   package Generic_ManageStorage is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_FreeProc is access procedure
        (blockPtr : in C.Strings.chars_ptr);
      pragma Convention (C, Tcl_FreeProc);

      procedure Tcl_EventuallyFree
        (data     : in ClientData;
         freeProc : not null Tcl_FreeProc);
      pragma Import (C, Tcl_EventuallyFree, "Tcl_EventuallyFree");

      procedure Tcl_Preserve (data : in ClientData);
      pragma Import (C, Tcl_Preserve, "Tcl_Preserve");

      procedure Tcl_Release (data : in ClientData);
      pragma Import (C, Tcl_Release, "Tcl_Release");

   end Generic_ManageStorage;

   generic
      type ClientData is private;
   package Generic_MathFunc is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_MathProc is access function
        (data      : in ClientData;
         interp    : in Tcl_Interp;
         args      : in Tcl_Value;
         resultPtr : in Tcl_Value)
        return         C.int;
      pragma Convention (C, Tcl_MathProc);

      procedure Tcl_CreateMathFunc
        (interp   : not null Tcl_Interp;
         name     : in C.Strings.chars_ptr;
         numArgs  : in C.int;
         argTypes : in Tcl_ValueType;
         proc     : not null Tcl_MathProc;
         data     : in ClientData);
      pragma Import (C, Tcl_CreateMathFunc, "Tcl_CreateMathFunc");

      procedure Tcl_CreateMathFunc
        (interp   : not null Tcl_Interp;
         name     : in String;
         numArgs  : in C.int;
         argTypes : in Tcl_ValueType;
         proc     : not null Tcl_MathProc;
         data     : in ClientData);

   end Generic_MathFunc;

   generic
      type ClientData is private;
   package Generic_ObjCommand is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_ObjCmdProc is access function
        (data   : in ClientData;
         interp : in Tcl_Interp;
         objc   : in C.int)
        return      C.int;
      pragma Convention (C, Tcl_ObjCmdProc);

      type Tcl_CmdDeleteProc is access procedure (data : in ClientData);
      pragma Convention (C, Tcl_CmdDeleteProc);

      function Tcl_CreateObjCommand
        (interp     : not null Tcl_Interp;
         cmdName    : in C.Strings.chars_ptr;
         proc       : not null Tcl_ObjCmdProc;
         data       : in ClientData;
         deleteProc : in Tcl_CmdDeleteProc)
        return       Tcl_Command;
      pragma Import (C, Tcl_CreateObjCommand, "Tcl_CreateObjCommand");

      function Tcl_CreateObjCommand
        (interp     : not null Tcl_Interp;
         cmdName    : in String;
         proc       : not null Tcl_ObjCmdProc;
         data       : in ClientData;
         deleteProc : in Tcl_CmdDeleteProc)
        return       Tcl_Command;

   end Generic_ObjCommand;

   generic
      type ClientData is private;
   package Generic_PkgRequire is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      function Tcl_PkgPresentEx
        (interp        : not null Tcl_Interp;
         name          : in C.Strings.chars_ptr;
         version       : in C.Strings.chars_ptr;
         exact         : in C.int;
         clientdataptr : access ClientData)    -- can be null
        return          C.Strings.chars_ptr;
      pragma Import (C, Tcl_PkgPresentEx, "Tcl_PkgPresentEx");

      function Tcl_PkgPresentEx
        (interp        : not null Tcl_Interp;
         name          : in String;
         version       : in String;
         exact         : in C.int;
         clientdataptr : access ClientData)    -- can be null
        return          String;

      function Tcl_PkgProvideEx
        (interp  : not null Tcl_Interp;
         name    : in C.Strings.chars_ptr;
         version : in C.Strings.chars_ptr;
         data    : in ClientData)
        return    C.int;
      pragma Import (C, Tcl_PkgProvideEx, "Tcl_PkgProvideEx");

      function Tcl_PkgProvideEx
        (interp  : not null Tcl_Interp;
         name    : in String;
         version : in String;
         data    : in ClientData)
        return    C.int;

      procedure Tcl_PkgProvideEx
        (interp  : not null Tcl_Interp;
         name    : in String;
         version : in String;
         data    : in ClientData);

      function Tcl_PkgRequireEx
        (interp        : not null Tcl_Interp;
         name          : in C.Strings.chars_ptr;
         version       : in C.Strings.chars_ptr;
         exact         : in C.int;
         clientdataptr : access ClientData)    -- can be null
        return          C.Strings.chars_ptr;
      pragma Import (C, Tcl_PkgRequireEx, "Tcl_PkgRequireEx");

      function Tcl_PkgRequireEx
        (interp        : not null Tcl_Interp;
         name          : in String;
         version       : in String;
         exact         : in C.int;
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
         instancedata : in ClientData;
         mask         : in C.int;
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
        (callbackdata : in ClientData;
         chan         : in Tcl_Channel;
         address      : in System.Address;
         port         : in C.int);
      pragma Convention (C, Tcl_TcpAcceptProc);

      function Tcl_MakeTcpClientChannel
        (tcpsocket : in ClientData)
        return      Tcl_Channel;
      pragma Import (C, Tcl_MakeTcpClientChannel, "Tcl_MakeTcpClientChannel");

      function Tcl_OpenTcpServer
        (interp       : not null Tcl_Interp;
         port         : in C.int;
         host         : in C.Strings.chars_ptr;
         acceptProc   : not null Tcl_TcpAcceptProc;
         callbackdata : in ClientData)
        return         Tcl_Channel;
      pragma Import (C, Tcl_OpenTcpServer, "Tcl_OpenTcpServer");

      function Tcl_OpenTcpServer
        (interp       : not null Tcl_Interp;
         port         : in C.int;
         host         : in String;
         acceptProc   : not null Tcl_TcpAcceptProc;
         callbackdata : in ClientData)
        return         Tcl_Channel;

   end Generic_TcpChannel;

   generic
      type ClientData is private;
   package Generic_TimerHandler is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_TimerProc is access procedure (data : in ClientData);
      pragma Convention (C, Tcl_TimerProc);

      function Tcl_CreateTimerHandler
        (milliseconds : in C.int;
         proc         : not null Tcl_TimerProc;
         data         : in ClientData)
        return         Tcl_TimerToken;
      pragma Import (C, Tcl_CreateTimerHandler, "Tcl_CreateTimerHandler");

   end Generic_TimerHandler;

   generic
      type ClientData is private;
   package Generic_Trace is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

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

      function Tcl_CreateTrace
        (interp : not null Tcl_Interp;
         level  : in C.int;
         proc   : not null Tcl_CmdTraceProc;
         data   : in ClientData)
        return   Tcl_Trace;
      pragma Import (C, Tcl_CreateTrace, "Tcl_CreateTrace");

   end Generic_Trace;

   generic
      type ClientData is private;
   package Generic_TraceVar is

      pragma Assert (ClientData'Size <= System.Address'Size,
                     "ClientData too big");

      type Tcl_VarTraceProc is access function
        (data   : in ClientData;
         interp : in Tcl_Interp;
         part1  : in C.Strings.chars_ptr;
         part2  : in C.Strings.chars_ptr;
         flags  : in C.int)
        return      C.Strings.chars_ptr;
      pragma Convention (C, Tcl_VarTraceProc);

      function Tcl_TraceVar
        (interp  : not null Tcl_Interp;
         varName : in C.Strings.chars_ptr;
         flags   : in C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : in ClientData)
        return    C.int;
      pragma Import (C, Tcl_TraceVar, "Tcl_TraceVar");

      function Tcl_TraceVar
        (interp  : not null Tcl_Interp;
         varName : in String;
         flags   : in C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : in ClientData)
        return    C.int;

      procedure Tcl_TraceVar
        (interp  : not null Tcl_Interp;
         varName : in String;
         flags   : in C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : in ClientData);

      function Tcl_TraceVar2
        (interp : not null Tcl_Interp;
         part1  : in C.Strings.chars_ptr;
         part2  : in C.Strings.chars_ptr;
         flags  : in C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : in ClientData)
        return   C.int;
      pragma Import (C, Tcl_TraceVar2, "Tcl_TraceVar2");

      function Tcl_TraceVar2
        (interp : not null Tcl_Interp;
         part1  : in String;
         part2  : in String;
         flags  : in C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : in ClientData)
        return   C.int;

      procedure Tcl_TraceVar2
        (interp : not null Tcl_Interp;
         part1  : in String;
         part2  : in String;
         flags  : in C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : in ClientData);

      procedure Tcl_UntraceVar
        (interp  : not null Tcl_Interp;
         varName : in C.Strings.chars_ptr;
         flags   : in C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : in ClientData);
      pragma Import (C, Tcl_UntraceVar, "Tcl_UntraceVar");

      procedure Tcl_UntraceVar
        (interp  : not null Tcl_Interp;
         varName : in String;
         flags   : in C.int;
         proc    : not null Tcl_VarTraceProc;
         data    : in ClientData);

      procedure Tcl_UntraceVar2
        (interp : not null Tcl_Interp;
         part1  : in C.Strings.chars_ptr;
         part2  : in C.Strings.chars_ptr;
         flags  : in C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : in ClientData);
      pragma Import (C, Tcl_UntraceVar2, "Tcl_UntraceVar2");

      procedure Tcl_UntraceVar2
        (interp : not null Tcl_Interp;
         part1  : in String;
         part2  : in String;
         flags  : in C.int;
         proc   : not null Tcl_VarTraceProc;
         data   : in ClientData);

      function Tcl_VarTraceInfo
        (interp         : not null Tcl_Interp;
         varName        : in C.Strings.chars_ptr;
         flags          : in C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : in ClientData)
        return           ClientData;
      pragma Import (C, Tcl_VarTraceInfo, "Tcl_VarTraceInfo");

      function Tcl_VarTraceInfo
        (interp         : not null Tcl_Interp;
         varName        : in String;
         flags          : in C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : in ClientData)
        return           ClientData;

      function Tcl_VarTraceInfo2
        (interp         : not null Tcl_Interp;
         part1          : in C.Strings.chars_ptr;
         part2          : in C.Strings.chars_ptr;
         flags          : in C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : in ClientData)
        return           ClientData;
      pragma Import (C, Tcl_VarTraceInfo2, "Tcl_VarTraceInfo2");

      function Tcl_VarTraceInfo2
        (interp         : not null Tcl_Interp;
         part1          : in String;
         part2          : in String;
         flags          : in C.int;
         procPtr        : not null Tcl_VarTraceProc;
         prevclientdata : in ClientData)
        return           ClientData;

   end Generic_TraceVar;

   function Tcl_DStringValue (dsPtr : not null Tcl_DString) return String;

   function Tcl_DbCkalloc
     (size : in C.unsigned;
      file : in String;
      line : in C.int)
     return String;

   function Tcl_DbCkfree
     (ptr  : in String;
      file : in String;
      line : in C.int)
     return C.int;

   function Tcl_DbCkrealloc
     (ptr  : in String;
      size : in C.unsigned;
      file : in String;
      line : in C.int)
     return String;

   procedure Tcl_AppendStringsToObj
     (objPtr  : not null Tcl_Obj;
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "");

   procedure Tcl_AppendToObj
     (objPtr : not null Tcl_Obj;
      bytes  : in String;
      length : in C.int);

   procedure Tcl_DbDecrRefCount
     (objPtr : not null Tcl_Obj;
      file   : in String;
      line   : in C.int);

   procedure Tcl_DbIncrRefCount
     (objPtr : not null Tcl_Obj;
      file   : in String;
      line   : in C.int);

   function Tcl_DbIsShared
     (objPtr : not null Tcl_Obj;
      file   : in String;
      line   : in C.int)
     return   C.int;

   function Tcl_DbNewBooleanObj
     (boolValue : in C.int;
      file      : in String;
      line      : in C.int)
     return      Tcl_Obj;

   function Tcl_DbNewByteArrayObj
     (bytes  : in String;
      length : in C.int;
      file   : in String;
      line   : in C.int)
     return   Tcl_Obj;

   function Tcl_DbNewDoubleObj
     (doubleValue : in C.double;
      file        : in String;
      line        : in C.int)
     return        Tcl_Obj;

   function Tcl_DbNewListObj
     (objc : in C.int;
      objv : in Tcl_Obj_Array;
      file : in String;
      line : in C.int)
     return Tcl_Obj;

   function Tcl_DbNewLongObj
     (longValue : in C.long;
      file      : in String;
      line      : in C.int)
     return      Tcl_Obj;

   function Tcl_DbNewObj (file : in String; line : in C.int) return Tcl_Obj;

   function Tcl_DbNewStringObj
     (bytes  : in String;
      length : in C.int;
      file   : in String;
      line   : in C.int)
     return   Tcl_Obj;

   function Tcl_GetBoolean
     (interp  : not null Tcl_Interp;
      str     : in String;
      boolPtr : not null access C.int)
     return    C.int;

   procedure Tcl_GetBoolean
     (interp  : not null Tcl_Interp;
      str     : in String;
      boolPtr : not null access C.int);

   function Tcl_GetByteArrayFromObj
     (objPtr    : not null Tcl_Obj;
      lengthPtr : access C.int)    -- can be null
     return      String;

   function Tcl_GetDouble
     (interp    : not null Tcl_Interp;
      str       : in String;
      doublePtr : not null access C.double)
     return      C.int;

   procedure Tcl_GetDouble
     (interp    : not null Tcl_Interp;
      str       : in String;
      doublePtr : not null access C.double);

   function Tcl_GetIndexFromObj
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      msg      : in String;
      flags    : in C.int;
      indexPtr : not null access C.int)
     return     C.int;

   procedure Tcl_GetIndexFromObj
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      msg      : in String;
      flags    : in C.int;
      indexPtr : not null access C.int);

   function Tcl_GetInt
     (interp : not null Tcl_Interp;
      str    : in String;
      intPtr : not null access C.int)
     return   C.int;

   procedure Tcl_GetInt
     (interp : not null Tcl_Interp;
      str    : in String;
      intPtr : not null access C.int);

   function Tcl_GetObjType (typeName : in String) return Tcl_ObjType;

   function Tcl_GetObjTypeName (objPtr : not null Tcl_Obj) return String;

   function Tcl_GetStringFromObj
     (objPtr    : not null Tcl_Obj;
      lengthPtr : access C.int)    -- can be null
     return      String;

   function Tcl_NewByteArrayObj
     (bytes  : in String;
      length : in C.int)
     return   Tcl_Obj;

   function Tcl_NewStringObj
     (bytes  : in String;
      length : in C.int)
     return   Tcl_Obj;

   function Tcl_SetByteArrayLength
     (objPtr : not null Tcl_Obj;
      length : in C.int)
     return   String;

   procedure Tcl_SetByteArrayObj
     (objPtr : not null Tcl_Obj;
      bytes  : in String;
      length : in C.int);

   procedure Tcl_SetStringObj
     (objPtr : not null Tcl_Obj;
      bytes  : in String;
      length : in C.int);

   procedure Tcl_AddErrorInfo (interp : not null Tcl_Interp;
                               message : in String);

   procedure Tcl_AddObjErrorInfo
     (interp  : not null Tcl_Interp;
      message : in String;
      length  : in C.int);

   procedure Tcl_AppendElement (interp : not null Tcl_Interp;
                                strng : in String);

   procedure Tcl_AppendResult
     (interp  : not null Tcl_Interp;
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "");

   function Tcl_Backslash
     (src     : in String;
      readPtr : access C.int)    -- can be null
     return    C.char;

   function Tcl_CommandComplete (cmd : in String) return C.int;

   function Tcl_Concat
     (argc : in C.int;
      argv : in CArgv.Chars_Ptr_Ptr)
     return String;

   function Tcl_ConvertElement
     (src   : in String;
      dst   : in String;
      flags : in C.int)
     return  C.int;

   function Tcl_ConvertCountedElement
     (src    : in String;
      length : in C.int;
      dst    : in String;
      flags  : in C.int)
     return   C.int;

   function Tcl_CreateAlias
     (slave     : not null Tcl_Interp;
      slaveCmd  : in String;
      target    : not null Tcl_Interp;
      targetCmd : in String;
      argc      : in C.int;
      argv      : in CArgv.Chars_Ptr_Ptr)
     return      C.int;

   function Tcl_CreateAliasObj
     (slave     : not null Tcl_Interp;
      slaveCmd  : in String;
      target    : not null Tcl_Interp;
      targetCmd : in String;
      objc      : in C.int;
      objv      : in Tcl_Obj_Array)
     return      C.int;

   function Tcl_CreateSlave
     (interp    : not null Tcl_Interp;
      slaveName : in String;
      isSafe    : in C.int)
     return      Tcl_Interp;

   function Tcl_DStringAppend
     (dsPtr  : not null Tcl_DString;
      str    : in String;
      length : in C.int)
     return   String;

   function Tcl_DStringAppendElement
     (dsPtr : not null Tcl_DString;
      strng : in String)
     return  String;

   function Tcl_ErrnoId return C.Strings.chars_ptr;

   function Tcl_ErrnoMsg (err : in C.int) return String;

   function Tcl_Eval
     (interp : not null Tcl_Interp;
      strng  : in String)
     return   C.int;

   procedure Tcl_Eval (interp : not null Tcl_Interp; strng : in String);

   function Tcl_EvalFile
     (interp   : not null Tcl_Interp;
      fileName : in String)
     return     C.int;

   procedure Tcl_EvalFile (interp : not null Tcl_Interp;
                           fileName : in String);

   function Tcl_ExposeCommand
     (interp         : not null Tcl_Interp;
      hiddenCmdToken : in String;
      cmdName        : in String)
     return           C.int;

   procedure Tcl_ExposeCommand
     (interp         : not null Tcl_Interp;
      hiddenCmdToken : in String;
      cmdName        : in String);

   function Tcl_ExprBoolean
     (interp : not null Tcl_Interp;
      str    : in String)
     return   Boolean;

   function Tcl_ExprDouble
     (interp : not null Tcl_Interp;
      str    : in String)
     return   C.double;

   function Tcl_ExprLong
     (interp : not null Tcl_Interp;
      str    : in String)
     return   C.long;

   function Tcl_ExprString
     (interp : not null Tcl_Interp;
      strng  : in String)
     return   String;

   procedure Tcl_FindExecutable (argv0 : in String);

   function Tcl_GetAlias
     (interp          : not null Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      argcPtr         : not null access C.int;
      argvPtr         : not null access CArgv.Chars_Ptr_Ptr)
     return            C.int;

   procedure Tcl_GetAlias
     (interp          : not null Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      argcPtr         : not null access C.int;
      argvPtr         : not null access CArgv.Chars_Ptr_Ptr);

   function Tcl_GetAliasObj
     (interp          : not null Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      objcPtr         : not null access C.int;
      objv            : in Tcl_Obj_Array)
     return            C.int;

   procedure Tcl_GetAliasObj
     (interp          : not null Tcl_Interp;
      slaveCmd        : in String;
      targetInterpPtr : not null access Tcl_Interp;
      targetCmdPtr    : in CArgv.Chars_Ptr_Ptr;
      objcPtr         : not null access C.int;
      objv            : in Tcl_Obj_Array);

   function Tcl_GetHostName return C.Strings.chars_ptr;

   function Tcl_GetNameOfExecutable return C.Strings.chars_ptr;

   function Tcl_GetPathType (path : in String) return Tcl_PathType;

   function Tcl_GetResult (interp : not null Tcl_Interp) return String;

   function Tcl_GetSlave
     (interp    : not null Tcl_Interp;
      slaveName : in String)
     return      Tcl_Interp;

   function Tcl_GetStringResult (interp : not null Tcl_Interp) return String;

   function Tcl_GetVar
     (interp  : not null Tcl_Interp;
      varName : in String;
      flags   : in C.int := TCL_GLOBAL_ONLY)
     return    String;

   function Tcl_GetVar2
     (interp : not null Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY)
     return   String;

   function Tcl_GlobalEval
     (interp  : not null Tcl_Interp;
      command : in String)
     return    C.int;

   procedure Tcl_GlobalEval (interp : not null Tcl_Interp;
                             command : in String);

   function Tcl_HideCommand
     (interp         : not null Tcl_Interp;
      cmdName        : in String;
      hiddenCmdToken : in String)
     return           C.int;

   procedure Tcl_HideCommand
     (interp         : not null Tcl_Interp;
      cmdName        : in String;
      hiddenCmdToken : in String);

   function Tcl_JoinPath
     (argc      : in C.int;
      argv      : in CArgv.Chars_Ptr_Ptr;
      resultPtr : not null Tcl_DString)
     return      String;

   function Tcl_LinkVar
     (interp  : not null Tcl_Interp;
      varName : in String;
      addr    : in System.Address;
      typ     : in C.int)
     return    C.int;

   procedure Tcl_LinkVar
     (interp  : not null Tcl_Interp;
      varName : in String;
      addr    : in System.Address;
      typ     : in C.int);

   function Tcl_Merge
     (argc : in C.int;
      argv : in CArgv.Chars_Ptr_Ptr)
     return String;

   procedure Tcl_PrintDouble
     (interp : not null Tcl_Interp;
      value  : in C.double;
      dst    : in String);

   function Tcl_PutEnv (strng : in String) return C.int;

   function Tcl_PosixError (interp : not null Tcl_Interp) return String;

   function Tcl_RecordAndEval
     (interp : not null Tcl_Interp;
      cmd    : in String;
      flags  : in C.int)
     return   C.int;

   procedure Tcl_RecordAndEval
     (interp : not null Tcl_Interp;
      cmd    : in String;
      flags  : in C.int);

   function Tcl_RegExpCompile
     (interp : not null Tcl_Interp;
      strng  : in String)
     return   Tcl_RegExp;

   function Tcl_RegExpExec
     (interp : not null Tcl_Interp;
      regexp : not null Tcl_RegExp;
      str    : in String;
      start  : in String)
     return   C.int;

   procedure Tcl_RegExpExec
     (interp : not null Tcl_Interp;
      regexp : not null Tcl_RegExp;
      str    : in String;
      start  : in String);

   function Tcl_RegExpMatch
     (interp  : not null Tcl_Interp;
      str     : in String;
      pattern : in String)
     return    C.int;

   procedure Tcl_RegExpMatch
     (interp  : not null Tcl_Interp;
      str     : in String;
      pattern : in String);

   function Tcl_ScanElement
     (str     : in String;
      flagPtr : access C.int)
     return    C.int;

   function Tcl_ScanCountedElement
     (str     : in String;
      length  : in C.int;
      flagPtr : access C.int)
     return    C.int;

   procedure Tcl_SetErrorCode
     (interp  : not null Tcl_Interp;
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "");

   procedure Tcl_SetResult (interp : not null Tcl_Interp; str : in String);

   function Tcl_SetVar
     (interp   : not null Tcl_Interp;
      varName  : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY)
     return     String;

   procedure Tcl_SetVar
     (interp   : not null Tcl_Interp;
      varName  : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY);

   function Tcl_SetVar2
     (interp   : not null Tcl_Interp;
      part1    : in String;
      part2    : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY)
     return     String;

   procedure Tcl_SetVar2
     (interp   : not null Tcl_Interp;
      part1    : in String;
      part2    : in String;
      newValue : in String;
      flags    : in C.int := TCL_GLOBAL_ONLY);

   function Tcl_SignalId (sig : in C.int) return String;

   function Tcl_SignalMsg (sig : in C.int) return String;

   function Tcl_SplitList
     (interp  : not null Tcl_Interp;
      listStr : in String;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr)
     return    C.int;

   procedure Tcl_SplitList
     (interp  : not null Tcl_Interp;
      listStr : in String;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr);

   procedure Tcl_SplitPath
     (path    : in String;
      argcPtr : not null access C.int;
      argvPtr : not null access CArgv.Chars_Ptr_Ptr);

   procedure Tcl_StaticPackage
     (interp       : not null Tcl_Interp;
      pkgName      : in String;
      initProc     : not null Tcl_PackageInitProc;
      safeInitProc : Tcl_PackageInitProc);    -- can be null

   function Tcl_StringMatch
     (str     : in String;
      pattern : in String)
     return    C.int;

   function Tcl_Ungets
     (chan   : not null Tcl_Channel;
      str    : in String;
      len    : in C.int;
      atHead : in C.int)
     return   C.int;

   procedure Tcl_UnlinkVar (interp : not null Tcl_Interp;
                            varName : in String);

   function Tcl_UnsetVar
     (interp  : not null Tcl_Interp;
      varName : in String;
      flags   : in C.int := TCL_GLOBAL_ONLY)
     return    C.int;

   procedure Tcl_UnsetVar
     (interp  : not null Tcl_Interp;
      varName : in String;
      flags   : in C.int := TCL_GLOBAL_ONLY);

   function Tcl_UnsetVar2
     (interp : not null Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY)
     return   C.int;

   procedure Tcl_UnsetVar2
     (interp : not null Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY);

   procedure Tcl_UpdateLinkedVar
     (interp  : not null Tcl_Interp;
      varName : in String);

   function Tcl_UpVar
     (interp    : not null Tcl_Interp;
      frameName : in String;
      varName   : in String;
      localName : in String;
      flags     : in C.int)
     return      C.int;

   procedure Tcl_UpVar
     (interp    : not null Tcl_Interp;
      frameName : in String;
      varName   : in String;
      localName : in String;
      flags     : in C.int);

   function Tcl_UpVar2
     (interp    : not null Tcl_Interp;
      frameName : in String;
      part1     : in String;
      part2     : in String;
      localName : in String;
      flags     : in C.int)
     return      C.int;

   procedure Tcl_UpVar2
     (interp    : not null Tcl_Interp;
      frameName : in String;
      part1     : in String;
      part2     : in String;
      localName : in String;
      flags     : in C.int);

   function Tcl_VarEval
     (interp  : not null Tcl_Interp;
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "")
     return    C.int;

   procedure Tcl_VarEval
     (interp  : not null Tcl_Interp;
      String1 : in String := "";
      String2 : in String := "";
      String3 : in String := "";
      String4 : in String := "";
      String5 : in String := "";
      String6 : in String := "";
      String7 : in String := "";
      String8 : in String := "";
      String9 : in String := "");

   procedure Tcl_WrongNumArgs
     (interp  : not null Tcl_Interp;
      objc    : in C.int;
      objv    : in Tcl_Obj_Array;
      message : in String);

   function Tcl_DumpActiveMemory (fileName : in String) return C.int;

   procedure Tcl_ValidateAllMemory (file : in String; line : in C.int);

   function Tcl_ParseVar
     (interp  : not null Tcl_Interp;
      str     : in String;
      termPtr : in CArgv.Chars_Ptr_Ptr)
     return    String;

   function Tcl_EvalEx
     (interp   : not null Tcl_Interp;
      script   : in String;
      numBytes : in C.int;
      flags    : in C.int)
     return     C.int;

   procedure Tcl_EvalEx
     (interp   : not null Tcl_Interp;
      script   : in String;
      numBytes : in C.int;
      flags    : in C.int);

   function Tcl_ExternalToUtf
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int)    -- can be null
     return        C.int;

   procedure Tcl_ExternalToUtf
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int);   -- can be null

   function Tcl_ExternalToUtfDString
     (encoding : not null Tcl_Encoding;
      src      : in String;
      srcLen   : in C.int;
      dsPtr    : not null Tcl_DString)
     return     String;

   function Tcl_GetEncoding
     (interp : not null Tcl_Interp;
      name   : in String)
     return   Tcl_Encoding;

   function Tcl_GetEncodingName
     (encoding : not null Tcl_Encoding) return String;

   function Tcl_GetIndexFromObjStruct
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      offset   : in C.int;
      msg      : in String;
      flags    : in C.int;
      indexPtr : not null access C.int)
     return     C.int;

   procedure Tcl_GetIndexFromObjStruct
     (interp   : not null Tcl_Interp;
      objPtr   : not null Tcl_Obj;
      tablePtr : in CArgv.Chars_Ptr_Ptr;
      offset   : in C.int;
      msg      : in String;
      flags    : in C.int;
      indexPtr : not null access C.int);

   function Tcl_GetVar2Ex
     (interp : not null Tcl_Interp;
      part1  : in String;
      part2  : in String;
      flags  : in C.int := TCL_GLOBAL_ONLY)
     return   Tcl_Obj;

   function Tcl_NumUtfChars (src : in String; len : in C.int) return C.int;

   function Tcl_SetSystemEncoding
     (interp : not null Tcl_Interp;
      name   : in String)
     return   C.int;

   procedure Tcl_SetSystemEncoding
     (interp : not null Tcl_Interp;
      name   : in String);

   function Tcl_SetVar2Ex
     (interp      : not null Tcl_Interp;
      part1       : in String;
      part2       : in String;
      newValuePtr : not null Tcl_Obj;
      flags       : in C.int := TCL_GLOBAL_ONLY)
     return        Tcl_Obj;

   function Tcl_UniCharAtIndex
     (src   : in String;
      index : in C.int)
     return  Tcl_UniChar;

   function Tcl_UniCharToUtf (ch : in C.int; buf : in String) return C.int;

   function Tcl_UtfAtIndex
     (src   : in String;
      index : in C.int)
     return  String;

   function Tcl_UtfCharComplete
     (src  : in String;
      len  : in C.int)
     return C.int;

   function Tcl_UtfBackslash
     (src     : in String;
      readPtr : access C.int;    -- can be null
      dst     : in String)
     return    C.int;

   function Tcl_UtfFindFirst (src : in String; ch : in C.int) return String;

   function Tcl_UtfFindLast (src : in String; ch : in C.int) return String;

   function Tcl_UtfNext (src : in String) return String;

   function Tcl_UtfPrev (src : in String; start : in String) return String;

   function Tcl_UtfToExternal
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int)    -- can be null
     return        C.int;

   procedure Tcl_UtfToExternal
     (interp      : not null Tcl_Interp;
      encoding    : not null Tcl_Encoding;
      src         : in String;
      srcLen      : in C.int;
      flags       : in C.int;
      statePtr    : not null Tcl_EncodingState;
      dst         : in String;
      dstLen      : in C.int;
      srcReadPtr  : access C.int;    -- can be null
      dstWrotePtr : access C.int;    -- can be null
      dstCharsPtr : access C.int);   -- can be null

   function Tcl_UtfToExternalDString
     (encoding : not null Tcl_Encoding;
      src      : in String;
      srcLen   : in C.int;
      dsPtr    : not null Tcl_DString)
     return     String;

   function Tcl_UtfToLower (src : in String) return C.int;

   function Tcl_UtfToTitle (src : in String) return C.int;

   function Tcl_UtfToUniChar
     (src   : in String;
      chPtr : access Tcl_UniChar)
     return  C.int;

   function Tcl_UtfToUpper (src : in String) return C.int;

   function Tcl_GetString (objPtr : not null Tcl_Obj) return String;

   function Tcl_GetDefaultEncodingDir return C.Strings.chars_ptr;

   procedure Tcl_SetDefaultEncodingDir (path : in String);

   function Tcl_UniCharToUtfDString
     (strng    : in String;
      numChars : in C.int;
      dsPtr    : not null Tcl_DString)
     return      String;

   function Tcl_UtfToUniCharDString
     (strng  : in String;
      length : in C.int;
      dsPtr  : not null Tcl_DString)
     return    String;

   procedure Tcl_LogCommandInfo
     (interp  : not null Tcl_Interp;
      script  : in String;
      command : in String;
      length  : in C.int);

   function Tcl_ParseBraces
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
     return     C.int;

   procedure Tcl_ParseBraces
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr);

   function Tcl_ParseCommand
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      nested   : in C.int;
      parsePtr : not null Tcl_Parse)
     return     C.int;

   procedure Tcl_ParseCommand
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      nested   : in C.int;
      parsePtr : not null Tcl_Parse);

   function Tcl_ParseExpr
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse)
     return     C.int;

   procedure Tcl_ParseExpr
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse);

   function Tcl_ParseQuotedString
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr)
     return     C.int;

   procedure Tcl_ParseQuotedString
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse;
      append   : in C.int;
      termPtr  : in CArgv.Chars_Ptr_Ptr);

   function Tcl_ParseVarName
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse;
      append   : in C.int)
     return     C.int;

   procedure Tcl_ParseVarName
     (interp   : not null Tcl_Interp;
      strng    : in String;
      numBytes : in C.int;
      parsePtr : not null Tcl_Parse;
      append   : in C.int);

   function Tcl_Chdir (dirName : in String) return C.int;

   function Tcl_Access (path : in String; mode : in C.int) return C.int;

   function Tcl_Stat (path : in String) return C.int;

   function Tcl_UtfNcmp
     (s1   : in String;
      s2   : in String;
      n    : in C.unsigned_long)
     return C.int;

   function Tcl_UtfNcasecmp
     (s1   : in String;
      s2   : in String;
      n    : in C.unsigned_long)
     return C.int;

   function Tcl_StringCaseMatch
     (str     : in String;
      pattern : in String;
      nocase  : in C.int)
     return    C.int;

end Tcl.Ada;
