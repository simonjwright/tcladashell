--------------------------------------------------------------------
--
--  tcl-tk.ads -- This package is the "thin" binding to Tcl.Tk.
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
--
--  This package was automatically generated from tk.h.
--  Note that some of the comments below, preserved from tk.h,
--  do not apply to the Ada version.  Someday, these comments may be
--  customized better.
--
--------------------------------------------------------------------

with CArgv;
with CHelper;
with Interfaces.C.Strings;
with Tcl;

package Tcl.Tk is

   package C renames Interfaces.C;

   --
   --  tk.h --
   --
   --   Declarations for Tk-related things that are visible
   --   outside of the Tk module itself.
   --
   --  Copyright {c} 1989-1994 The Regents of the University of California.
   --  Copyright {c} 1994 The Australian National University.
   --  Copyright {c} 1994-1998 Sun Microsystems, Inc.
   --  Copyright {c} 1998-1999 Scriptics Corporation.
   --
   --
   --

   --
   --  For C++ compilers, use extern
   --

   --
   --  When version numbers change here, you must also go into the following
   --  files
   --  and update the version numbers:
   --
   --  library/tk.tcl   {only if Major.minor changes, not patchlevel}
   --  unix/configure.in
   --  win/configure.in
   --  win/makefile.vc  {not patchlevel}
   --  README
   --  mac/README               {not patchlevel}
   --  win/README               {not patchlevel}
   --  unix/README              {not patchlevel}
   --  win/aclocal.m4   {not patchlevel}
   --
   --  You may also need to update some of these files when the numbers change
   --  for the version of Tcl that this release of Tk is compiled against.
   --

   TK_MAJOR_VERSION  : constant := 8;
   TK_MINOR_VERSION  : constant := 3;
   TK_RELEASE_LEVEL  : constant String := "TCL_FINAL_RELEASE";
   TK_RELEASE_SERIAL : constant := 0;
   TK_VERSION        : constant String := "8.3";
   TK_PATCH_LEVEL    : constant String := "8.3.0";
   --
   --  The following definitions set up the proper options for Macintosh
   --  compilers.  We use this method because there is no autoconf equivalent.
   --

   --
   --  A special definition used to allow this header file to be included
   --  in resource files.
   --

   --
   --  Decide whether or not to use input methods.
   --

   --
   --  Dummy types that are used by clients:
   --

   type Tk_BindingTable_Rec is private;
   type Tk_BindingTable is access all Tk_BindingTable_Rec;
   pragma Convention (C, Tk_BindingTable);

   Null_Tk_BindingTable : constant Tk_BindingTable;

   function Is_Null (Ptr : in Tk_BindingTable) return Boolean;

   type Tk_Canvas_Rec is private;
   type Tk_Canvas is access all Tk_Canvas_Rec;
   pragma Convention (C, Tk_Canvas);

   Null_Tk_Canvas : constant Tk_Canvas;

   function Is_Null (Ptr : in Tk_Canvas) return Boolean;

   type Tk_Cursor_Rec is private;
   type Tk_Cursor is access all Tk_Cursor_Rec;
   pragma Convention (C, Tk_Cursor);

   Null_Tk_Cursor : constant Tk_Cursor;

   function Is_Null (Ptr : in Tk_Cursor) return Boolean;

   type Tk_ErrorHandler_Rec is private;
   type Tk_ErrorHandler is access all Tk_ErrorHandler_Rec;
   pragma Convention (C, Tk_ErrorHandler);

   Null_Tk_ErrorHandler : constant Tk_ErrorHandler;

   function Is_Null (Ptr : in Tk_ErrorHandler) return Boolean;

   type Tk_Font_Rec is private;
   type Tk_Font is access all Tk_Font_Rec;
   pragma Convention (C, Tk_Font);

   Null_Tk_Font : constant Tk_Font;

   function Is_Null (Ptr : in Tk_Font) return Boolean;

   type Tk_Image_Rec is private;
   type Tk_Image is access all Tk_Image_Rec;
   pragma Convention (C, Tk_Image);

   Null_Tk_Image : constant Tk_Image;

   function Is_Null (Ptr : in Tk_Image) return Boolean;

   type Tk_ImageMaster_Rec is private;
   type Tk_ImageMaster is access all Tk_ImageMaster_Rec;
   pragma Convention (C, Tk_ImageMaster);

   Null_Tk_ImageMaster : constant Tk_ImageMaster;

   function Is_Null (Ptr : in Tk_ImageMaster) return Boolean;

   type Tk_OptionTable_Rec is private;
   type Tk_OptionTable is access all Tk_OptionTable_Rec;
   pragma Convention (C, Tk_OptionTable);

   Null_Tk_OptionTable : constant Tk_OptionTable;

   function Is_Null (Ptr : in Tk_OptionTable) return Boolean;

   type Tk_PostscriptInfo_Rec is private;
   type Tk_PostscriptInfo is access all Tk_PostscriptInfo_Rec;
   pragma Convention (C, Tk_PostscriptInfo);

   Null_Tk_PostscriptInfo : constant Tk_PostscriptInfo;

   function Is_Null (Ptr : in Tk_PostscriptInfo) return Boolean;

   type Tk_TextLayout_Rec is private;
   type Tk_TextLayout is access all Tk_TextLayout_Rec;
   pragma Convention (C, Tk_TextLayout);

   Null_Tk_TextLayout : constant Tk_TextLayout;

   function Is_Null (Ptr : in Tk_TextLayout) return Boolean;

   type Tk_Window_Rec is private;
   type Tk_Window is access all Tk_Window_Rec;
   pragma Convention (C, Tk_Window);

   Null_Tk_Window : constant Tk_Window;

   function Is_Null (Ptr : in Tk_Window) return Boolean;

   type Tk_3DBorder_Rec is private;
   type Tk_3DBorder is access all Tk_3DBorder_Rec;
   pragma Convention (C, Tk_3DBorder);

   Null_Tk_3DBorder : constant Tk_3DBorder;

   function Is_Null (Ptr : in Tk_3DBorder) return Boolean;

   --
   --  Additional types exported to clients.
   --

   subtype Tk_Uid is C.Strings.chars_ptr;

   --
   --  The enum below defines the valid types for Tk configuration options
   --  as implemented by Tk_InitOptions, Tk_SetOptions, etc.
   --

   type Tk_OptionType is (
      TK_OPTION_BOOLEAN,
      TK_OPTION_INT,
      TK_OPTION_DOUBLE,
      TK_OPTION_STRING,
      TK_OPTION_STRING_TABLE,
      TK_OPTION_COLOR,
      TK_OPTION_FONT,
      TK_OPTION_BITMAP,
      TK_OPTION_BORDER,
      TK_OPTION_RELIEF,
      TK_OPTION_CURSOR,
      TK_OPTION_JUSTIFY,
      TK_OPTION_ANCHOR,
      TK_OPTION_SYNONYM,
      TK_OPTION_PIXELS,
      TK_OPTION_WINDOW,
      TK_OPTION_END);
   for Tk_OptionType'Size use 32;

   --
   --  Structures of the following type are used by widgets to specify
   --  their configuration options.  Typically each widget has a static
   --  array of these structures, where each element of the array describes
   --  a single configuration option.  The array is passed to
   --  Tk_CreateOptionTable.
   --

   type Tk_OptionSpec_Rec is private;
   type Tk_OptionSpec is access all Tk_OptionSpec_Rec;
   pragma Convention (C, Tk_OptionSpec);

   Null_Tk_OptionSpec : constant Tk_OptionSpec;

   function Is_Null (Ptr : in Tk_OptionSpec) return Boolean;

   --
   --  Flag values for Tk_OptionSpec structures.  These flags are shared by
   --  Tk_ConfigSpec structures, so be sure to coordinate any changes
   --  carefully.
   --

   TK_OPTION_NULL_OK          : constant := 1;
   TK_OPTION_DONT_SET_DEFAULT : constant := 8;
   --
   --  Macro to use to fill in "offset" fields of the Tk_OptionSpec.
   --  struct.  Computes number of bytes from beginning of structure
   --  to a given field.
   --

   --  Tcl_Offset is not implemented because it's implementation
   --  depends on some C tricks to get offset of a data field.

   --
   --  The following two structures are used for error handling.  When
   --  configuration options are being modified, the old values are
   --  saved in a Tk_SavedOptions structure.  If an error occurs, then the
   --  contents of the structure can be used to restore all of the old
   --  values.  The contents of this structure are for the private use
   --  Tk.  No-one outside Tk should ever read or write any of the fields
   --  of these structures.
   --

   type Tk_SavedOption_Rec is private;
   type Tk_SavedOption is access all Tk_SavedOption_Rec;
   pragma Convention (C, Tk_SavedOption);

   Null_Tk_SavedOption : constant Tk_SavedOption;

   subtype CNatural is C.int range 0 .. C.int'Last;

   type Tk_SavedOption_Array is
     array (CNatural range <>) of aliased Tk_SavedOption;
   pragma Convention (C, Tk_SavedOption_Array);

   function Is_Null (Ptr : in Tk_SavedOption) return Boolean;

   TK_NUM_SAVED_OPTIONS : constant := 20;
   type Tk_SavedOptions_Rec is private;
   type Tk_SavedOptions is access all Tk_SavedOptions_Rec;
   pragma Convention (C, Tk_SavedOptions);

   Null_Tk_SavedOptions : constant Tk_SavedOptions;

   function Is_Null (Ptr : in Tk_SavedOptions) return Boolean;

   --
   --  Structure used to describe application-specific configuration
   --  options:  indicates procedures to call to parse an option and
   --  to return a text string describing an option. THESE ARE
   --  DEPRECATED; PLEASE USE THE NEW STRUCTURES LISTED ABOVE.
   --

   --
   --  This is a temporary flag used while tkObjConfig and new widgets
   --  are in development.
   --

   type Tk_OptionParseProc is access function
     (data    : in ClientData;
      interp  : in Tcl_Interp;
      tkwin   : in Tk_Window;
      value   : in C.Strings.chars_ptr;
      widgRec : in C.Strings.chars_ptr;
      offset  : in C.int)
   return       C.int;
   pragma Convention (C, Tk_OptionParseProc);

   type Tk_OptionPrintProc is access function
     (data        : in ClientData;
      tkwin       : in Tk_Window;
      widgRec     : in C.Strings.chars_ptr;
      offset      : in C.int;
      freeProcPtr : in Tcl_FreeProc)
   return           C.Strings.chars_ptr;
   pragma Convention (C, Tk_OptionPrintProc);

   type Tk_CustomOption_Rec is private;
   type Tk_CustomOption is access all Tk_CustomOption_Rec;
   pragma Convention (C, Tk_CustomOption);

   Null_Tk_CustomOption : constant Tk_CustomOption;

   function Is_Null (Ptr : in Tk_CustomOption) return Boolean;

   --
   --  Structure used to specify information for Tk_ConfigureWidget.  Each
   --  structure gives complete information for one option, including
   --  how the option is specified on the command line, where it appears
   --  in the option database, etc.
   --

   type Tk_ConfigSpec_Rec is private;
   type Tk_ConfigSpec is access all Tk_ConfigSpec_Rec;
   pragma Convention (C, Tk_ConfigSpec);

   Null_Tk_ConfigSpec : constant Tk_ConfigSpec;

   function Is_Null (Ptr : in Tk_ConfigSpec) return Boolean;

   --
   --  Type values for Tk_ConfigSpec structures.  See the user
   --  documentation for details.
   --

   type Tk_ConfigTypes is (
      TK_CONFIG_BOOLEAN,
      TK_CONFIG_INT,
      TK_CONFIG_DOUBLE,
      TK_CONFIG_STRING,
      TK_CONFIG_UID,
      TK_CONFIG_COLOR,
      TK_CONFIG_FONT,
      TK_CONFIG_BITMAP,
      TK_CONFIG_BORDER,
      TK_CONFIG_RELIEF,
      TK_CONFIG_CURSOR,
      TK_CONFIG_ACTIVE_CURSOR,
      TK_CONFIG_JUSTIFY,
      TK_CONFIG_ANCHOR,
      TK_CONFIG_SYNONYM,
      TK_CONFIG_CAP_STYLE,
      TK_CONFIG_JOIN_STYLE,
      TK_CONFIG_PIXELS,
      TK_CONFIG_MM,
      TK_CONFIG_WINDOW,
      TK_CONFIG_CUSTOM,
      TK_CONFIG_END);
   for Tk_ConfigTypes'Size use 32;

   --
   --  Possible values for flags argument to Tk_ConfigureWidget:
   --

   TK_CONFIG_ARGV_ONLY : constant := 1;
   TK_CONFIG_OBJS      : constant := 128;
   --
   --  Possible flag values for Tk_ConfigSpec structures.  Any bits at
   --  or above TK_CONFIG_USER_BIT may be used by clients for selecting
   --  certain entries.  Before changing any values here, coordinate with
   --  tkOldConfig.c {internal-use-only flags are defined there}.
   --

   TK_CONFIG_NULL_OK          : constant := 1;
   TK_CONFIG_COLOR_ONLY       : constant := 2;
   TK_CONFIG_MONO_ONLY        : constant := 4;
   TK_CONFIG_DONT_SET_DEFAULT : constant := 8;
   TK_CONFIG_OPTION_SPECIFIED : constant := 16;
   TK_CONFIG_USER_BIT         : constant := 256;
   --  __NO_OLD_CONFIG

   --
   --  Structure used to specify how to handle argv options.
   --

   type Tk_ArgvInfo_Rec is private;
   type Tk_ArgvInfo is access all Tk_ArgvInfo_Rec;
   pragma Convention (C, Tk_ArgvInfo);

   Null_Tk_ArgvInfo : constant Tk_ArgvInfo;

   function Is_Null (Ptr : in Tk_ArgvInfo) return Boolean;

   --
   --  Legal values for the type field of a Tk_ArgvInfo: see the user
   --  documentation for details.
   --

   TK_ARGV_CONSTANT          : constant := 15;
   TK_ARGV_INT               : constant := 16;
   TK_ARGV_STRING            : constant := 17;
   TK_ARGV_UID               : constant := 18;
   TK_ARGV_REST              : constant := 19;
   TK_ARGV_FLOAT             : constant := 20;
   TK_ARGV_FUNC              : constant := 21;
   TK_ARGV_GENFUNC           : constant := 22;
   TK_ARGV_HELP              : constant := 23;
   TK_ARGV_CONST_OPTION      : constant := 24;
   TK_ARGV_OPTION_VALUE      : constant := 25;
   TK_ARGV_OPTION_NAME_VALUE : constant := 26;
   TK_ARGV_END               : constant := 27;
   --
   --  Flag bits for passing to Tk_ParseArgv:
   --

   TK_ARGV_NO_DEFAULTS         : constant := 1;
   TK_ARGV_NO_LEFTOVERS        : constant := 2;
   TK_ARGV_NO_ABBREV           : constant := 4;
   TK_ARGV_DONT_SKIP_FIRST_ARG : constant := 8;
   --
   --  Enumerated type for describing actions to be taken in response
   --  to a restrictProc established by Tk_RestrictEvents.
   --

   type Tk_RestrictAction is (
      TK_DEFER_EVENT,
      TK_PROCESS_EVENT,
      TK_DISCARD_EVENT);
   for Tk_RestrictAction'Size use 32;

   --
   --  Priority levels to pass to Tk_AddOption:
   --

   TK_WIDGET_DEFAULT_PRIO : constant := 20;
   TK_STARTUP_FILE_PRIO   : constant := 40;
   TK_USER_DEFAULT_PRIO   : constant := 60;
   TK_INTERACTIVE_PRIO    : constant := 80;
   TK_MAX_PRIO            : constant := 100;
   --
   --  Relief values returned by Tk_GetRelief:
   --

   TK_RELIEF_FLAT   : constant := 0;
   TK_RELIEF_GROOVE : constant := 1;
   TK_RELIEF_RAISED : constant := 2;
   TK_RELIEF_RIDGE  : constant := 3;
   TK_RELIEF_SOLID  : constant := 4;
   TK_RELIEF_SUNKEN : constant := 5;
   --
   --  "Which" argument values for Tk_3DBorderGC:
   --

   TK_3D_FLAT_GC  : constant := 1;
   TK_3D_LIGHT_GC : constant := 2;
   TK_3D_DARK_GC  : constant := 3;
   --
   --  Special EnterNotify/LeaveNotify "mode" for use in events
   --  generated by tkShare.c.  Pick a high enough value that it's
   --  unlikely to conflict with existing values {like NotifyNormal}
   --  or any new values defined in the future.
   --

   TK_NOTIFY_SHARE : constant := 20;
   --
   --  Enumerated type for describing a point by which to anchor something:
   --

   type Tk_Anchor is (
      TK_ANCHOR_N,
      TK_ANCHOR_NE,
      TK_ANCHOR_E,
      TK_ANCHOR_SE,
      TK_ANCHOR_S,
      TK_ANCHOR_SW,
      TK_ANCHOR_W,
      TK_ANCHOR_NW,
      TK_ANCHOR_CENTER);
   for Tk_Anchor'Size use 32;

   --
   --  Enumerated type for describing a style of justification:
   --

   type Tk_Justify is (TK_JUSTIFY_LEFT, TK_JUSTIFY_RIGHT, TK_JUSTIFY_CENTER);
   for Tk_Justify'Size use 32;

   --
   --  The following structure is used by Tk_GetFontMetrics {} to return
   --  information about the properties of a Tk_Font.
   --

   type Tk_FontMetrics_Rec is private;
   type Tk_FontMetrics is access all Tk_FontMetrics_Rec;
   pragma Convention (C, Tk_FontMetrics);

   Null_Tk_FontMetrics : constant Tk_FontMetrics;

   function Is_Null (Ptr : in Tk_FontMetrics) return Boolean;

   --
   --  Flags passed to Tk_MeasureChars:
   --

   TK_WHOLE_WORDS  : constant := 1;
   TK_AT_LEAST_ONE : constant := 2;
   TK_PARTIAL_OK   : constant := 4;
   --
   --  Flags passed to Tk_ComputeTextLayout:
   --

   TK_IGNORE_TABS     : constant := 8;
   TK_IGNORE_NEWLINES : constant := 16;
   --
   --  Each geometry manager {the packer, the placer, etc.} is represented
   --  by a structure of the following form, which indicates procedures
   --  to invoke in the geometry manager to carry out certain functions.
   --

   type Tk_GeomRequestProc is access procedure
  (data  : in ClientData;
   tkwin : in Tk_Window);
   pragma Convention (C, Tk_GeomRequestProc);

   type Tk_GeomLostSlaveProc is access procedure
  (data  : in ClientData;
   tkwin : in Tk_Window);
   pragma Convention (C, Tk_GeomLostSlaveProc);

   type Tk_GeomMgr_Rec is private;
   type Tk_GeomMgr is access all Tk_GeomMgr_Rec;
   pragma Convention (C, Tk_GeomMgr);

   Null_Tk_GeomMgr : constant Tk_GeomMgr;

   function Is_Null (Ptr : in Tk_GeomMgr) return Boolean;

   --
   --  Result values returned by Tk_GetScrollInfo:
   --

   TK_SCROLL_MOVETO : constant := 1;
   TK_SCROLL_PAGES  : constant := 2;
   TK_SCROLL_UNITS  : constant := 3;
   TK_SCROLL_ERROR  : constant := 4;
   --
   -- -------------------------------------------------------------------------
   ----
   --
   --  Extensions to the X event set
   --
   -- -------------------------------------------------------------------------
   ----
   --

   MouseWheelMask   : constant := 268435456;
   ActivateMask     : constant := 536870912;
   VirtualEventMask : constant := 1073741824;
   --
   --  A virtual event shares most of its fields with the XKeyEvent and
   --  XButtonEvent structures.  99% of the time a virtual event will be
   --  an abstraction of a key or button event, so this structure provides
   --  the most information to the user.  The only difference is the changing
   --  of the detail field for a virtual event so that it holds the name of the
   --  virtual event being triggered.
   --

   type XVirtualEvent_Rec is private;
   type XVirtualEvent is access all XVirtualEvent_Rec;
   pragma Convention (C, XVirtualEvent);

   Null_XVirtualEvent : constant XVirtualEvent;

   function Is_Null (Ptr : in XVirtualEvent) return Boolean;

   type XActivateDeactivateEvent_Rec is private;
   type XActivateDeactivateEvent is access all XActivateDeactivateEvent_Rec;
   pragma Convention (C, XActivateDeactivateEvent);

   Null_XActivateDeactivateEvent : constant XActivateDeactivateEvent;

   function Is_Null (Ptr : in XActivateDeactivateEvent) return Boolean;

   subtype XActivateEvent is XActivateDeactivateEvent;

   subtype XDeactivateEvent is XActivateDeactivateEvent;

   --
   -- --------------------------------------------------------------
   --
   --  Macros for querying Tk_Window structures.  See the
   --  manual entries for documentation.
   --
   -- --------------------------------------------------------------
   --

   function Tk_ScreenNumber (tkwin : in Tk_Window) return C.int;
   pragma Import (C, Tk_ScreenNumber, "Tk_CallScreenNumber");

   function Tk_Depth (tkwin : in Tk_Window) return C.int;
   pragma Import (C, Tk_Depth, "Tk_CallDepth");

   function Tk_PathName (tkwin : in Tk_Window) return C.Strings.chars_ptr;
   pragma Import (C, Tk_PathName, "Tk_CallPathName");

   function Tk_Name (tkwin : in Tk_Window) return Tk_Uid;
   pragma Import (C, Tk_Name, "Tk_CallName");

   function Tk_Class (tkwin : in Tk_Window) return Tk_Uid;
   pragma Import (C, Tk_Class, "Tk_CallClass");

   function Tk_Parent (tkwin : in Tk_Window) return Tk_Window;
   pragma Import (C, Tk_Parent, "Tk_CallParent");

   --
   --  The structure below is needed by the macros above so that they can
   --  access the fields of a Tk_Window.  The fields not needed by the macros
   --  are declared as "dummyX".  The structure has its own type in order to
   --  prevent applications from accessing Tk_Window fields except using
   --  official macros.  WARNING!! The structure definition must be kept
   --  consistent with the TkWindow structure in tkInt.h.  If you change one,
   --  then change the other.  See the declaration in tkInt.h for
   --  documentation on what the fields are used for internally.
   --

   type Tk_FakeWin_Rec is private;
   type Tk_FakeWin is access all Tk_FakeWin_Rec;
   pragma Convention (C, Tk_FakeWin);

   Null_Tk_FakeWin : constant Tk_FakeWin;

   function Is_Null (Ptr : in Tk_FakeWin) return Boolean;

   --
   --  Flag values for TkWindow {and Tk_FakeWin} structures are:
   --
   --  TK_MAPPED:                       1 means window is currently mapped,
   --                           0 means unmapped.
   --  TK_TOP_LEVEL:            1 means this is a top-level window {it
   --                           was or will be created as a child of
   --                           a root window}.
   --  TK_ALREADY_DEAD:         1 means the window is in the process of
   --                           being destroyed already.
   --  TK_NEED_CONFIG_NOTIFY:   1 means that the window has been reconfigured
   --                           before it was made to exist.  At the time of
   --                           making it exist a ConfigureNotify event needs
   --                           to be generated.
   --  TK_GRAB_FLAG:            Used to manage grabs.  See tkGrab.c for
   --                           details.
   --  TK_CHECKED_IC:           1 means we've already tried to get an input
   --                           context for this window;  if the ic field
   --                           is NULL it means that there isn't a context
   --                           for the field.
   --  TK_DONT_DESTROY_WINDOW:  1 means that Tk_DestroyWindow should not
   --                           invoke XDestroyWindow to destroy this widget's
   --                           X window.  The flag is set when the window
   --                           has already been destroyed elsewhere {e.g.
   --                           by another application} or when it will be
   --                           destroyed later {e.g. by destroying its
   --                           parent}.
   --  TK_WM_COLORMAP_WINDOW:   1 means that this window has at some time
   --                           appeared in the WM_COLORMAP_WINDOWS property
   --                           for its toplevel, so we have to remove it
   --                           from that property if the window is
   --                           deleted and the toplevel isn't.
   --  TK_EMBEDDED:                     1 means that this window {which must
   --  be a
   --                           toplevel} is not a free-standing window but
   --                           rather is embedded in some other application.
   --  TK_CONTAINER:            1 means that this window is a container, and
   --                           that some other application {either in
   --                           this process or elsewhere} may be
   --                           embedding itself inside the window.
   --  TK_BOTH_HALVES:          1 means that this window is used for
   --                           application embedding {either as
   --                           container or embedded application}, and
   --                           both the containing and embedded halves
   --                           are associated with windows in this
   --                           particular process.
   --  TK_DEFER_MODAL:          1 means that this window has deferred a modal
   --                           loop until all of the bindings for the current
   --                           event have been invoked.
   --  TK_WRAPPER:                      1 means that this window is the extra
   --                           wrapper window created around a toplevel
   --                           to hold the menubar under Unix.  See
   --                           tkUnixWm.c for more information.
   --  TK_REPARENTED:           1 means that this window has been reparented
   --                           so that as far as the window system is
   --                           concerned it isn't a child of its Tk
   --                           parent.  Initially this is used only for
   --                           special Unix menubar windows.
   --

   TK_MAPPED              : constant := 1;
   TK_TOP_LEVEL           : constant := 2;
   TK_ALREADY_DEAD        : constant := 4;
   TK_NEED_CONFIG_NOTIFY  : constant := 8;
   TK_GRAB_FLAG           : constant := 16;
   TK_CHECKED_IC          : constant := 32;
   TK_DONT_DESTROY_WINDOW : constant := 64;
   TK_WM_COLORMAP_WINDOW  : constant := 128;
   TK_EMBEDDED            : constant := 256;
   TK_CONTAINER           : constant := 512;
   TK_BOTH_HALVES         : constant := 1024;
   TK_DEFER_MODAL         : constant := 2048;
   TK_WRAPPER             : constant := 4096;
   TK_REPARENTED          : constant := 8192;
   --
   -- --------------------------------------------------------------
   --
   --  Procedure prototypes and structures used for defining new canvas
   --  items:
   --
   -- --------------------------------------------------------------
   --

   type Tk_State is (
      TK_STATE_NULL,
      TK_STATE_ACTIVE,
      TK_STATE_DISABLED,
      TK_STATE_NORMAL,
      TK_STATE_HIDDEN);
   for Tk_State use
     (TK_STATE_NULL     => -1,
      TK_STATE_ACTIVE   => 0,
      TK_STATE_DISABLED => 1,
      TK_STATE_NORMAL   => 2,
      TK_STATE_HIDDEN   => 3);
   for Tk_State'Size use 32;

   type Tk_SmoothMethod_Rec is private;
   type Tk_SmoothMethod is access all Tk_SmoothMethod_Rec;
   pragma Convention (C, Tk_SmoothMethod);

   Null_Tk_SmoothMethod : constant Tk_SmoothMethod;

   function Is_Null (Ptr : in Tk_SmoothMethod) return Boolean;

   --
   --  For each item in a canvas widget there exists one record with
   --  the following structure.  Each actual item is represented by
   --  a record with the following stuff at its beginning, plus additional
   --  type-specific stuff after that.
   --

   TK_TAG_SPACE : constant := 3;
   type Tk_Item_Rec is private;
   type Tk_Item is access all Tk_Item_Rec;
   pragma Convention (C, Tk_Item);

   Null_Tk_Item : constant Tk_Item;

   function Is_Null (Ptr : in Tk_Item) return Boolean;

   --
   --  Flag bits for canvases {redraw_flags}:
   --
   --  TK_ITEM_STATE_DEPENDANT -        1 means that object needs to be
   --                           redrawn if the canvas state changes.
   --  TK_ITEM_DONT_REDRAW -    1 means that the object redraw is already
   --                           been prepared, so the general canvas code
   --                           doesn't need to do that any more.
   --

   TK_ITEM_STATE_DEPENDANT : constant := 1;
   TK_ITEM_DONT_REDRAW     : constant := 2;
   --
   --  Records of the following type are used to describe a type of
   --  item {e.g.  lines, circles, etc.} that can form part of a
   --  canvas widget.
   --

   type Tk_ItemCreateProc is access function
     (interp  : in Tcl_Interp;
      canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      argc    : in C.int;
      objv    : in Tcl_Obj_Array)
   return       C.int;
   pragma Convention (C, Tk_ItemCreateProc);

   type Tk_ItemConfigureProc is access function
     (interp  : in Tcl_Interp;
      canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      argc    : in C.int;
      objv    : in Tcl_Obj_Array;
      flags   : in C.int)
   return       C.int;
   pragma Convention (C, Tk_ItemConfigureProc);

   type Tk_ItemCoordProc is access function
     (interp  : in Tcl_Interp;
      canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      argc    : in C.int;
      argv    : in Tcl_Obj)
   return       C.int;
   pragma Convention (C, Tk_ItemCoordProc);

   type Tk_ItemDeleteProc is access procedure
     (canvas  : in Tk_Canvas;
      itemPtr : access Tk_Item;
      display : in System.Address);  --  @todo should be Display* (Xlib)

   type Tk_ItemDisplayProc is access procedure
     (canvas  : in Tk_Canvas;
      itemPtr : access Tk_Item;
      display : in System.Address;   --  @todo should be Display* (Xlib)
      dst     : in C.int;            --  @todo should be Drawable (Xlib)
      x, y, width, height : in C.int);

   type Tk_ItemPointProc is access function
     (canvas   : in Tk_Canvas;
      itemPtr  : in Tk_Item;
      pointPtr : access C.double)
   return        C.double;
   pragma Convention (C, Tk_ItemPointProc);

   type Tk_ItemAreaProc is access function
     (canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      rectPtr : access C.double)
   return       C.int;
   pragma Convention (C, Tk_ItemAreaProc);

   type Tk_ItemPostscriptProc is access function
     (interp  : in Tcl_Interp;
      canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      prepass : in C.int)
   return       C.int;
   pragma Convention (C, Tk_ItemPostscriptProc);

   type Tk_ItemScaleProc is access procedure
     (canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      originX : in C.double;
      originY : in C.double;
      scaleX  : in C.double;
      scaleY  : in C.double);
   pragma Convention (C, Tk_ItemScaleProc);

   type Tk_ItemTranslateProc is access procedure
     (canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      deltaX  : in C.double;
      deltaY  : in C.double);
   pragma Convention (C, Tk_ItemTranslateProc);

   type Tk_ItemIndexProc is access function
     (interp      : in Tcl_Interp;
      canvas      : in Tk_Canvas;
      itemPtr     : in Tk_Item;
      indexString : in C.Strings.chars_ptr;
      indexPtr    : access C.int)
   return           C.int;
   pragma Convention (C, Tk_ItemIndexProc);

   type Tk_ItemCursorProc is access procedure
     (canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      index   : in C.int);
   pragma Convention (C, Tk_ItemCursorProc);

   type Tk_ItemSelectionProc is access function
     (canvas   : in Tk_Canvas;
      itemPtr  : in Tk_Item;
      offset   : in C.int;
      buffer   : in C.Strings.chars_ptr;
      maxBytes : in C.int)
   return        C.int;
   pragma Convention (C, Tk_ItemSelectionProc);

   type Tk_ItemInsertProc is access procedure
     (canvas     : in Tk_Canvas;
      itemPtr    : in Tk_Item;
      beforeThis : in C.int;
      strng      : in C.Strings.chars_ptr);
   pragma Convention (C, Tk_ItemInsertProc);

   type Tk_ItemDCharsProc is access procedure
     (canvas  : in Tk_Canvas;
      itemPtr : in Tk_Item;
      first   : in C.int;
      last    : in C.int);
   pragma Convention (C, Tk_ItemDCharsProc);

   type Tk_ItemType_Rec is private;
   type Tk_ItemType is access all Tk_ItemType_Rec;
   pragma Convention (C, Tk_ItemType);

   Null_Tk_ItemType : constant Tk_ItemType;

   function Is_Null (Ptr : in Tk_ItemType) return Boolean;

   --
   --  The following structure provides information about the selection and
   --  the insertion cursor.  It is needed by only a few items, such as
   --  those that display text.  It is shared by the generic canvas code
   --  and the item-specific code, but most of the fields should be written
   --  only by the canvas generic code.
   --

   type Tk_CanvasTextInfo_Rec is private;
   type Tk_CanvasTextInfo is access all Tk_CanvasTextInfo_Rec;
   pragma Convention (C, Tk_CanvasTextInfo);

   Null_Tk_CanvasTextInfo : constant Tk_CanvasTextInfo;

   function Is_Null (Ptr : in Tk_CanvasTextInfo) return Boolean;

   --
   --  Structures used for Dashing and Outline.
   --

   type Tk_Dash_Rec is private;
   type Tk_Dash is access all Tk_Dash_Rec;
   pragma Convention (C, Tk_Dash);

   Null_Tk_Dash : constant Tk_Dash;

   function Is_Null (Ptr : in Tk_Dash) return Boolean;

   type Tk_TSOffset_Rec is private;
   type Tk_TSOffset is access all Tk_TSOffset_Rec;
   pragma Convention (C, Tk_TSOffset);

   Null_Tk_TSOffset : constant Tk_TSOffset;

   function Is_Null (Ptr : in Tk_TSOffset) return Boolean;

   --
   --  Bit fields in Tk_Offset->flags:
   --

   TK_OFFSET_INDEX    : constant := 1;
   TK_OFFSET_RELATIVE : constant := 2;
   TK_OFFSET_LEFT     : constant := 4;
   TK_OFFSET_CENTER   : constant := 8;
   TK_OFFSET_RIGHT    : constant := 16;
   TK_OFFSET_TOP      : constant := 32;
   TK_OFFSET_MIDDLE   : constant := 64;
   TK_OFFSET_BOTTOM   : constant := 128;
   type Tk_Outline_Rec is private;
   type Tk_Outline is access all Tk_Outline_Rec;
   pragma Convention (C, Tk_Outline);

   Null_Tk_Outline : constant Tk_Outline;

   function Is_Null (Ptr : in Tk_Outline) return Boolean;

   --
   -- --------------------------------------------------------------
   --
   --  Procedure prototypes and structures used for managing images:
   --
   -- --------------------------------------------------------------
   --

   type Tk_ImageType_Rec is private;
   type Tk_ImageType is access all Tk_ImageType_Rec;
   pragma Convention (C, Tk_ImageType);

   Null_Tk_ImageType : constant Tk_ImageType;

   function Is_Null (Ptr : in Tk_ImageType) return Boolean;

   type Tk_ImageCreateProc is access function
     (interp        : in Tcl_Interp;
      name          : in C.Strings.chars_ptr;
      objc          : in C.int;
      objv          : in Tcl_Obj_Array;
      typePtr       : in Tk_ImageType;
      master        : in Tk_ImageMaster;
      masterdataptr : in ClientData)
   return             C.int;
   pragma Convention (C, Tk_ImageCreateProc);

   type Tk_ImageGetProc is access function
     (tkwin      : in Tk_Window;
      masterdata : in ClientData)
   return          ClientData;
   pragma Convention (C, Tk_ImageGetProc);

   type Tk_ImageDeleteProc is access procedure (masterdata : in ClientData);
   pragma Convention (C, Tk_ImageDeleteProc);

   type Tk_ImageChangedProc is access procedure
     (data        : in ClientData;
      x           : in C.int;
      y           : in C.int;
      width       : in C.int;
      height      : in C.int;
      imageWidth  : in C.int;
      imageHeight : in C.int);
   pragma Convention (C, Tk_ImageChangedProc);

   type Tk_ImagePostscriptProc is access function
     (data    : in ClientData;
      interp  : in Tcl_Interp;
      tkwin   : in Tk_Window;
      psinfo  : in Tk_PostscriptInfo;
      x       : in C.int;
      y       : in C.int;
      width   : in C.int;
      height  : in C.int;
      prepass : in C.int)
   return       C.int;
   pragma Convention (C, Tk_ImagePostscriptProc);

   --
   --  The following structure represents a particular type of image
   --  {bitmap, xpm image, etc.}.  It provides information common to
   --  all images of that type, such as the type name and a collection
   --  of procedures in the image manager that respond to various
   --  events.  Each image manager is represented by one of these
   --  structures.
   --

   --
   -- --------------------------------------------------------------
   --
   --  Additional definitions used to manage images of type "photo".
   --
   -- --------------------------------------------------------------
   --

   --
   --  The following type is used to identify a particular photo image
   --  to be manipulated:
   --

   type Tk_PhotoHandle_Rec is private;
   type Tk_PhotoHandle is access all Tk_PhotoHandle_Rec;
   pragma Convention (C, Tk_PhotoHandle);

   Null_Tk_PhotoHandle : constant Tk_PhotoHandle;

   function Is_Null (Ptr : in Tk_PhotoHandle) return Boolean;

   --
   --  The following structure describes a block of pixels in memory:
   --

   type Tk_PhotoImageBlock_Rec is private;
   type Tk_PhotoImageBlock is access all Tk_PhotoImageBlock_Rec;
   pragma Convention (C, Tk_PhotoImageBlock);

   Null_Tk_PhotoImageBlock : constant Tk_PhotoImageBlock;

   function Is_Null (Ptr : in Tk_PhotoImageBlock) return Boolean;

   --
   --  Procedure prototypes and structures used in reading and
   --  writing photo images:
   --

   type Tk_PhotoImageFormat_Rec is private;
   type Tk_PhotoImageFormat is access all Tk_PhotoImageFormat_Rec;
   pragma Convention (C, Tk_PhotoImageFormat);

   Null_Tk_PhotoImageFormat : constant Tk_PhotoImageFormat;

   function Is_Null (Ptr : in Tk_PhotoImageFormat) return Boolean;

   type Tk_ImageFileMatchProc is access function
     (chan      : in Tcl_Channel;
      fileName  : in C.Strings.chars_ptr;
      format    : in Tcl_Obj;
      widthPtr  : access C.int;
      heightPtr : access C.int;
      interp    : in Tcl_Interp)
   return         C.int;
   pragma Convention (C, Tk_ImageFileMatchProc);

   type Tk_ImageStringMatchProc is access function
     (dataObj   : in Tcl_Obj;
      format    : in Tcl_Obj;
      widthPtr  : access C.int;
      heightPtr : access C.int;
      interp    : in Tcl_Interp)
   return         C.int;
   pragma Convention (C, Tk_ImageStringMatchProc);

   type Tk_ImageFileReadProc is access function
     (interp      : in Tcl_Interp;
      chan        : in Tcl_Channel;
      fileName    : in C.Strings.chars_ptr;
      format      : in Tcl_Obj;
      imageHandle : in Tk_PhotoHandle;
      destX       : in C.int;
      destY       : in C.int;
      width       : in C.int;
      height      : in C.int;
      srcX        : in C.int;
      srcY        : in C.int)
   return           C.int;
   pragma Convention (C, Tk_ImageFileReadProc);

   type Tk_ImageStringReadProc is access function
     (interp      : in Tcl_Interp;
      dataObj     : in Tcl_Obj;
      format      : in Tcl_Obj;
      imageHandle : in Tk_PhotoHandle;
      destX       : in C.int;
      destY       : in C.int;
      width       : in C.int;
      height      : in C.int;
      srcX        : in C.int;
      srcY        : in C.int)
   return           C.int;
   pragma Convention (C, Tk_ImageStringReadProc);

   type Tk_ImageFileWriteProc is access function
     (interp   : in Tcl_Interp;
      fileName : in C.Strings.chars_ptr;
      format   : in Tcl_Obj;
      blockPtr : in Tk_PhotoImageBlock)
   return        C.int;
   pragma Convention (C, Tk_ImageFileWriteProc);

   type Tk_ImageStringWriteProc is access function
     (interp   : in Tcl_Interp;
      format   : in Tcl_Obj;
      blockPtr : in Tk_PhotoImageBlock)
   return        C.int;
   pragma Convention (C, Tk_ImageStringWriteProc);

   --
   --  The following structure represents a particular file format for
   --  storing images {e.g., PPM, GIF, JPEG, etc.}.  It provides information
   --  to allow image files of that format to be recognized and read into
   --  a photo image.
   --

   procedure Tk_CreateOldImageType (typePtr : in Tk_ImageType);
   pragma Import (C, Tk_CreateOldImageType, "Tk_CreateOldImageType");

   procedure Tk_CreateOldPhotoImageFormat
     (formatPtr : in Tk_PhotoImageFormat);
   pragma Import
     (C,
      Tk_CreateOldPhotoImageFormat,
      "Tk_CreateOldPhotoImageFormat");

   --
   -- --------------------------------------------------------------
   --
   --  The definitions below provide backward compatibility for
   --  functions and types related to event handling that used to
   --  be in Tk but have moved to Tcl.
   --
   -- --------------------------------------------------------------
   --

   TK_READABLE      : constant := TCL_READABLE;
   TK_WRITABLE      : constant := TCL_WRITABLE;
   TK_EXCEPTION     : constant := TCL_EXCEPTION;
   TK_DONT_WAIT     : constant := TCL_DONT_WAIT;
   TK_X_EVENTS      : constant := TCL_WINDOW_EVENTS;
   TK_WINDOW_EVENTS : constant := TCL_WINDOW_EVENTS;
   TK_FILE_EVENTS   : constant := TCL_FILE_EVENTS;
   TK_TIMER_EVENTS  : constant := TCL_TIMER_EVENTS;
   TK_IDLE_EVENTS   : constant := TCL_IDLE_EVENTS;
   TK_ALL_EVENTS    : constant := TCL_ALL_EVENTS;
   --  Additional stuff that has moved to Tcl:

   procedure Tk_Main
     (argc : in C.int;
      argv : in CArgv.Chars_Ptr_Ptr;
      proc : in Tcl_AppInitProc);
   pragma Import (C, Tk_Main, "Tk_CallMain");

   procedure Tk_InitImageArgs
     (interp : in Tcl_Interp;
      argc   : in C.int;
      argv   : in CArgv.Chars_Ptr_Ptr);
   pragma Import (C, Tk_InitImageArgs, "Tk_InitImageArgs");

   --
   -- --------------------------------------------------------------
   --
   --  Additional procedure types defined by Tk.
   --
   -- --------------------------------------------------------------
   --

   type Tk_GetSelProc is access function
     (data    : in ClientData;
      interp  : in Tcl_Interp;
      portion : in C.Strings.chars_ptr)
   return       C.int;
   pragma Convention (C, Tk_GetSelProc);

   type Tk_LostSelProc is access procedure (data : in ClientData);
   pragma Convention (C, Tk_LostSelProc);

   type Tk_SelectionProc is access function
     (data     : in ClientData;
      offset   : in C.int;
      buffer   : in C.Strings.chars_ptr;
      maxBytes : in C.int)
   return        C.int;
   pragma Convention (C, Tk_SelectionProc);

   --
   -- --------------------------------------------------------------
   --
   --  Exported procedures and variables.
   --
   -- --------------------------------------------------------------
   --

   --
   --  tkDecls.h --
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
   --  in the generic/tk.decls script.
   --

   --  !BEGIN!: Do not edit below this line.

   --
   --  Exported function declarations:
   --

   --  0

   procedure Tk_MainLoop;
   pragma Import (C, Tk_MainLoop, "Tk_MainLoop");

   --  1

   --  2

   --  3

   --  4

   --  5

   procedure Tk_AddOption
     (tkwin    : in Tk_Window;
      name     : in C.Strings.chars_ptr;
      value    : in C.Strings.chars_ptr;
      priority : in C.int);
   pragma Import (C, Tk_AddOption, "Tk_AddOption");

   --  6

   --  7

   procedure Tk_CanvasDrawableCoords
     (canvas       : in Tk_Canvas;
      x            : in C.double;
      y            : in C.double;
      drawableXPtr : access C.short;
      drawableYPtr : access C.short);
   pragma Import (C, Tk_CanvasDrawableCoords, "Tk_CanvasDrawableCoords");

   --  8

   procedure Tk_CanvasEventuallyRedraw
     (canvas : in Tk_Canvas;
      x1     : in C.int;
      y1     : in C.int;
      x2     : in C.int;
      y2     : in C.int);
   pragma Import (C, Tk_CanvasEventuallyRedraw, "Tk_CanvasEventuallyRedraw");

   --  9

   function Tk_CanvasGetCoord
     (interp    : in Tcl_Interp;
      canvas    : in Tk_Canvas;
      str       : in C.Strings.chars_ptr;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_CanvasGetCoord, "Tk_CanvasGetCoord");

   --  10

   function Tk_CanvasGetTextInfo
     (canvas : in Tk_Canvas)
      return   Tk_CanvasTextInfo;
   pragma Import (C, Tk_CanvasGetTextInfo, "Tk_CanvasGetTextInfo");

   --  11

   --  12

   --  13

   function Tk_CanvasPsFont
     (interp : in Tcl_Interp;
      canvas : in Tk_Canvas;
      font   : in Tk_Font)
      return   C.int;
   pragma Import (C, Tk_CanvasPsFont, "Tk_CanvasPsFont");

   --  14

   procedure Tk_CanvasPsPath
     (interp    : in Tcl_Interp;
      canvas    : in Tk_Canvas;
      coordPtr  : access C.double;
      numPoints : in C.int);
   pragma Import (C, Tk_CanvasPsPath, "Tk_CanvasPsPath");

   --  15

   --  16

   function Tk_CanvasPsY
     (canvas : in Tk_Canvas;
      y      : in C.double)
      return   C.double;
   pragma Import (C, Tk_CanvasPsY, "Tk_CanvasPsY");

   --  17

   --  18

   function Tk_CanvasTagsParseProc
     (data    : in ClientData;
      interp  : in Tcl_Interp;
      tkwin   : in Tk_Window;
      value   : in C.Strings.chars_ptr;
      widgRec : in C.Strings.chars_ptr;
      offset  : in C.int)
      return    C.int;
   pragma Import (C, Tk_CanvasTagsParseProc, "Tk_CanvasTagsParseProc");

   --  19

   function Tk_CanvasTagsPrintProc
     (data        : in ClientData;
      tkwin       : in Tk_Window;
      widgRec     : in C.Strings.chars_ptr;
      offset      : in C.int;
      freeProcPtr : in Tcl_FreeProc)
      return        C.Strings.chars_ptr;
   pragma Import (C, Tk_CanvasTagsPrintProc, "Tk_CanvasTagsPrintProc");

   --  20

   function Tk_CanvasTkwin (canvas : in Tk_Canvas) return Tk_Window;
   pragma Import (C, Tk_CanvasTkwin, "Tk_CanvasTkwin");

   --  21

   procedure Tk_CanvasWindowCoords
     (canvas     : in Tk_Canvas;
      x          : in C.double;
      y          : in C.double;
      screenXPtr : access C.short;
      screenYPtr : access C.short);
   pragma Import (C, Tk_CanvasWindowCoords, "Tk_CanvasWindowCoords");

   --  22

   --  23

   function Tk_CharBbox
     (layout    : in Tk_TextLayout;
      index     : in C.int;
      xPtr      : access C.int;
      yPtr      : access C.int;
      widthPtr  : access C.int;
      heightPtr : access C.int)
      return      C.int;
   pragma Import (C, Tk_CharBbox, "Tk_CharBbox");

   --  24

   --  25

   --  26

   function Tk_ClipboardClear
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window)
      return   C.int;
   pragma Import (C, Tk_ClipboardClear, "Tk_ClipboardClear");

   --  27

   function Tk_ConfigureInfo
     (interp   : in Tcl_Interp;
      tkwin    : in Tk_Window;
      specs    : in Tk_ConfigSpec;
      widgRec  : in C.Strings.chars_ptr;
      argvName : in C.Strings.chars_ptr;
      flags    : in C.int)
      return     C.int;
   pragma Import (C, Tk_ConfigureInfo, "Tk_ConfigureInfo");

   --  28

   function Tk_ConfigureValue
     (interp   : in Tcl_Interp;
      tkwin    : in Tk_Window;
      specs    : in Tk_ConfigSpec;
      widgRec  : in C.Strings.chars_ptr;
      argvName : in C.Strings.chars_ptr;
      flags    : in C.int)
      return     C.int;
   pragma Import (C, Tk_ConfigureValue, "Tk_ConfigureValue");

   --  29

   function Tk_ConfigureWidget
     (interp  : in Tcl_Interp;
      tkwin   : in Tk_Window;
      specs   : in Tk_ConfigSpec;
      argc    : in C.int;
      argv    : in CArgv.Chars_Ptr_Ptr;
      widgRec : in C.Strings.chars_ptr;
      flags   : in C.int)
      return    C.int;
   pragma Import (C, Tk_ConfigureWidget, "Tk_ConfigureWidget");

   --  30

   --  31

   function Tk_ComputeTextLayout
     (font       : in Tk_Font;
      str        : in C.Strings.chars_ptr;
      numChars   : in C.int;
      wrapLength : in C.int;
      justify    : in Tk_Justify;
      flags      : in C.int;
      widthPtr   : access C.int;
      heightPtr  : access C.int)
      return       Tk_TextLayout;
   pragma Import (C, Tk_ComputeTextLayout, "Tk_ComputeTextLayout");

   --  32

   function Tk_CoordsToWindow
     (rootX : in C.int;
      rootY : in C.int;
      tkwin : in Tk_Window)
      return  Tk_Window;
   pragma Import (C, Tk_CoordsToWindow, "Tk_CoordsToWindow");

   --  33

   function Tk_CreateBinding
     (interp       : in Tcl_Interp;
      bindingTable : in Tk_BindingTable;
      object       : in ClientData;
      eventStr     : in C.Strings.chars_ptr;
      command      : in C.Strings.chars_ptr;
      append       : in C.int)
      return         C.unsigned_long;
   pragma Import (C, Tk_CreateBinding, "Tk_CreateBinding");

   --  34

   function Tk_CreateBindingTable
     (interp : in Tcl_Interp)
      return   Tk_BindingTable;
   pragma Import (C, Tk_CreateBindingTable, "Tk_CreateBindingTable");

   --  35

   --  36

   --  37

   --  38

   procedure Tk_CreateImageType (typePtr : in Tk_ImageType);
   pragma Import (C, Tk_CreateImageType, "Tk_CreateImageType");

   --  39

   procedure Tk_CreateItemType (typePtr : in Tk_ItemType);
   pragma Import (C, Tk_CreateItemType, "Tk_CreateItemType");

   --  40

   procedure Tk_CreatePhotoImageFormat (formatPtr : in Tk_PhotoImageFormat);
   pragma Import (C, Tk_CreatePhotoImageFormat, "Tk_CreatePhotoImageFormat");

   --  41

   --  42

   function Tk_CreateWindow
     (interp     : in Tcl_Interp;
      parent     : in Tk_Window;
      name       : in C.Strings.chars_ptr;
      screenName : in C.Strings.chars_ptr)
      return       Tk_Window;
   pragma Import (C, Tk_CreateWindow, "Tk_CreateWindow");

   --  43

   function Tk_CreateWindowFromPath
     (interp     : in Tcl_Interp;
      tkwin      : in Tk_Window;
      pathName   : in C.Strings.chars_ptr;
      screenName : in C.Strings.chars_ptr)
      return       Tk_Window;
   pragma Import (C, Tk_CreateWindowFromPath, "Tk_CreateWindowFromPath");

   --  44

   function Tk_DefineBitmap
     (interp : in Tcl_Interp;
      name   : in C.Strings.chars_ptr;
      source : in C.Strings.chars_ptr;
      width  : in C.int;
      height : in C.int)
      return   C.int;
   pragma Import (C, Tk_DefineBitmap, "Tk_DefineBitmap");

   --  45

   procedure Tk_DefineCursor (window : in Tk_Window; cursor : in Tk_Cursor);
   pragma Import (C, Tk_DefineCursor, "Tk_DefineCursor");

   --  46

   procedure Tk_DeleteAllBindings
     (bindingTable : in Tk_BindingTable;
      object       : in ClientData);
   pragma Import (C, Tk_DeleteAllBindings, "Tk_DeleteAllBindings");

   --  47

   function Tk_DeleteBinding
     (interp       : in Tcl_Interp;
      bindingTable : in Tk_BindingTable;
      object       : in ClientData;
      eventStr     : in C.Strings.chars_ptr)
      return         C.int;
   pragma Import (C, Tk_DeleteBinding, "Tk_DeleteBinding");

   --  48

   procedure Tk_DeleteBindingTable (bindingTable : in Tk_BindingTable);
   pragma Import (C, Tk_DeleteBindingTable, "Tk_DeleteBindingTable");

   --  49

   procedure Tk_DeleteErrorHandler (handler : in Tk_ErrorHandler);
   pragma Import (C, Tk_DeleteErrorHandler, "Tk_DeleteErrorHandler");

   --  50

   --  51

   --  52

   procedure Tk_DeleteImage
     (interp : in Tcl_Interp;
      name   : in C.Strings.chars_ptr);
   pragma Import (C, Tk_DeleteImage, "Tk_DeleteImage");

   --  53

   --  54

   procedure Tk_DestroyWindow (tkwin : in Tk_Window);
   pragma Import (C, Tk_DestroyWindow, "Tk_DestroyWindow");

   --  55

   function Tk_DisplayName
     (tkwin : in Tk_Window)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tk_DisplayName, "Tk_DisplayName");

   --  56

   function Tk_DistanceToTextLayout
     (layout : in Tk_TextLayout;
      x      : in C.int;
      y      : in C.int)
      return   C.int;
   pragma Import (C, Tk_DistanceToTextLayout, "Tk_DistanceToTextLayout");

   --  57

   --  58

   --  59

   --  60

   --  61

   --  62

   --  63

   --  64

   function Tk_FindPhoto
     (interp    : in Tcl_Interp;
      imageName : in C.Strings.chars_ptr)
      return      Tk_PhotoHandle;
   pragma Import (C, Tk_FindPhoto, "Tk_FindPhoto");

   --  65

   --  66

   procedure Tk_Free3DBorder (border : in Tk_3DBorder);
   pragma Import (C, Tk_Free3DBorder, "Tk_Free3DBorder");

   --  67

   --  68

   --  69

   --  70

   --  71

   procedure Tk_FreeFont (f : in Tk_Font);
   pragma Import (C, Tk_FreeFont, "Tk_FreeFont");

   --  72

   --  73

   procedure Tk_FreeImage (image : in Tk_Image);
   pragma Import (C, Tk_FreeImage, "Tk_FreeImage");

   --  74

   --  75

   --  76

   procedure Tk_FreeTextLayout (textLayout : in Tk_TextLayout);
   pragma Import (C, Tk_FreeTextLayout, "Tk_FreeTextLayout");

   --  77

   --  78

   --  79

   procedure Tk_GeometryRequest
     (tkwin     : in Tk_Window;
      reqWidth  : in C.int;
      reqHeight : in C.int);
   pragma Import (C, Tk_GeometryRequest, "Tk_GeometryRequest");

   --  80

   function Tk_Get3DBorder
     (interp    : in Tcl_Interp;
      tkwin     : in Tk_Window;
      colorName : in Tk_Uid)
      return      Tk_3DBorder;
   pragma Import (C, Tk_Get3DBorder, "Tk_Get3DBorder");

   --  81

   procedure Tk_GetAllBindings
     (interp       : in Tcl_Interp;
      bindingTable : in Tk_BindingTable;
      object       : in ClientData);
   pragma Import (C, Tk_GetAllBindings, "Tk_GetAllBindings");

   --  82

   function Tk_GetAnchor
     (interp    : in Tcl_Interp;
      str       : in C.Strings.chars_ptr;
      anchorPtr : in Tk_Anchor)
      return      C.int;
   pragma Import (C, Tk_GetAnchor, "Tk_GetAnchor");

   --  83

   --  84

   function Tk_GetBinding
     (interp       : in Tcl_Interp;
      bindingTable : in Tk_BindingTable;
      object       : in ClientData;
      eventStr     : in C.Strings.chars_ptr)
      return         C.Strings.chars_ptr;
   pragma Import (C, Tk_GetBinding, "Tk_GetBinding");

   --  85

   --  86

   --  87

   function Tk_GetCapStyle
     (interp : in Tcl_Interp;
      str    : in C.Strings.chars_ptr;
      capPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetCapStyle, "Tk_GetCapStyle");

   --  88

   --  89

   --  90

   --  91

   function Tk_GetCursor
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      str    : in Tk_Uid)
      return   Tk_Cursor;
   pragma Import (C, Tk_GetCursor, "Tk_GetCursor");

   --  92

   function Tk_GetCursorFromData
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      source : in C.Strings.chars_ptr;
      mask   : in C.Strings.chars_ptr;
      width  : in C.int;
      height : in C.int;
      xHot   : in C.int;
      yHot   : in C.int;
      fg     : in Tk_Uid;
      bg     : in Tk_Uid)
      return   Tk_Cursor;
   pragma Import (C, Tk_GetCursorFromData, "Tk_GetCursorFromData");

   --  93

   function Tk_GetFont
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      str    : in C.Strings.chars_ptr)
      return   Tk_Font;
   pragma Import (C, Tk_GetFont, "Tk_GetFont");

   --  94

   function Tk_GetFontFromObj
     (tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj)
      return   Tk_Font;
   pragma Import (C, Tk_GetFontFromObj, "Tk_GetFontFromObj");

   --  95

   procedure Tk_GetFontMetrics
     (font  : in Tk_Font;
      fmPtr : in Tk_FontMetrics);
   pragma Import (C, Tk_GetFontMetrics, "Tk_GetFontMetrics");

   --  96

   --  97

   function Tk_GetImage
     (interp     : in Tcl_Interp;
      tkwin      : in Tk_Window;
      name       : in C.Strings.chars_ptr;
      changeProc : in Tk_ImageChangedProc;
      data       : in ClientData)
      return       Tk_Image;
   pragma Import (C, Tk_GetImage, "Tk_GetImage");

   --  98

   function Tk_GetImageMasterData
     (interp     : in Tcl_Interp;
      name       : in C.Strings.chars_ptr;
      typePtrPtr : in Tk_ImageType)
      return       ClientData;
   pragma Import (C, Tk_GetImageMasterData, "Tk_GetImageMasterData");

   --  99

   function Tk_GetItemTypes return Tk_ItemType;
   pragma Import (C, Tk_GetItemTypes, "Tk_GetItemTypes");

   --  100

   function Tk_GetJoinStyle
     (interp  : in Tcl_Interp;
      str     : in C.Strings.chars_ptr;
      joinPtr : access C.int)
      return    C.int;
   pragma Import (C, Tk_GetJoinStyle, "Tk_GetJoinStyle");

   --  101

   function Tk_GetJustify
     (interp     : in Tcl_Interp;
      str        : in C.Strings.chars_ptr;
      justifyPtr : in Tk_Justify)
      return       C.int;
   pragma Import (C, Tk_GetJustify, "Tk_GetJustify");

   --  102

   function Tk_GetNumMainWindows return C.int;
   pragma Import (C, Tk_GetNumMainWindows, "Tk_GetNumMainWindows");

   --  103

   function Tk_GetOption
     (tkwin     : in Tk_Window;
      name      : in C.Strings.chars_ptr;
      className : in C.Strings.chars_ptr)
      return      Tk_Uid;
   pragma Import (C, Tk_GetOption, "Tk_GetOption");

   --  104

   function Tk_GetPixels
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      str    : in C.Strings.chars_ptr;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetPixels, "Tk_GetPixels");

   --  105

   --  106

   function Tk_GetRelief
     (interp    : in Tcl_Interp;
      name      : in C.Strings.chars_ptr;
      reliefPtr : access C.int)
      return      C.int;
   pragma Import (C, Tk_GetRelief, "Tk_GetRelief");

   --  107

   procedure Tk_GetRootCoords
     (tkwin : in Tk_Window;
      xPtr  : access C.int;
      yPtr  : access C.int);
   pragma Import (C, Tk_GetRootCoords, "Tk_GetRootCoords");

   --  108

   function Tk_GetScrollInfo
     (interp : in Tcl_Interp;
      argc   : in C.int;
      argv   : in CArgv.Chars_Ptr_Ptr;
      dblPtr : access C.double;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetScrollInfo, "Tk_GetScrollInfo");

   --  109

   function Tk_GetScreenMM
     (interp    : in Tcl_Interp;
      tkwin     : in Tk_Window;
      str       : in C.Strings.chars_ptr;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_GetScreenMM, "Tk_GetScreenMM");

   --  110

   --  111

   function Tk_GetUid (str : in C.Strings.chars_ptr) return Tk_Uid;
   pragma Import (C, Tk_GetUid, "Tk_GetUid");

   --  112

   --  113

   procedure Tk_GetVRootGeometry
     (tkwin     : in Tk_Window;
      xPtr      : access C.int;
      yPtr      : access C.int;
      widthPtr  : access C.int;
      heightPtr : access C.int);
   pragma Import (C, Tk_GetVRootGeometry, "Tk_GetVRootGeometry");

   --  114

   function Tk_Grab
     (interp     : in Tcl_Interp;
      tkwin      : in Tk_Window;
      grabGlobal : in C.int)
      return       C.int;
   pragma Import (C, Tk_Grab, "Tk_Grab");

   --  115

   --  116

   --  117

   procedure Tk_ImageChanged
     (master      : in Tk_ImageMaster;
      x           : in C.int;
      y           : in C.int;
      width       : in C.int;
      height      : in C.int;
      imageWidth  : in C.int;
      imageHeight : in C.int);
   pragma Import (C, Tk_ImageChanged, "Tk_ImageChanged");

   --  118

   function Tk_Init (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tk_Init, "Tk_Init");

   --  119

   --  120

   function Tk_IntersectTextLayout
     (layout : in Tk_TextLayout;
      x      : in C.int;
      y      : in C.int;
      width  : in C.int;
      height : in C.int)
      return   C.int;
   pragma Import (C, Tk_IntersectTextLayout, "Tk_IntersectTextLayout");

   --  121

   procedure Tk_MaintainGeometry
     (slave  : in Tk_Window;
      master : in Tk_Window;
      x      : in C.int;
      y      : in C.int;
      width  : in C.int;
      height : in C.int);
   pragma Import (C, Tk_MaintainGeometry, "Tk_MaintainGeometry");

   --  122

   function Tk_MainWindow (interp : in Tcl_Interp) return Tk_Window;
   pragma Import (C, Tk_MainWindow, "Tk_MainWindow");

   --  123

   procedure Tk_MakeWindowExist (tkwin : in Tk_Window);
   pragma Import (C, Tk_MakeWindowExist, "Tk_MakeWindowExist");

   --  124

   procedure Tk_ManageGeometry
     (tkwin  : in Tk_Window;
      mgrPtr : in Tk_GeomMgr;
      data   : in ClientData);
   pragma Import (C, Tk_ManageGeometry, "Tk_ManageGeometry");

   --  125

   procedure Tk_MapWindow (tkwin : in Tk_Window);
   pragma Import (C, Tk_MapWindow, "Tk_MapWindow");

   --  126

   function Tk_MeasureChars
     (tkfont    : in Tk_Font;
      source    : in C.Strings.chars_ptr;
      numBytes  : in C.int;
      maxPixels : in C.int;
      flags     : in C.int;
      lengthPtr : access C.int)
      return      C.int;
   pragma Import (C, Tk_MeasureChars, "Tk_MeasureChars");

   --  127

   procedure Tk_MoveResizeWindow
     (tkwin  : in Tk_Window;
      x      : in C.int;
      y      : in C.int;
      width  : in C.int;
      height : in C.int);
   pragma Import (C, Tk_MoveResizeWindow, "Tk_MoveResizeWindow");

   --  128

   procedure Tk_MoveWindow
     (tkwin : in Tk_Window;
      x     : in C.int;
      y     : in C.int);
   pragma Import (C, Tk_MoveWindow, "Tk_MoveWindow");

   --  129

   procedure Tk_MoveToplevelWindow
     (tkwin : in Tk_Window;
      x     : in C.int;
      y     : in C.int);
   pragma Import (C, Tk_MoveToplevelWindow, "Tk_MoveToplevelWindow");

   --  130

   function Tk_NameOf3DBorder
     (border : in Tk_3DBorder)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOf3DBorder, "Tk_NameOf3DBorder");

   --  131

   function Tk_NameOfAnchor
     (anchor : in Tk_Anchor)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfAnchor, "Tk_NameOfAnchor");

   --  132

   --  133

   function Tk_NameOfCapStyle (cap : in C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfCapStyle, "Tk_NameOfCapStyle");

   --  134

   --  135

   --  136

   function Tk_NameOfFont (font : in Tk_Font) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfFont, "Tk_NameOfFont");

   --  137

   function Tk_NameOfImage
     (imageMaster : in Tk_ImageMaster)
      return        C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfImage, "Tk_NameOfImage");

   --  138

   function Tk_NameOfJoinStyle (join : in C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfJoinStyle, "Tk_NameOfJoinStyle");

   --  139

   function Tk_NameOfJustify
     (justify : in Tk_Justify)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfJustify, "Tk_NameOfJustify");

   --  140

   function Tk_NameOfRelief (relief : in C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfRelief, "Tk_NameOfRelief");

   --  141

   function Tk_NameToWindow
     (interp   : in Tcl_Interp;
      pathName : in C.Strings.chars_ptr;
      tkwin    : in Tk_Window)
      return     Tk_Window;
   pragma Import (C, Tk_NameToWindow, "Tk_NameToWindow");

   --  142

   --  143

   function Tk_ParseArgv
     (interp   : in Tcl_Interp;
      tkwin    : in Tk_Window;
      argcPtr  : access C.int;
      argv     : in CArgv.Chars_Ptr_Ptr;
      argTable : in Tk_ArgvInfo;
      flags    : in C.int)
      return     C.int;
   pragma Import (C, Tk_ParseArgv, "Tk_ParseArgv");

   --  144

   procedure Tk_PhotoPutBlock
     (handle   : in Tk_PhotoHandle;
      blockPtr : in Tk_PhotoImageBlock;
      x        : in C.int;
      y        : in C.int;
      width    : in C.int;
      height   : in C.int);
   pragma Import (C, Tk_PhotoPutBlock, "Tk_PhotoPutBlock");

   --  145

   procedure Tk_PhotoPutZoomedBlock
     (handle     : in Tk_PhotoHandle;
      blockPtr   : in Tk_PhotoImageBlock;
      x          : in C.int;
      y          : in C.int;
      width      : in C.int;
      height     : in C.int;
      zoomX      : in C.int;
      zoomY      : in C.int;
      subsampleX : in C.int;
      subsampleY : in C.int);
   pragma Import (C, Tk_PhotoPutZoomedBlock, "Tk_PhotoPutZoomedBlock");

   --  146

   function Tk_PhotoGetImage
     (handle   : in Tk_PhotoHandle;
      blockPtr : in Tk_PhotoImageBlock)
      return     C.int;
   pragma Import (C, Tk_PhotoGetImage, "Tk_PhotoGetImage");

   --  147

   procedure Tk_PhotoBlank (handle : in Tk_PhotoHandle);
   pragma Import (C, Tk_PhotoBlank, "Tk_PhotoBlank");

   --  148

   procedure Tk_PhotoExpand
     (handle : in Tk_PhotoHandle;
      width  : in C.int;
      height : in C.int);
   pragma Import (C, Tk_PhotoExpand, "Tk_PhotoExpand");

   --  149

   procedure Tk_PhotoGetSize
     (handle    : in Tk_PhotoHandle;
      widthPtr  : access C.int;
      heightPtr : access C.int);
   pragma Import (C, Tk_PhotoGetSize, "Tk_PhotoGetSize");

   --  150

   procedure Tk_PhotoSetSize
     (handle : in Tk_PhotoHandle;
      width  : in C.int;
      height : in C.int);
   pragma Import (C, Tk_PhotoSetSize, "Tk_PhotoSetSize");

   --  151

   function Tk_PointToChar
     (layout : in Tk_TextLayout;
      x      : in C.int;
      y      : in C.int)
      return   C.int;
   pragma Import (C, Tk_PointToChar, "Tk_PointToChar");

   --  152

   function Tk_PostscriptFontName
     (tkfont : in Tk_Font;
      dsPtr  : in Tcl_DString)
      return   C.int;
   pragma Import (C, Tk_PostscriptFontName, "Tk_PostscriptFontName");

   --  153

   --  154

   --  155

   --  156

   procedure Tk_ResizeWindow
     (tkwin  : in Tk_Window;
      width  : in C.int;
      height : in C.int);
   pragma Import (C, Tk_ResizeWindow, "Tk_ResizeWindow");

   --  157

   function Tk_RestackWindow
     (tkwin      : in Tk_Window;
      aboveBelow : in C.int;
      other      : in Tk_Window)
      return       C.int;
   pragma Import (C, Tk_RestackWindow, "Tk_RestackWindow");

   --  158

   --  159

   function Tk_SafeInit (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tk_SafeInit, "Tk_SafeInit");

   --  160

   function Tk_SetAppName
     (tkwin : in Tk_Window;
      name  : in C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tk_SetAppName, "Tk_SetAppName");

   --  161

   procedure Tk_SetBackgroundFromBorder
     (tkwin  : in Tk_Window;
      border : in Tk_3DBorder);
   pragma Import
     (C,
      Tk_SetBackgroundFromBorder,
      "Tk_SetBackgroundFromBorder");

   --  162

   procedure Tk_SetClass
     (tkwin     : in Tk_Window;
      className : in C.Strings.chars_ptr);
   pragma Import (C, Tk_SetClass, "Tk_SetClass");

   --  163

   procedure Tk_SetGrid
     (tkwin      : in Tk_Window;
      reqWidth   : in C.int;
      reqHeight  : in C.int;
      gridWidth  : in C.int;
      gridHeight : in C.int);
   pragma Import (C, Tk_SetGrid, "Tk_SetGrid");

   --  164

   procedure Tk_SetInternalBorder (tkwin : in Tk_Window; width : in C.int);
   pragma Import (C, Tk_SetInternalBorder, "Tk_SetInternalBorder");

   --  165

   procedure Tk_SetWindowBackground
     (tkwin : in Tk_Window;
      pixel : in C.unsigned_long);
   pragma Import (C, Tk_SetWindowBackground, "Tk_SetWindowBackground");

   --  166

   --  167

   procedure Tk_SetWindowBorder
     (tkwin : in Tk_Window;
      pixel : in C.unsigned_long);
   pragma Import (C, Tk_SetWindowBorder, "Tk_SetWindowBorder");

   --  168

   procedure Tk_SetWindowBorderWidth
     (tkwin : in Tk_Window;
      width : in C.int);
   pragma Import (C, Tk_SetWindowBorderWidth, "Tk_SetWindowBorderWidth");

   --  169

   --  170

   --  171

   --  172

   --  173

   procedure Tk_SizeOfImage
     (image     : in Tk_Image;
      widthPtr  : access C.int;
      heightPtr : access C.int);
   pragma Import (C, Tk_SizeOfImage, "Tk_SizeOfImage");

   --  174

   function Tk_StrictMotif (tkwin : in Tk_Window) return C.int;
   pragma Import (C, Tk_StrictMotif, "Tk_StrictMotif");

   --  175

   procedure Tk_TextLayoutToPostscript
     (interp : in Tcl_Interp;
      layout : in Tk_TextLayout);
   pragma Import (C, Tk_TextLayoutToPostscript, "Tk_TextLayoutToPostscript");

   --  176

   function Tk_TextWidth
     (font     : in Tk_Font;
      str      : in C.Strings.chars_ptr;
      numBytes : in C.int)
      return     C.int;
   pragma Import (C, Tk_TextWidth, "Tk_TextWidth");

   --  177

   procedure Tk_UndefineCursor (window : in Tk_Window);
   pragma Import (C, Tk_UndefineCursor, "Tk_UndefineCursor");

   --  178

   --  179

   --  180

   procedure Tk_Ungrab (tkwin : in Tk_Window);
   pragma Import (C, Tk_Ungrab, "Tk_Ungrab");

   --  181

   procedure Tk_UnmaintainGeometry
     (slave  : in Tk_Window;
      master : in Tk_Window);
   pragma Import (C, Tk_UnmaintainGeometry, "Tk_UnmaintainGeometry");

   --  182

   procedure Tk_UnmapWindow (tkwin : in Tk_Window);
   pragma Import (C, Tk_UnmapWindow, "Tk_UnmapWindow");

   --  183

   procedure Tk_UnsetGrid (tkwin : in Tk_Window);
   pragma Import (C, Tk_UnsetGrid, "Tk_UnsetGrid");

   --  184

   procedure Tk_UpdatePointer
     (tkwin : in Tk_Window;
      x     : in C.int;
      y     : in C.int;
      state : in C.int);
   pragma Import (C, Tk_UpdatePointer, "Tk_UpdatePointer");

   --  185

   --  186

   function Tk_Alloc3DBorderFromObj
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj)
      return   Tk_3DBorder;
   pragma Import (C, Tk_Alloc3DBorderFromObj, "Tk_Alloc3DBorderFromObj");

   --  187

   --  188

   function Tk_AllocCursorFromObj
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj)
      return   Tk_Cursor;
   pragma Import (C, Tk_AllocCursorFromObj, "Tk_AllocCursorFromObj");

   --  189

   function Tk_AllocFontFromObj
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj)
      return   Tk_Font;
   pragma Import (C, Tk_AllocFontFromObj, "Tk_AllocFontFromObj");

   --  190

   function Tk_CreateOptionTable
     (interp      : in Tcl_Interp;
      templatePtr : in Tk_OptionSpec)
      return        Tk_OptionTable;
   pragma Import (C, Tk_CreateOptionTable, "Tk_CreateOptionTable");

   --  191

   procedure Tk_DeleteOptionTable (optionTable : in Tk_OptionTable);
   pragma Import (C, Tk_DeleteOptionTable, "Tk_DeleteOptionTable");

   --  192

   procedure Tk_Free3DBorderFromObj
     (tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj);
   pragma Import (C, Tk_Free3DBorderFromObj, "Tk_Free3DBorderFromObj");

   --  193

   procedure Tk_FreeBitmapFromObj
     (tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj);
   pragma Import (C, Tk_FreeBitmapFromObj, "Tk_FreeBitmapFromObj");

   --  194

   procedure Tk_FreeColorFromObj
     (tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj);
   pragma Import (C, Tk_FreeColorFromObj, "Tk_FreeColorFromObj");

   --  195

   procedure Tk_FreeConfigOptions
     (recordPtr   : in C.Strings.chars_ptr;
      optionToken : in Tk_OptionTable;
      tkwin       : in Tk_Window);
   pragma Import (C, Tk_FreeConfigOptions, "Tk_FreeConfigOptions");

   --  196

   procedure Tk_FreeSavedOptions (savePtr : in Tk_SavedOptions);
   pragma Import (C, Tk_FreeSavedOptions, "Tk_FreeSavedOptions");

   --  197

   procedure Tk_FreeCursorFromObj
     (tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj);
   pragma Import (C, Tk_FreeCursorFromObj, "Tk_FreeCursorFromObj");

   --  198

   procedure Tk_FreeFontFromObj (tkwin : in Tk_Window; objPtr : in Tcl_Obj);
   pragma Import (C, Tk_FreeFontFromObj, "Tk_FreeFontFromObj");

   --  199

   function Tk_Get3DBorderFromObj
     (tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj)
      return   Tk_3DBorder;
   pragma Import (C, Tk_Get3DBorderFromObj, "Tk_Get3DBorderFromObj");

   --  200

   function Tk_GetAnchorFromObj
     (interp    : in Tcl_Interp;
      objPtr    : in Tcl_Obj;
      anchorPtr : in Tk_Anchor)
      return      C.int;
   pragma Import (C, Tk_GetAnchorFromObj, "Tk_GetAnchorFromObj");

   --  201

   --  202

   --  203

   function Tk_GetCursorFromObj
     (tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj)
      return   Tk_Cursor;
   pragma Import (C, Tk_GetCursorFromObj, "Tk_GetCursorFromObj");

   --  204

   function Tk_GetOptionInfo
     (interp      : in Tcl_Interp;
      recordPtr   : in C.Strings.chars_ptr;
      optionTable : in Tk_OptionTable;
      namePtr     : in Tcl_Obj;
      tkwin       : in Tk_Window)
      return        Tcl_Obj;
   pragma Import (C, Tk_GetOptionInfo, "Tk_GetOptionInfo");

   --  205

   function Tk_GetOptionValue
     (interp      : in Tcl_Interp;
      recordPtr   : in C.Strings.chars_ptr;
      optionTable : in Tk_OptionTable;
      namePtr     : in Tcl_Obj;
      tkwin       : in Tk_Window)
      return        Tcl_Obj;
   pragma Import (C, Tk_GetOptionValue, "Tk_GetOptionValue");

   --  206

   function Tk_GetJustifyFromObj
     (interp     : in Tcl_Interp;
      objPtr     : in Tcl_Obj;
      justifyPtr : in Tk_Justify)
      return       C.int;
   pragma Import (C, Tk_GetJustifyFromObj, "Tk_GetJustifyFromObj");

   --  207

   function Tk_GetMMFromObj
     (interp    : in Tcl_Interp;
      tkwin     : in Tk_Window;
      objPtr    : in Tcl_Obj;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_GetMMFromObj, "Tk_GetMMFromObj");

   --  208

   function Tk_GetPixelsFromObj
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      objPtr : in Tcl_Obj;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetPixelsFromObj, "Tk_GetPixelsFromObj");

   --  209

   function Tk_GetReliefFromObj
     (interp    : in Tcl_Interp;
      objPtr    : in Tcl_Obj;
      resultPtr : access C.int)
      return      C.int;
   pragma Import (C, Tk_GetReliefFromObj, "Tk_GetReliefFromObj");

   --  210

   function Tk_GetScrollInfoObj
     (interp : in Tcl_Interp;
      objc   : in C.int;
      objv   : in Tcl_Obj_Array;
      dblPtr : access C.double;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetScrollInfoObj, "Tk_GetScrollInfoObj");

   --  211

   function Tk_InitOptions
     (interp      : in Tcl_Interp;
      recordPtr   : in C.Strings.chars_ptr;
      optionToken : in Tk_OptionTable;
      tkwin       : in Tk_Window)
      return        C.int;
   pragma Import (C, Tk_InitOptions, "Tk_InitOptions");

   --  212

   procedure Tk_MainEx
     (argc        : in C.int;
      argv        : in CArgv.Chars_Ptr_Ptr;
      appInitProc : in Tcl_AppInitProc;
      interp      : in Tcl_Interp);
   pragma Import (C, Tk_MainEx, "Tk_MainEx");

   --  213

   procedure Tk_RestoreSavedOptions (savePtr : in Tk_SavedOptions);
   pragma Import (C, Tk_RestoreSavedOptions, "Tk_RestoreSavedOptions");

   --  214

   function Tk_SetOptions
     (interp      : in Tcl_Interp;
      recordPtr   : in C.Strings.chars_ptr;
      optionTable : in Tk_OptionTable;
      objc        : in C.int;
      objv        : in Tcl_Obj_Array;
      tkwin       : in Tk_Window;
      savePtr     : in Tk_SavedOptions;
      maskPtr     : access C.int)
      return        C.int;
   pragma Import (C, Tk_SetOptions, "Tk_SetOptions");

   --  215

   procedure Tk_InitConsoleChannels (interp : in Tcl_Interp);
   pragma Import (C, Tk_InitConsoleChannels, "Tk_InitConsoleChannels");

   --  216

   function Tk_CreateConsoleWindow (interp : in Tcl_Interp) return C.int;
   pragma Import (C, Tk_CreateConsoleWindow, "Tk_CreateConsoleWindow");

   --  217

   procedure Tk_CreateSmoothMethod
     (interp : in Tcl_Interp;
      method : in Tk_SmoothMethod);
   pragma Import (C, Tk_CreateSmoothMethod, "Tk_CreateSmoothMethod");

   --  Slot 218 is reserved

   --  Slot 219 is reserved

   --  220

   function Tk_GetDash
     (interp : in Tcl_Interp;
      value  : in C.Strings.chars_ptr;
      dash   : in Tk_Dash)
      return   C.int;
   pragma Import (C, Tk_GetDash, "Tk_GetDash");

   --  221

   procedure Tk_CreateOutline (outline : in Tk_Outline);
   pragma Import (C, Tk_CreateOutline, "Tk_CreateOutline");

   --  222

   --  223

   --  224

   function Tk_ChangeOutlineGC
     (canvas  : in Tk_Canvas;
      item    : in Tk_Item;
      outline : in Tk_Outline)
      return    C.int;
   pragma Import (C, Tk_ChangeOutlineGC, "Tk_ChangeOutlineGC");

   --  225

   function Tk_ResetOutlineGC
     (canvas  : in Tk_Canvas;
      item    : in Tk_Item;
      outline : in Tk_Outline)
      return    C.int;
   pragma Import (C, Tk_ResetOutlineGC, "Tk_ResetOutlineGC");

   --  226

   function Tk_CanvasPsOutline
     (canvas  : in Tk_Canvas;
      item    : in Tk_Item;
      outline : in Tk_Outline)
      return    C.int;
   pragma Import (C, Tk_CanvasPsOutline, "Tk_CanvasPsOutline");

   --  227

   --  228

   function Tk_CanvasGetCoordFromObj
     (interp    : in Tcl_Interp;
      canvas    : in Tk_Canvas;
      obj       : in Tcl_Obj;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_CanvasGetCoordFromObj, "Tk_CanvasGetCoordFromObj");

   --  229

   --  230

   procedure Tk_DitherPhoto
     (handle : in Tk_PhotoHandle;
      x      : in C.int;
      y      : in C.int;
      width  : in C.int;
      height : in C.int);
   pragma Import (C, Tk_DitherPhoto, "Tk_DitherPhoto");

   --  231

   --  232

   --  233

   function Tk_PostscriptFont
     (interp : in Tcl_Interp;
      psInfo : in Tk_PostscriptInfo;
      font   : in Tk_Font)
      return   C.int;
   pragma Import (C, Tk_PostscriptFont, "Tk_PostscriptFont");

   --  234

   function Tk_PostscriptImage
     (image   : in Tk_Image;
      interp  : in Tcl_Interp;
      tkwin   : in Tk_Window;
      psinfo  : in Tk_PostscriptInfo;
      x       : in C.int;
      y       : in C.int;
      width   : in C.int;
      height  : in C.int;
      prepass : in C.int)
      return    C.int;
   pragma Import (C, Tk_PostscriptImage, "Tk_PostscriptImage");

   --  235

   procedure Tk_PostscriptPath
     (interp    : in Tcl_Interp;
      psInfo    : in Tk_PostscriptInfo;
      coordPtr  : access C.double;
      numPoints : in C.int);
   pragma Import (C, Tk_PostscriptPath, "Tk_PostscriptPath");

   --  236

   --  237

   function Tk_PostscriptY
     (y      : in C.double;
      psInfo : in Tk_PostscriptInfo)
      return   C.double;
   pragma Import (C, Tk_PostscriptY, "Tk_PostscriptY");

   --  defined {USE_TK_STUBS} && !defined {USE_TK_STUB_PROCS}

   --  !END!: Do not edit above this line.

   TCL_STORAGE_CLASS : constant String := "DLLIMPORT";
   --
   --  Tcl commands exported by Tk:
   --

   --
   --  end block for C++
   --

private
   type Tk_3DBorder_rec is null record;
   Null_Tk_3DBorder : constant Tk_3DBorder := null;

   type Tk_ArgvInfo_rec is null record;
   Null_Tk_ArgvInfo : constant Tk_ArgvInfo := null;

   type Tk_BindingTable_rec is null record;
   Null_Tk_BindingTable : constant Tk_BindingTable := null;

   type Tk_Canvas_rec is null record;
   Null_Tk_Canvas : constant Tk_Canvas := null;

   type Tk_CanvasTextInfo_rec is record
      --  Border and background for selected characters.
      --  Read-only to items.
      selBorder : Tk_3DBorder;
      --  Width of border around selection.
      --  Read-only to items.
      selBorderWidth : C.int;
      --  Foreground color for selected text.
      --  Read-only to items.
      selFgColorPtr : System.Address;   -- @todo should be XColor* (Xlib)
      --  Pointer to selected item.
      --  NULL means selection isn't in this canvas.
      --  Writable by items.
      selItemPtr : Tk_Item;
      --  Character index of first selected character.
      --  Writable by items.
      selectFirst : C.int;
      --  Character index of last selected character.
      --  Writable by items.
      selectLast : C.int;
      --  Item corresponding to "selectAnchor": not necessarily selItemPtr.
      --  Read-only to items.
      anchorItemPtr : Tk_Item;
      --  Character index of fixed end of selection (i.e. "select to"
      --  operation will use this as one end of the selection).
      --  Writable by items.
      selectAnchor : C.int;
      --  Used to draw vertical bar for insertion cursor.
      --  Read-only to items.
      insertBorder : Tk_3DBorder;
      --  Total width of insertion cursor. Read-only to items.
      insertWidth : C.int;
      --  Width of 3-D border around insert cursor.
      --  Read-only to items.
      insertBorderWidth : C.int;
      --  Item that currently has the input focus, or NULL if no such item.
      --  Read-only to items.
      focusItemPtr : Tk_Item;
      --  Non-zero means that the canvas widget has the input focus.
      --  Read-only to items.
      gotFocus : C.int;
      --  Non-zero means that an insertion cursor should be displayed in 
      --  focusItemPtr. Read-only to items.
      cursorOn : C.int;
   end record;
   pragma Convention (C, Tk_CanvasTextInfo_rec);

   Null_Tk_CanvasTextInfo : constant Tk_CanvasTextInfo := null;

   type Tk_ConfigSpec_rec is record
      typ : C.int;
      --  Type of option, such as TK_CONFIG_COLOR;
      --  see definitions below.  Last option in
      --  table must have type TK_CONFIG_END.
      argvName : C.Strings.chars_ptr;
      --  Switch used to specify option in argv.
      --  NULL means this spec is part of a group.
      dbName : C.Strings.chars_ptr;
      --  Name for option in option database.
      dbClass : C.Strings.chars_ptr;
      --  Class for option in database.
      defValue : C.Strings.chars_ptr;
      --  Default value for option if not
      --  specified in command line or database.
      offset : C.int;
      --  Where in widget record to store value;
      --  use Tk_Offset macro to generate values
      --  for this.
      specFlags : C.int;
      --  Any combination of the values defined
      --  below;  other bits are used internally
      --  by tkConfig.c.
      customPtr : Tk_CustomOption;
      --  If type is TK_CONFIG_CUSTOM then this is
      --  a pointer to info about how to parse and
      --  print the option.  Otherwise it is
      --  irrelevant.
   end record;
   pragma Convention (C, Tk_ConfigSpec_rec);

   Null_Tk_ConfigSpec : constant Tk_ConfigSpec := null;

   type Tk_Cursor_rec is null record;
   Null_Tk_Cursor : constant Tk_Cursor := null;

   type Tk_CustomOption_rec is record
      parseProc : Tk_OptionParseProc;
      --  Procedure to call to parse an
      --  option and store it in converted
      --  form.
      printProc : Tk_OptionPrintProc;
      --  Procedure to return a printable
      --  string describing an existing
      --  option.
      data : ClientData;
      --  Arbitrary one-word value used by
      --  option parser:  passed to
      --  parseProc and printProc.
   end record;
   pragma Convention (C, Tk_CustomOption_rec);

   Null_Tk_CustomOption : constant Tk_CustomOption := null;

   type Tk_Dash_rec is record
      number : C.int;
      key    : C.char_array (0 .. 3);
   end record;
   pragma Convention (C, Tk_Dash_rec);

   Null_Tk_Dash : constant Tk_Dash := null;

   type Tk_ErrorHandler_rec is null record;
   Null_Tk_ErrorHandler : constant Tk_ErrorHandler := null;

   type Tk_FakeWin_rec is null record;
   Null_Tk_FakeWin : constant Tk_FakeWin := null;

   type Tk_Font_rec is null record;
   Null_Tk_Font : constant Tk_Font := null;

   type Tk_FontMetrics_rec is record
      ascent : C.int;
      --  The amount in pixels that the tallest
      --  letter sticks up above the baseline, plus
      --  any extra blank space added by the designer
      --  of the font.
      descent : C.int;
      --  The largest amount in pixels that any
      --  letter sticks below the baseline, plus any
      --  extra blank space added by the designer of
      --  the font.
      linespace : C.int;
      --  The sum of the ascent and descent.  How
      --  far apart two lines of text in the same
      --  font should be placed so that none of the
      --  characters in one line overlap any of the
      --  characters in the other line.
   end record;
   pragma Convention (C, Tk_FontMetrics_rec);

   Null_Tk_FontMetrics : constant Tk_FontMetrics := null;

   type Tk_GeomMgr_rec is record
      name : C.Strings.chars_ptr;
      --  Name of the geometry manager {command
      --  used to invoke it, or name of widget
      --  class that allows embedded widgets}.
      requestProc : Tk_GeomRequestProc;
      --  Procedure to invoke when a slave's
      --  requested geometry changes.
      lostSlaveProc : Tk_GeomLostSlaveProc;
      --  Procedure to invoke when a slave is
      --  taken away from one geometry manager
      --  by another.  NULL means geometry manager
      --  doesn't care when slaves are lost.
   end record;
   pragma Convention (C, Tk_GeomMgr_rec);

   Null_Tk_GeomMgr : constant Tk_GeomMgr := null;

   type Tk_Image_rec is null record;
   Null_Tk_Image : constant Tk_Image := null;

   type Tk_ImageMaster_rec is null record;
   Null_Tk_ImageMaster : constant Tk_ImageMaster := null;

   type Tk_ImageType_rec is null record;
   Null_Tk_ImageType : constant Tk_ImageType := null;

   --  auxiliary type for Ada
   type StaticTagSpace_Type is array (0 .. TK_TAG_SPACE-1) of Tk_Uid;
   pragma Convention (C, StaticTagSpace_Type);

   type Tk_Item_rec is record
      --  Unique identifier for this item (also
      --  serves as first tag for item).
      id : C.int;
      --  Next in display list of all items in this
      --  canvas. Later items in list are drawn on
      --  top of earlier ones.
      nextPtr : Tk_Item;
      --  Built-in space for limited # of tags.
      staticTagSpace : StaticTagSpace_Type;
      --  Pointer to array of tags. Usually points to
      --  staticTagSpace, but may point to malloc-ed
      --  space if there are lots of tags.
      tagPtr : System.Address;  -- @todo should be access Tk_Uid
      --  Total amount of tag space available at tagPtr.
      tagSpace : C.int;
      --  Number of tag slots actually used at *tagPtr.
      numTags : C.int;
      --  Table of procedures that implement this type of item.
      typePtr : Tk_ItemType;
      --  Bounding box for item, in integer canvas units.
      --  Set by item-specific code and guaranteed to contain
      --  every pixel drawn in item.
      --  Item area includes x1 and y1 but not x2 and y2.
      x1, y1, x2, y2 : C.int;
      --  Previous in display list of all items in this canvas.
      --  Later items in list are drawn just below earlier ones.
      prevPtr : Tk_Item;
      --  State of item.
      state : Tk_State;
      --  reserved for future use
      reserved1 : C.Strings.chars_ptr;
      --  Some flags used in the canvas
      redraw_flags : C.int;

      ----------------------------------------------------------------------
      --  Starting here is additional type-specific stuff; see the
      --  declarations for individual types to see what is part of each type.
      --  The actual space below is determined by the "itemInfoSize" of the
      --  type's Tk_ItemType record.
      ----------------------------------------------------------------------
   end record;
   pragma Convention (C, Tk_Item_rec);

   Null_Tk_Item : constant Tk_Item := null;

   type Tk_ItemType_rec is record
      --  The name of this type of item, such as "line".
      name : C.Strings.chars_ptr;
      --  Total amount of space needed for item's record.
      itemSize : C.int;
      --  Procedure to create a new item of this type.
      createProc : Tk_ItemCreateProc;
      --  Pointer to array of configuration specs for this type.
      --  Used for returning configuration info.
      configSpecs : Tk_ConfigSpec;
      --  Procedure to call to change configuration options.
      configProc : Tk_ItemConfigureProc;
      --  Procedure to call to get and set the item's coordinates.
      coordProc : Tk_ItemCoordProc;
      --  Procedure to delete existing item of this type.
      deleteProc : Tk_ItemDeleteProc;
      --  Procedure to display items of this type.
      displayProc : Tk_ItemDisplayProc;
      --  Non-zero means displayProc should be called even when
      --  the item has been moved off-screen.
      alwaysRedraw : C.int;
      --  Computes distance from item to a given point.
      pointProc : Tk_ItemPointProc;
      --  Computes whether item is inside, outside, or overlapping
      --  an area.
      areaProc : Tk_ItemAreaProc;
      --  Procedure to write a Postscript description for items of
      --  this type.
      postscriptProc : Tk_ItemPostscriptProc;
      --  Procedure to rescale items of this type.
      scaleProc : Tk_ItemScaleProc;
      --  Procedure to translate items of this type.
      indexProc : Tk_ItemIndexProc;
      --  Procedure to set insert cursor posn to just before a given
      --  position.
      icursorProc : Tk_ItemCursorProc;
      --  Procedure to return selection (in STRING format) when it is
      --  in this item.
      selectionProc : Tk_ItemSelectionProc;
      --  Procedure to insert something into an item.
      insertProc : Tk_ItemInsertProc;
      --  Procedure to delete characters from an item.
      --  dCharsProc Tk_ItemDCharsProc;
      --  Used to link types together into a list.
      nextPtr : Tk_ItemType;
      --  Reserved for future extension.
      reserved1 : C.Strings.chars_ptr;
      reserved2 : C.int;                --  Carefully compatible with
      reserved3 : C.Strings.chars_ptr;  --  Jan Nijtmans dash patch
      reserved4 : C.Strings.chars_ptr;
   end record;
   pragma Convention (C, Tk_ItemType_rec);

   Null_Tk_ItemType : constant Tk_ItemType := null;

   type Tk_OptionSpec_rec is record
      typ : Tk_OptionType;
      --  Type of option, such as TK_OPTION_COLOR;
      --  see definitions above. Last option in
      --  table must have type TK_OPTION_END.
      optionName : C.Strings.chars_ptr;
      --  Name used to specify option in Tcl
      --  commands.
      dbName : C.Strings.chars_ptr;
      --  Name for option in option database.
      dbClass : C.Strings.chars_ptr;
      --  Class for option in database.
      defValue : C.Strings.chars_ptr;
      --  Default value for option if not specified
      --  in command line, the option database,
      --  or the system.
      objOffset : C.int;
      --  Where in record to store a Tcl_Obj * that
      --  holds the value of this option, specified
      --  as an offset in bytes from the start of
      --  the record. Use the Tk_Offset macro to
      --  generate values for this.  -1 means don't
      --  store the Tcl_Obj in the record.
      internalOffset : C.int;
      --  Where in record to store the internal
      --  representation of the value of this option,
      --  such as an int or XColor *.  This field
      --  is specified as an offset in bytes
      --  from the start of the record. Use the
      --  Tk_Offset macro to generate values for it.
      --  -1 means don't store the internal
      --  representation in the record.
      flags : C.int;
      --  Any combination of the values defined
      --  below.
      data : ClientData;
      --  An alternate place to put option-specific
      --  data. Used for the monochrome default value
      --  for colors, etc.
      typeMask : C.int;
      --  An arbitrary bit mask defined by the
      --  class manager; typically bits correspond
      --  to certain kinds of options such as all
      --  those that require a redisplay when they
      --  change.  Tk_SetOptions returns the bit-wise
      --  OR of the typeMasks of all options that
      --  were changed.
   end record;
   pragma Convention (C, Tk_OptionSpec_rec);

   Null_Tk_OptionSpec : constant Tk_OptionSpec := null;

   type Tk_OptionTable_rec is null record;
   Null_Tk_OptionTable : constant Tk_OptionTable := null;

   type Tk_Outline_rec is null record;
   Null_Tk_Outline : constant Tk_Outline := null;

   type Tk_PhotoHandle_rec is null record;
   Null_Tk_PhotoHandle : constant Tk_PhotoHandle := null;

   type Tk_PhotoImageBlock_rec is record
      pixelPtr : C.Strings.chars_ptr;
      --  Pointer to the first pixel.
      width : C.int;
      --  Width of block, in pixels.
      height : C.int;
      --  Height of block, in pixels.
      pitch : C.int;
      --  Address difference between corresponding
      --  pixels in successive lines.
      pixelSize : C.int;
      --  Address difference between successive
      --  pixels in the same line.
      offset : CHelper.Int_Array (0 .. 3);
      --  Address differences between the red, green,
      --  blue and alpha components of the pixel and
      --  the pixel as a whole.
   end record;
   pragma Convention (C, Tk_PhotoImageBlock_rec);

   Null_Tk_PhotoImageBlock : constant Tk_PhotoImageBlock := null;

   type Tk_PhotoImageFormat_rec is null record;
   Null_Tk_PhotoImageFormat : constant Tk_PhotoImageFormat := null;

   type Tk_PostscriptInfo_rec is null record;
   Null_Tk_PostscriptInfo : constant Tk_PostscriptInfo := null;

   type Tk_SavedOption_rec is record
   --  Points to information that describes
   --  the option.
      valuePtr : Tcl_Obj;
      --  The old value of the option, in
      --  the form of a Tcl object; may be
      --  NULL if the value wasn't saved as
      --  an object.
      internalForm : C.double;
      --  The old value of the option, in
      --  some internal representation such
      --  as an int or {XColor *}.  Valid
      --  only if optionPtr->specPtr->objOffset
      --  is < 0.  The space must be large
      --  enough to accommodate a double, a
      --  long, or a pointer; right now it
      --  looks like a double is big
      --  enough.  Also, using a double
      --  guarantees that the field is
      --  properly aligned for storing large
      --  values.
   end record;
   pragma Convention (C, Tk_SavedOption_rec);

   Null_Tk_SavedOption : constant Tk_SavedOption := null;

   type Tk_SavedOptions_rec is record
      recordPtr : C.Strings.chars_ptr;
      --  The data structure in which to
      --  restore configuration options.
      tkwin : Tk_Window;
      --  Window associated with recordPtr;
      --  needed to restore certain options.
      numItems : C.int;
      --  The number of valid items in
      --  items field.
      items : Tk_SavedOption_Array (0 .. 19);
      --  Items used to hold old values.
      nextPtr : Tk_SavedOptions;
      --  Points to next structure in list;
      --  needed if too many options changed
      --  to hold all the old values in a
      --  single structure.  NULL means no
      --  more structures.
   end record;
   pragma Convention (C, Tk_SavedOptions_rec);

   Null_Tk_SavedOptions : constant Tk_SavedOptions := null;

   type Tk_SmoothMethod_rec is record
      name : C.Strings.chars_ptr;
      coordProc : System.Address;       --  @todo make exact type
      postscriptProc : System.Address;  --  @todo make exact type
   end record;
   pragma Convention (C, Tk_SmoothMethod_rec);

   Null_Tk_SmoothMethod : constant Tk_SmoothMethod := null;

   type Tk_TSOffset_rec is record
      flags : C.int;     --  flags; see tk.h for possible values
      xoffset : C.int;   --  x offset
      yoffset : C.int;   --  y offset
   end record;
   pragma Convention (C, Tk_TSOffset_rec);

   Null_Tk_TSOffset : constant Tk_TSOffset := null;

   type Tk_TextLayout_rec is null record;
   Null_Tk_TextLayout : constant Tk_TextLayout := null;

   type Tk_Window_rec is null record;
   Null_Tk_Window : constant Tk_Window := null;

   type XActivateDeactivateEvent_rec is null record;
   Null_XActivateDeactivateEvent : constant XActivateDeactivateEvent := null;

   type XVirtualEvent_rec is null record;
   Null_XVirtualEvent : constant XVirtualEvent := null;

end Tcl.Tk;
