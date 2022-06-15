--------------------------------------------------------------------
--
--  tcl-tk.ads -- This package is the "thin" binding to Tcl.Tk.
--
--  Copyright (c) 1995-2000 Terry J. Westley
--  Copyright (c) 2014, 2019 Simon Wright <simon@pushface.org>
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
--  This package was automatically generated from tk.h at patchlevel
--  8.3.0.  Note that some of the comments below, preserved from tk.h,
--  do not apply to the Ada version.  Someday, these comments may be
--  customized better.
--
--------------------------------------------------------------------

with CHelper;

package Tcl.Tk is

   --
   --  Dummy types that are used by clients:
   --

   type Tk_BindingTable_Rec (<>) is private;
   type Tk_BindingTable is access all Tk_BindingTable_Rec;
   pragma Convention (C, Tk_BindingTable);

   type Tk_Canvas_Rec (<>) is private;
   type Tk_Canvas is access all Tk_Canvas_Rec;
   pragma Convention (C, Tk_Canvas);

   type Tk_Cursor_Rec (<>) is private;
   type Tk_Cursor is access all Tk_Cursor_Rec;
   pragma Convention (C, Tk_Cursor);

   type Tk_ErrorHandler_Rec (<>) is private;
   type Tk_ErrorHandler is access all Tk_ErrorHandler_Rec;
   pragma Convention (C, Tk_ErrorHandler);

   type Tk_Font_Rec (<>) is private;
   type Tk_Font is access all Tk_Font_Rec;
   pragma Convention (C, Tk_Font);

   type Tk_Image_Rec (<>) is private;
   type Tk_Image is access all Tk_Image_Rec;
   pragma Convention (C, Tk_Image);

   type Tk_ImageMaster_Rec (<>) is private;
   type Tk_ImageMaster is access all Tk_ImageMaster_Rec;
   pragma Convention (C, Tk_ImageMaster);

   type Tk_OptionTable_Rec (<>) is private;
   type Tk_OptionTable is access all Tk_OptionTable_Rec;
   pragma Convention (C, Tk_OptionTable);

   type Tk_PostscriptInfo_Rec (<>) is private;
   type Tk_PostscriptInfo is access all Tk_PostscriptInfo_Rec;
   pragma Convention (C, Tk_PostscriptInfo);

   type Tk_TextLayout_Rec (<>) is private;
   type Tk_TextLayout is access all Tk_TextLayout_Rec;
   pragma Convention (C, Tk_TextLayout);

   type Tk_Window_Rec (<>) is private;
   type Tk_Window is access all Tk_Window_Rec;
   pragma Convention (C, Tk_Window);

   type Tk_3DBorder_Rec (<>) is private;
   type Tk_3DBorder is access all Tk_3DBorder_Rec;
   pragma Convention (C, Tk_3DBorder);

   --
   --  Additional types exported to clients.
   --

   subtype Tk_Uid is C.Strings.chars_ptr;

   --
   --  The enum below defines the valid types for Tk configuration options
   --  as implemented by Tk_InitOptions, Tk_SetOptions, etc.
   --

   type Tk_OptionType is
     (TK_OPTION_BOOLEAN,
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

   type Tk_OptionSpec_Rec is record
      typ : Tk_OptionType;
      --  Type of option, such as TK_OPTION_COLOR; see definitions
      --  above. Last option in table must have type TK_OPTION_END.
      optionName : C.Strings.chars_ptr;
      --  Name used to specify option in Tcl commands.
      dbName : C.Strings.chars_ptr;
      --  Name for option in option database.
      dbClass : C.Strings.chars_ptr;
      --  Class for option in database.
      defValue : C.Strings.chars_ptr;
      --  Default value for option if not specified in command line,
      --  the option database, or the system.
      objOffset : C.int;
      --  Where in record to store a Tcl_Obj * that holds the value of
      --  this option, specified as an offset in bytes from the start
      --  of the record. Use the Tk_Offset macro to generate values
      --  for this.  -1 means don't store the Tcl_Obj in the record.
      internalOffset : C.int;
      --  Where in record to store the internal representation of the
      --  value of this option, such as an int or XColor *.  This
      --  field is specified as an offset in bytes from the start of
      --  the record. Use the Tk_Offset macro to generate values for
      --  it.  -1 means don't store the internal representation in the
      --  record.
      flags : C.int;
      --  Any combination of the values defined below.
      data : ClientData;
      --  An alternate place to put option-specific data. Used for the
      --  monochrome default value for colors, etc.
      typeMask : C.int;
      --  An arbitrary bit mask defined by the class manager;
      --  typically bits correspond to certain kinds of options such
      --  as all those that require a redisplay when they change.
      --  Tk_SetOptions returns the bit-wise OR of the typeMasks of
      --  all options that were changed.
   end record;
   pragma Convention (C, Tk_OptionSpec_Rec);
   type Tk_OptionSpec is access all Tk_OptionSpec_Rec;
   pragma Convention (C, Tk_OptionSpec);

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

   --  Tcl_Offset is not implemented because its implementation
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
   --  So TASH doesn't make them visible.

   type Tk_SavedOption_Rec (<>) is private;
   type Tk_SavedOption is access all Tk_SavedOption_Rec;
   pragma Convention (C, Tk_SavedOption);

   type Tk_SavedOptions_Rec (<>) is private;
   type Tk_SavedOptions is access all Tk_SavedOptions_Rec;
   pragma Convention (C, Tk_SavedOptions);

   --
   --  Structure used to describe application-specific configuration
   --  options:  indicates procedures to call to parse an option and
   --  to return a text string describing an option. THESE ARE
   --  DEPRECATED; PLEASE USE THE NEW STRUCTURES LISTED ABOVE.
   --

   --
   --  This is a temporary flag used while tkObjConfig and new
   --  widgets are in development.
   --

   type Tk_OptionParseProc is access function
     (data    : ClientData;
      interp  : Tcl_Interp;
      tkwin   : Tk_Window;
      value   : C.Strings.chars_ptr;
      widgRec : C.Strings.chars_ptr;
      offset  : C.int)
   return       C.int;
   pragma Convention (C, Tk_OptionParseProc);

   type Tk_OptionPrintProc is access function
     (data        : ClientData;
      tkwin       : Tk_Window;
      widgRec     : C.Strings.chars_ptr;
      offset      : C.int;
      freeProcPtr : Tcl_FreeProc)
   return           C.Strings.chars_ptr;
   pragma Convention (C, Tk_OptionPrintProc);

   type Tk_CustomOption_Rec is record
      parseProc : Tk_OptionParseProc;
      --  Procedure to call to parse an option and store it in
      --  converted form.
      printProc : Tk_OptionPrintProc;
      --  Procedure to return a printable string describing an
      --  existing option.
      data : ClientData;
      --  Arbitrary one-word value used by option parser: passed to
      --  parseProc and printProc.
   end record;
   pragma Convention (C, Tk_CustomOption_Rec);
   type Tk_CustomOption is access all Tk_CustomOption_Rec;
   pragma Convention (C, Tk_CustomOption);

   --
   --  Structure used to specify information for Tk_ConfigureWidget.  Each
   --  structure gives complete information for one option, including
   --  how the option is specified on the command line, where it appears
   --  in the option database, etc.
   --

   --
   --  Type values for Tk_ConfigSpec structures.  See the user
   --  documentation for details.
   --
   --  Last option in table must have type TK_CONFIG_END.
   --

   type Tk_ConfigTypes is
     (TK_CONFIG_BOOLEAN,
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

   type Tk_ConfigSpec_Rec is record
      typ : Tk_ConfigTypes;
      --  Type of option, such as TK_CONFIG_COLOR; see definitions
      --  above.
      argvName : C.Strings.chars_ptr;
      --  Switch used to specify option in argv.  NULL means this spec
      --  is part of a group.
      dbName : C.Strings.chars_ptr;
      --  Name for option in option database.
      dbClass : C.Strings.chars_ptr;
      --  Class for option in database.
      defValue : C.Strings.chars_ptr;
      --  Default value for option if not specified in command line or
      --  database.
      offset : C.int;
      --  Where in widget record to store value; use Tk_Offset macro
      --  to generate values for this.
      specFlags : C.int;
      --  Any combination of the values defined below; other bits are
      --  used internally by tkConfig.c.
      customPtr : Tk_CustomOption;
      --  If type is TK_CONFIG_CUSTOM then this is a pointer to info
      --  about how to parse and print the option.  Otherwise it is
      --  irrelevant.
   end record;
   pragma Convention (C, Tk_ConfigSpec_Rec);
   type Tk_ConfigSpec is access all Tk_ConfigSpec_Rec;
   pragma Convention (C, Tk_ConfigSpec);

   type Tk_ConfigSpec_Array is
     array (CNatural range <>) of aliased Tk_ConfigSpec_Rec;

   package Tk_ConfigSpec_Pointers is new C.Pointers
     (Index => CNatural,
      Element => Tk_ConfigSpec_Rec,
      Element_Array => Tk_ConfigSpec_Array,
      Default_Terminator => (Tk_ConfigTypes'First,
                             C.Strings.Null_Ptr,
                             C.Strings.Null_Ptr,
                             C.Strings.Null_Ptr,
                             C.Strings.Null_Ptr,
                             0,
                             0,
                             null));

   subtype Tk_ConfigSpecs is Tk_ConfigSpec_Pointers.Pointer;

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

   type Tk_ArgvInfo_Rec (<>) is private;                       -- @todo
   type Tk_ArgvInfo is access all Tk_ArgvInfo_Rec;
   pragma Convention (C, Tk_ArgvInfo);

   --
   --  Legal values for the type field of a Tk_ArgvInfo: see the user
   --  documentation for details.
   --
   --  @todo: these are useless without a visible definition of the
   --  structure!

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

   type Tk_RestrictAction is
     (TK_DEFER_EVENT,
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

   type Tk_Anchor is
     (TK_ANCHOR_N,
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

   type Tk_FontMetrics_Rec is record
      ascent : C.int;
      --  The amount in pixels that the tallest letter sticks up above
      --  the baseline, plus any extra blank space added by the
      --  designer of the font.
      descent : C.int;
      --  The largest amount in pixels that any letter sticks below
      --  the baseline, plus any extra blank space added by the
      --  designer of the font.
      linespace : C.int;
      --  The sum of the ascent and descent.  How far apart two lines
      --  of text in the same font should be placed so that none of
      --  the characters in one line overlap any of the characters in
      --  the other line.
   end record;
   pragma Convention (C, Tk_FontMetrics_Rec);
   type Tk_FontMetrics is access all Tk_FontMetrics_Rec;
   pragma Convention (C, Tk_FontMetrics);

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
     (data  : ClientData;
      tkwin : Tk_Window);
   pragma Convention (C, Tk_GeomRequestProc);

   type Tk_GeomLostSlaveProc is access procedure
     (data  : ClientData;
      tkwin : Tk_Window);
   pragma Convention (C, Tk_GeomLostSlaveProc);

   type Tk_GeomMgr_Rec is record
      name : C.Strings.chars_ptr;
      --  Name of the geometry manager {command used to invoke it, or
      --  name of widget class that allows embedded widgets}.
      requestProc : Tk_GeomRequestProc;
      --  Procedure to invoke when a slave's requested geometry
      --  changes.
      lostSlaveProc : Tk_GeomLostSlaveProc;
      --  Procedure to invoke when a slave is taken away from one
      --  geometry manager by another.  NULL means geometry manager
      --  doesn't care when slaves are lost.
   end record;
   pragma Convention (C, Tk_GeomMgr_Rec);
   type Tk_GeomMgr is access all Tk_GeomMgr_Rec;
   pragma Convention (C, Tk_GeomMgr);

   --
   --  Result values returned by Tk_GetScrollInfo:
   --

   TK_SCROLL_MOVETO : constant := 1;
   TK_SCROLL_PAGES  : constant := 2;
   TK_SCROLL_UNITS  : constant := 3;
   TK_SCROLL_ERROR  : constant := 4;

   --
   -------------------------------------
   --
   --  Extensions to the X event set
   --
   -------------------------------------
   --

   MouseWheelMask   : constant := 16#10000000#;
   ActivateMask     : constant := 16#20000000#;
   VirtualEventMask : constant := 16#40000000#;

   --
   --  A virtual event shares most of its fields with the XKeyEvent
   --  and XButtonEvent structures.  99% of the time a virtual event
   --  will be an abstraction of a key or button event, so this
   --  structure provides the most information to the user.  The only
   --  difference is the changing of the detail field for a virtual
   --  event so that it holds the name of the virtual event being
   --  triggered.
   --

   type XVirtualEvent_Rec (<>) is private;
   type XVirtualEvent is access all XVirtualEvent_Rec;
   pragma Convention (C, XVirtualEvent);

   type XActivateDeactivateEvent_Rec (<>) is private;
   type XActivateDeactivateEvent is access all XActivateDeactivateEvent_Rec;
   pragma Convention (C, XActivateDeactivateEvent);

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

   function Tk_ScreenNumber (tkwin : not null Tk_Window) return C.int;
   pragma Import (C, Tk_ScreenNumber, "Tk_CallScreenNumber");

   function Tk_Depth (tkwin : not null Tk_Window) return C.int;
   pragma Import (C, Tk_Depth, "Tk_CallDepth");

   function Tk_PathName
     (tkwin : not null Tk_Window) return C.Strings.chars_ptr;

   function Tk_Name (tkwin : not null Tk_Window) return Tk_Uid;
   pragma Import (C, Tk_Name, "Tk_CallName");

   function Tk_Class (tkwin : not null Tk_Window) return Tk_Uid;
   pragma Import (C, Tk_Class, "Tk_CallClass");

   function Tk_Parent (tkwin : not null Tk_Window) return Tk_Window;
   pragma Import (C, Tk_Parent, "Tk_CallParent");

   --
   --  The structure below is needed by the macros above so that they
   --  can access the fields of a Tk_Window.  The fields not needed by
   --  the macros are declared as "dummyX".  The structure has its own
   --  type in order to prevent applications from accessing Tk_Window
   --  fields except using official macros.  WARNING!! The structure
   --  definition must be kept consistent with the TkWindow structure
   --  in tkInt.h.  If you change one, then change the other.  See the
   --  declaration in tkInt.h for documentation on what the fields are
   --  used for internally.
   --
   --  TASH: this is retained but probably useless.

   type Tk_FakeWin_Rec (<>) is private;
   type Tk_FakeWin is access all Tk_FakeWin_Rec;
   pragma Convention (C, Tk_FakeWin);

   --
   --  Flag values for TkWindow {and Tk_FakeWin} structures are:
   --
   --  TK_MAPPED:               1 means window is currently mapped,
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
   --  TK_EMBEDDED:             1 means that this window {which must be a
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
   --  TK_WRAPPER:              1 means that this window is the extra
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

   type Tk_State is
     (TK_STATE_NULL,
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

   type Tk_SmoothMethod_Rec is record
      name : C.Strings.chars_ptr;
      coordProc : System.Address;       --  @todo make exact type
      postscriptProc : System.Address;  --  @todo make exact type
   end record;
   pragma Convention (C, Tk_SmoothMethod_Rec);
   type Tk_SmoothMethod is access all Tk_SmoothMethod_Rec;
   pragma Convention (C, Tk_SmoothMethod);

   --
   --  For each item in a canvas widget there exists one record with
   --  the following structure.  Each actual item is represented by
   --  a record with the following stuff at its beginning, plus additional
   --  type-specific stuff after that.
   --

   TK_TAG_SPACE : constant := 3;
   --  auxiliary type for Ada
   type StaticTagSpace_Type is array (0 .. TK_TAG_SPACE - 1) of Tk_Uid;
   pragma Convention (C, StaticTagSpace_Type);

   type Tk_Item_Rec;
   type Tk_Item is access all Tk_Item_Rec;
   pragma Convention (C, Tk_Item);

   type Tk_ItemType_Rec;
   type Tk_ItemType is access all Tk_ItemType_Rec;
   pragma Convention (C, Tk_ItemType);

   type Tk_Item_Rec is record
      id : C.int;
      --  Unique identifier for this item (also serves as first tag
      --  for item).
      nextPtr : Tk_Item;
      --  Next in display list of all items in this canvas. Later
      --  items in list are drawn on top of earlier ones.
      staticTagSpace : StaticTagSpace_Type;
      --  Built-in space for limited # of tags.
      tagPtr : System.Address;  -- @todo should be access Tk_Uid
      --  Pointer to array of tags. Usually points to staticTagSpace,
      --  but may point to malloc-ed space if there are lots of tags.
      tagSpace : C.int;
      --  Total amount of tag space available at tagPtr.
      numTags : C.int;
      --  Number of tag slots actually used at *tagPtr.
      typePtr : Tk_ItemType;
      --  Table of procedures that implement this type of item.
      x1, y1, x2, y2 : C.int;
      --  Bounding box for item, in integer canvas units.  Set by
      --  item-specific code and guaranteed to contain every pixel
      --  drawn in item.
      --  Item area includes x1 and y1 but not x2 and y2.
      prevPtr : Tk_Item;
      --  Previous in display list of all items in this canvas.  Later
      --  items in list are drawn just below earlier ones.
      state : Tk_State;
      --  State of item.
      reserved1 : C.Strings.chars_ptr;
      --  reserved for future use
      redraw_flags : C.int;
      --  Some flags used in the canvas

      ----------------------------------------------------------------------
      --  Starting here is additional type-specific stuff; see the
      --  declarations for individual types to see what is part of each type.
      --  The actual space below is determined by the "itemInfoSize" of the
      --  type's Tk_ItemType record.
      ----------------------------------------------------------------------

   end record;
   pragma Convention (C, Tk_Item_Rec);

   type Tk_ItemCreateProc is access function
     (interp  : Tcl_Interp;
      canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      argc    : C.int;
      objv    : Tcl_Obj_Array)
   return       C.int;
   pragma Convention (C, Tk_ItemCreateProc);

   type Tk_ItemConfigureProc is access function
     (interp  : Tcl_Interp;
      canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      argc    : C.int;
      objv    : Tcl_Obj_Array;
      flags   : C.int)
   return       C.int;
   pragma Convention (C, Tk_ItemConfigureProc);

   type Tk_ItemCoordProc is access function
     (interp  : Tcl_Interp;
      canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      argc    : C.int;
      argv    : Tcl_Obj)
   return       C.int;
   pragma Convention (C, Tk_ItemCoordProc);

   type Tk_ItemDeleteProc is access procedure
     (canvas  : Tk_Canvas;
      itemPtr : access Tk_Item;
      display : System.Address);  --  @todo should be Display* (Xlib)
   pragma Convention (C, Tk_ItemDeleteProc);

   type Tk_ItemDisplayProc is access procedure
     (canvas  : Tk_Canvas;
      itemPtr : access Tk_Item;
      display : System.Address;   --  @todo should be Display* (Xlib)
      dst     : C.int;            --  @todo should be Drawable (Xlib)
      x, y, width, height : C.int);
   pragma Convention (C, Tk_ItemDisplayProc);

   type Tk_ItemPointProc is access function
     (canvas   : Tk_Canvas;
      itemPtr  : Tk_Item;
      pointPtr : access C.double)
   return        C.double;
   pragma Convention (C, Tk_ItemPointProc);

   type Tk_ItemAreaProc is access function
     (canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      rectPtr : access C.double)
   return       C.int;
   pragma Convention (C, Tk_ItemAreaProc);

   type Tk_ItemPostscriptProc is access function
     (interp  : Tcl_Interp;
      canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      prepass : C.int)
   return       C.int;
   pragma Convention (C, Tk_ItemPostscriptProc);

   type Tk_ItemScaleProc is access procedure
     (canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      originX : C.double;
      originY : C.double;
      scaleX  : C.double;
      scaleY  : C.double);
   pragma Convention (C, Tk_ItemScaleProc);

   type Tk_ItemTranslateProc is access procedure
     (canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      deltaX  : C.double;
      deltaY  : C.double);
   pragma Convention (C, Tk_ItemTranslateProc);

   type Tk_ItemIndexProc is access function
     (interp      : Tcl_Interp;
      canvas      : Tk_Canvas;
      itemPtr     : Tk_Item;
      indexString : C.Strings.chars_ptr;
      indexPtr    : access C.int)
   return           C.int;
   pragma Convention (C, Tk_ItemIndexProc);

   type Tk_ItemCursorProc is access procedure
     (canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      index   : C.int);
   pragma Convention (C, Tk_ItemCursorProc);

   type Tk_ItemSelectionProc is access function
     (canvas   : Tk_Canvas;
      itemPtr  : Tk_Item;
      offset   : C.int;
      buffer   : C.Strings.chars_ptr;
      maxBytes : C.int)
   return        C.int;
   pragma Convention (C, Tk_ItemSelectionProc);

   type Tk_ItemInsertProc is access procedure
     (canvas     : Tk_Canvas;
      itemPtr    : Tk_Item;
      beforeThis : C.int;
      strng      : C.Strings.chars_ptr);
   pragma Convention (C, Tk_ItemInsertProc);

   type Tk_ItemDCharsProc is access procedure
     (canvas  : Tk_Canvas;
      itemPtr : Tk_Item;
      first   : C.int;
      last    : C.int);
   pragma Convention (C, Tk_ItemDCharsProc);

   type Tk_ItemType_Rec is record
      name : C.Strings.chars_ptr;
      --  The name of this type of item, such as "line".
      itemSize : C.int;
      --  Total amount of space needed for item's record.
      createProc : Tk_ItemCreateProc;
      --  Procedure to create a new item of this type.
      configSpecs : Tk_ConfigSpecs;
      --  Pointer to array of configuration specs for this type.
      --  Used for returning configuration info.
      configProc : Tk_ItemConfigureProc;
      --  Procedure to call to change configuration options.
      coordProc : Tk_ItemCoordProc;
      --  Procedure to call to get and set the item's coordinates.
      deleteProc : Tk_ItemDeleteProc;
      --  Procedure to delete existing item of this type.
      displayProc : Tk_ItemDisplayProc;
      --  Procedure to display items of this type.
      alwaysRedraw : C.int;
      --  Non-zero means displayProc should be called even when the
      --  item has been moved off-screen.
      pointProc : Tk_ItemPointProc;
      --  Computes distance from item to a given point.
      areaProc : Tk_ItemAreaProc;
      --  Computes whether item is inside, outside, or overlapping an
      --  area.
      postscriptProc : Tk_ItemPostscriptProc;
      --  Procedure to write a Postscript description for items of
      --  this type.
      scaleProc : Tk_ItemScaleProc;
      --  Procedure to rescale items of this type.
      indexProc : Tk_ItemIndexProc;
      --  Procedure to translate items of this type.
      icursorProc : Tk_ItemCursorProc;
      --  Procedure to set insert cursor posn to just before a given
      --  position.
      selectionProc : Tk_ItemSelectionProc;
      --  Procedure to return selection (in STRING format) when it is
      --  in this item.
      insertProc : Tk_ItemInsertProc;
      --  Procedure to insert something into an item.
      dCharsProc : Tk_ItemDCharsProc;
      --  Procedure to delete characters from an item.
      nextPtr : Tk_ItemType;
      --  Used to link types together into a list.
      reserved1 : C.Strings.chars_ptr;
      --  Reserved for future extension.
      reserved2 : C.int;                --  Carefully compatible with
      reserved3 : C.Strings.chars_ptr;  --  Jan Nijtmans dash patch
      reserved4 : C.Strings.chars_ptr;
   end record;
   pragma Convention (C, Tk_ItemType_Rec);

   --
   --  Flag bits for canvases {redraw_flags}:
   --
   --  TK_ITEM_STATE_DEPENDANT - 1 means that object needs to be
   --                            redrawn if the canvas state changes.
   --  TK_ITEM_DONT_REDRAW -     1 means that the object redraw is already
   --                            been prepared, so the general canvas code
   --                            doesn't need to do that any more.
   --

   TK_ITEM_STATE_DEPENDANT : constant := 1;
   TK_ITEM_DONT_REDRAW     : constant := 2;

   --
   --  Records of the following type (XXX) are used to describe a type of
   --  item {e.g.  lines, circles, etc.} that can form part of a
   --  canvas widget.
   --

   --
   --  The following structure provides information about the selection and
   --  the insertion cursor.  It is needed by only a few items, such as
   --  those that display text.  It is shared by the generic canvas code
   --  and the item-specific code, but most of the fields should be written
   --  only by the canvas generic code.
   --

   type Tk_CanvasTextInfo_Rec is record
      selBorder : Tk_3DBorder;
      --  Border and background for selected characters.
      --  Read-only to items.
      selBorderWidth : C.int;
      --  Width of border around selection.
      --  Read-only to items.
      selFgColorPtr : System.Address;   -- @todo should be XColor* (Xlib)
      --  Foreground color for selected text.
      --  Read-only to items.
      selItemPtr : Tk_Item;
      --  Pointer to selected item.
      --  NULL means selection isn't in this canvas.
      --  Writable by items.
      selectFirst : C.int;
      --  Character index of first selected character.
      --  Writable by items.
      selectLast : C.int;
      --  Character index of last selected character.
      --  Writable by items.
      anchorItemPtr : Tk_Item;
      --  Item corresponding to "selectAnchor": not necessarily selItemPtr.
      --  Read-only to items.
      selectAnchor : C.int;
      --  Character index of fixed end of selection (i.e. "select to"
      --  operation will use this as one end of the selection).
      --  Writable by items.
      insertBorder : Tk_3DBorder;
      --  Used to draw vertical bar for insertion cursor.
      --  Read-only to items.
      insertWidth : C.int;
      --  Total width of insertion cursor. Read-only to items.
      insertBorderWidth : C.int;
      --  Width of 3-D border around insert cursor.
      --  Read-only to items.
      focusItemPtr : Tk_Item;
      --  Item that currently has the input focus, or NULL if no such item.
      --  Read-only to items.
      gotFocus : C.int;
      --  Non-zero means that the canvas widget has the input focus.
      --  Read-only to items.
      cursorOn : C.int;
      --  Non-zero means that an insertion cursor should be displayed in
      --  focusItemPtr. Read-only to items.
   end record;
   pragma Convention (C, Tk_CanvasTextInfo_Rec);
   type Tk_CanvasTextInfo is access all Tk_CanvasTextInfo_Rec;
   pragma Convention (C, Tk_CanvasTextInfo);

   --
   --  Structures used for Dashing and Outline.
   --

   Sizeof_Char_Star : constant C.size_t
     := C.Strings.chars_ptr'Size / C.char'Size;

   type Tk_Dash_Rec is record
      number : C.int;
      key    : C.char_array (0 .. Sizeof_Char_Star - 1);
   end record;
   pragma Convention (C, Tk_Dash_Rec);
   type Tk_Dash is access all Tk_Dash_Rec;
   pragma Convention (C, Tk_Dash);

   type Tk_TSOffset_Rec is record
      flags : C.int;     --  flags; see tk.h for possible values
      xoffset : C.int;   --  x offset
      yoffset : C.int;   --  y offset
   end record;
   pragma Convention (C, Tk_TSOffset_Rec);
   type Tk_TSOffset is access all Tk_TSOffset_Rec;
   pragma Convention (C, Tk_TSOffset);

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

   type Tk_Outline_Rec (<>) is private;
   type Tk_Outline is access all Tk_Outline_Rec;
   pragma Convention (C, Tk_Outline);

   --
   -- --------------------------------------------------------------
   --
   --  Procedure prototypes and structures used for managing images:
   --
   -- --------------------------------------------------------------
   --

   type Tk_ImageType;

   type Tk_ImageCreateProc is access function
     (interp        : Tcl_Interp;
      name          : C.Strings.chars_ptr;
      objc          : C.int;
      objv          : Tcl_Obj_Array;
      typePtr       : Tk_ImageType;
      master        : Tk_ImageMaster;
      masterdataptr : ClientData)
   return             C.int;
   pragma Convention (C, Tk_ImageCreateProc);

   type Tk_ImageGetProc is access function
     (tkwin      : Tk_Window;
      masterdata : ClientData)
   return          ClientData;
   pragma Convention (C, Tk_ImageGetProc);

   type Tk_ImageDeleteProc is access procedure (masterdata : ClientData);
   pragma Convention (C, Tk_ImageDeleteProc);

   type Tk_ImageChangedProc is access procedure
     (data        : ClientData;
      x           : C.int;
      y           : C.int;
      width       : C.int;
      height      : C.int;
      imageWidth  : C.int;
      imageHeight : C.int);
   pragma Convention (C, Tk_ImageChangedProc);

   type Tk_ImagePostscriptProc is access function
     (data    : ClientData;
      interp  : Tcl_Interp;
      tkwin   : Tk_Window;
      psinfo  : Tk_PostscriptInfo;
      x       : C.int;
      y       : C.int;
      width   : C.int;
      height  : C.int;
      prepass : C.int)
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

   type Tk_ImageType_Rec (<>) is private;                  -- @todo
   type Tk_ImageType is access all Tk_ImageType_Rec;
   pragma Convention (C, Tk_ImageType);

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

   type Tk_PhotoHandle_Rec (<>) is private;                  -- @todo
   type Tk_PhotoHandle is access all Tk_PhotoHandle_Rec;
   pragma Convention (C, Tk_PhotoHandle);

   --
   --  The following structure describes a block of pixels in memory:
   --

   type Tk_PhotoImageBlock_Rec is record
      pixelPtr : C.Strings.chars_ptr;
      --  Pointer to the first pixel.
      width : C.int;
      --  Width of block, in pixels.
      height : C.int;
      --  Height of block, in pixels.
      pitch : C.int;
      --  Address difference between corresponding pixels in
      --  successive lines.
      pixelSize : C.int;
      --  Address difference between successive pixels in the same
      --  line.
      offset : CHelper.Int_Array (0 .. 3);
      --  Address differences between the red, green, blue and alpha
      --  components of the pixel and the pixel as a whole.
   end record;
   pragma Convention (C, Tk_PhotoImageBlock_Rec);
   type Tk_PhotoImageBlock is access all Tk_PhotoImageBlock_Rec;
   pragma Convention (C, Tk_PhotoImageBlock);

   --
   --  Procedure prototypes and structures used in reading and
   --  writing photo images:
   --

   --  @todo -- I'm not convinced that anyone actually needs PhotoImage
   --  features!

   type Tk_PhotoImageFormat_Rec (<>) is private;               -- @todo
   type Tk_PhotoImageFormat is access all Tk_PhotoImageFormat_Rec;
   pragma Convention (C, Tk_PhotoImageFormat);

   type Tk_ImageFileMatchProc is access function
     (chan      : Tcl_Channel;
      fileName  : C.Strings.chars_ptr;
      format    : Tcl_Obj;
      widthPtr  : access C.int;
      heightPtr : access C.int;
      interp    : Tcl_Interp)
   return         C.int;
   pragma Convention (C, Tk_ImageFileMatchProc);

   type Tk_ImageStringMatchProc is access function
     (dataObj   : Tcl_Obj;
      format    : Tcl_Obj;
      widthPtr  : access C.int;
      heightPtr : access C.int;
      interp    : Tcl_Interp)
   return         C.int;
   pragma Convention (C, Tk_ImageStringMatchProc);

   type Tk_ImageFileReadProc is access function
     (interp      : Tcl_Interp;
      chan        : Tcl_Channel;
      fileName    : C.Strings.chars_ptr;
      format      : Tcl_Obj;
      imageHandle : Tk_PhotoHandle;
      destX       : C.int;
      destY       : C.int;
      width       : C.int;
      height      : C.int;
      srcX        : C.int;
      srcY        : C.int)
   return           C.int;
   pragma Convention (C, Tk_ImageFileReadProc);

   type Tk_ImageStringReadProc is access function
     (interp      : Tcl_Interp;
      dataObj     : Tcl_Obj;
      format      : Tcl_Obj;
      imageHandle : Tk_PhotoHandle;
      destX       : C.int;
      destY       : C.int;
      width       : C.int;
      height      : C.int;
      srcX        : C.int;
      srcY        : C.int)
   return           C.int;
   pragma Convention (C, Tk_ImageStringReadProc);

   type Tk_ImageFileWriteProc is access function
     (interp   : Tcl_Interp;
      fileName : C.Strings.chars_ptr;
      format   : Tcl_Obj;
      blockPtr : Tk_PhotoImageBlock)
   return        C.int;
   pragma Convention (C, Tk_ImageFileWriteProc);

   type Tk_ImageStringWriteProc is access function
     (interp   : Tcl_Interp;
      format   : Tcl_Obj;
      blockPtr : Tk_PhotoImageBlock)
   return        C.int;
   pragma Convention (C, Tk_ImageStringWriteProc);

   --
   --  The following structure represents a particular file format for
   --  storing images {e.g., PPM, GIF, JPEG, etc.}.  It provides information
   --  to allow image files of that format to be recognized and read into
   --  a photo image.
   --

   procedure Tk_CreateOldImageType (typePtr : not null Tk_ImageType);
   pragma Import (C, Tk_CreateOldImageType, "Tk_CreateOldImageType");

   procedure Tk_CreateOldPhotoImageFormat
     (formatPtr : not null Tk_PhotoImageFormat);
   pragma Import
     (C, Tk_CreateOldPhotoImageFormat, "Tk_CreateOldPhotoImageFormat");

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
     (argc : C.int;
      argv : CArgv.Chars_Ptr_Ptr;
      proc : not null Tcl_AppInitProc);

   procedure Tk_InitImageArgs
     (interp : not null Tcl_Interp;
      argc   : C.int;
      argv   : CArgv.Chars_Ptr_Ptr);
   pragma Import (C, Tk_InitImageArgs, "Tk_InitImageArgs");

   --
   -- --------------------------------------------------------------
   --
   --  Additional procedure types defined by Tk.
   --
   -- --------------------------------------------------------------
   --

   type Tk_GetSelProc is access function
     (data    : ClientData;
      interp  : Tcl_Interp;
      portion : C.Strings.chars_ptr)
   return       C.int;
   pragma Convention (C, Tk_GetSelProc);

   type Tk_LostSelProc is access procedure (data : ClientData);
   pragma Convention (C, Tk_LostSelProc);

   type Tk_SelectionProc is access function
     (data     : ClientData;
      offset   : C.int;
      buffer   : C.Strings.chars_ptr;
      maxBytes : C.int)
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
     (tkwin    : not null Tk_Window;
      name     : C.Strings.chars_ptr;
      value    : C.Strings.chars_ptr;
      priority : C.int);
   pragma Import (C, Tk_AddOption, "Tk_AddOption");

   --  6

   --  7

   procedure Tk_CanvasDrawableCoords
     (canvas       : not null Tk_Canvas;
      x            : C.double;
      y            : C.double;
      drawableXPtr : access C.short;
      drawableYPtr : access C.short);
   pragma Import (C, Tk_CanvasDrawableCoords, "Tk_CanvasDrawableCoords");

   --  8

   procedure Tk_CanvasEventuallyRedraw
     (canvas : not null Tk_Canvas;
      x1     : C.int;
      y1     : C.int;
      x2     : C.int;
      y2     : C.int);
   pragma Import (C, Tk_CanvasEventuallyRedraw, "Tk_CanvasEventuallyRedraw");

   --  9

   function Tk_CanvasGetCoord
     (interp    : not null Tcl_Interp;
      canvas    : not null Tk_Canvas;
      str       : C.Strings.chars_ptr;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_CanvasGetCoord, "Tk_CanvasGetCoord");

   --  10

   function Tk_CanvasGetTextInfo
     (canvas : not null Tk_Canvas)
      return   Tk_CanvasTextInfo;
   pragma Import (C, Tk_CanvasGetTextInfo, "Tk_CanvasGetTextInfo");

   --  11

   --  12

   --  13

   function Tk_CanvasPsFont
     (interp : not null Tcl_Interp;
      canvas : not null Tk_Canvas;
      font   : not null Tk_Font)
      return   C.int;
   pragma Import (C, Tk_CanvasPsFont, "Tk_CanvasPsFont");

   --  14

   procedure Tk_CanvasPsPath
     (interp    : not null Tcl_Interp;
      canvas    : not null Tk_Canvas;
      coordPtr  : access C.double;
      numPoints : C.int);
   pragma Import (C, Tk_CanvasPsPath, "Tk_CanvasPsPath");

   --  15

   --  16

   function Tk_CanvasPsY
     (canvas : not null Tk_Canvas;
      y      : C.double)
      return   C.double;
   pragma Import (C, Tk_CanvasPsY, "Tk_CanvasPsY");

   --  17

   --  18

   function Tk_CanvasTagsParseProc
     (data    : ClientData;
      interp  : not null Tcl_Interp;
      tkwin   : not null Tk_Window;
      value   : C.Strings.chars_ptr;
      widgRec : C.Strings.chars_ptr;
      offset  : C.int)
      return    C.int;
   pragma Import (C, Tk_CanvasTagsParseProc, "Tk_CanvasTagsParseProc");

   --  19

   function Tk_CanvasTagsPrintProc
     (data        : ClientData;
      tkwin       : not null Tk_Window;
      widgRec     : C.Strings.chars_ptr;
      offset      : C.int;
      freeProcPtr : Tcl_FreeProc)          -- XXX
      return        C.Strings.chars_ptr;
   pragma Import (C, Tk_CanvasTagsPrintProc, "Tk_CanvasTagsPrintProc");

   --  20

   function Tk_CanvasTkwin (canvas : not null Tk_Canvas) return Tk_Window;
   pragma Import (C, Tk_CanvasTkwin, "Tk_CanvasTkwin");

   --  21

   procedure Tk_CanvasWindowCoords
     (canvas     : not null Tk_Canvas;
      x          : C.double;
      y          : C.double;
      screenXPtr : access C.short;
      screenYPtr : access C.short);
   pragma Import (C, Tk_CanvasWindowCoords, "Tk_CanvasWindowCoords");

   --  22

   --  23

   function Tk_CharBbox
     (layout    : not null Tk_TextLayout;
      index     : C.int;
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
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window)
      return   C.int;
   pragma Import (C, Tk_ClipboardClear, "Tk_ClipboardClear");

   --  27

   function Tk_ConfigureInfo
     (interp   : not null Tcl_Interp;
      tkwin    : not null Tk_Window;
      specs    : not null Tk_ConfigSpec;
      widgRec  : C.Strings.chars_ptr;
      argvName : C.Strings.chars_ptr;
      flags    : C.int)
      return     C.int;
   pragma Import (C, Tk_ConfigureInfo, "Tk_ConfigureInfo");

   --  28

   function Tk_ConfigureValue
     (interp   : not null Tcl_Interp;
      tkwin    : not null Tk_Window;
      specs    : not null Tk_ConfigSpec;
      widgRec  : C.Strings.chars_ptr;
      argvName : C.Strings.chars_ptr;
      flags    : C.int)
      return     C.int;
   pragma Import (C, Tk_ConfigureValue, "Tk_ConfigureValue");

   --  29

   function Tk_ConfigureWidget
     (interp  : not null Tcl_Interp;
      tkwin   : not null Tk_Window;
      specs   : not null Tk_ConfigSpec;
      argc    : C.int;
      argv    : CArgv.Chars_Ptr_Ptr;
      widgRec : C.Strings.chars_ptr;
      flags   : C.int)
      return    C.int;
   pragma Import (C, Tk_ConfigureWidget, "Tk_ConfigureWidget");

   --  30

   --  31

   function Tk_ComputeTextLayout
     (font       : not null Tk_Font;
      str        : C.Strings.chars_ptr;
      numChars   : C.int;
      wrapLength : C.int;
      justify    : Tk_Justify;
      flags      : C.int;
      widthPtr   : access C.int;
      heightPtr  : access C.int)
      return       Tk_TextLayout;
   pragma Import (C, Tk_ComputeTextLayout, "Tk_ComputeTextLayout");

   --  32

   function Tk_CoordsToWindow
     (rootX : C.int;
      rootY : C.int;
      tkwin : not null Tk_Window)
      return  Tk_Window;
   pragma Import (C, Tk_CoordsToWindow, "Tk_CoordsToWindow");

   --  33

   function Tk_CreateBinding
     (interp       : not null Tcl_Interp;
      bindingTable : not null Tk_BindingTable;
      object       : ClientData;
      eventStr     : C.Strings.chars_ptr;
      command      : C.Strings.chars_ptr;
      append       : C.int)
      return         C.unsigned_long;
   pragma Import (C, Tk_CreateBinding, "Tk_CreateBinding");

   --  34

   function Tk_CreateBindingTable
     (interp : not null Tcl_Interp)
      return   Tk_BindingTable;
   pragma Import (C, Tk_CreateBindingTable, "Tk_CreateBindingTable");

   --  35

   --  36

   --  37

   --  38

   procedure Tk_CreateImageType (typePtr : not null Tk_ImageType);
   pragma Import (C, Tk_CreateImageType, "Tk_CreateImageType");

   --  39

   procedure Tk_CreateItemType (typePtr : not null Tk_ItemType);
   pragma Import (C, Tk_CreateItemType, "Tk_CreateItemType");

   --  40

   procedure Tk_CreatePhotoImageFormat
     (formatPtr : not null Tk_PhotoImageFormat);
   pragma Import (C, Tk_CreatePhotoImageFormat, "Tk_CreatePhotoImageFormat");

   --  41

   --  42

   function Tk_CreateWindow
     (interp     : not null Tcl_Interp;
      parent     : not null Tk_Window;
      name       : C.Strings.chars_ptr;
      screenName : C.Strings.chars_ptr)
      return       Tk_Window;
   pragma Import (C, Tk_CreateWindow, "Tk_CreateWindow");

   --  43

   function Tk_CreateWindowFromPath
     (interp     : not null Tcl_Interp;
      tkwin      : not null Tk_Window;
      pathName   : C.Strings.chars_ptr;
      screenName : C.Strings.chars_ptr)
      return       Tk_Window;
   pragma Import (C, Tk_CreateWindowFromPath, "Tk_CreateWindowFromPath");

   --  44

   function Tk_DefineBitmap
     (interp : not null Tcl_Interp;
      name   : C.Strings.chars_ptr;
      source : C.Strings.chars_ptr;
      width  : C.int;
      height : C.int)
      return   C.int;
   pragma Import (C, Tk_DefineBitmap, "Tk_DefineBitmap");

   --  45

   procedure Tk_DefineCursor (window : not null Tk_Window;
                              cursor : not null Tk_Cursor);
   pragma Import (C, Tk_DefineCursor, "Tk_DefineCursor");

   --  46

   procedure Tk_DeleteAllBindings
     (bindingTable : not null Tk_BindingTable;
      object       : ClientData);
   pragma Import (C, Tk_DeleteAllBindings, "Tk_DeleteAllBindings");

   --  47

   function Tk_DeleteBinding
     (interp       : not null Tcl_Interp;
      bindingTable : not null Tk_BindingTable;
      object       : ClientData;
      eventStr     : C.Strings.chars_ptr)
      return         C.int;
   pragma Import (C, Tk_DeleteBinding, "Tk_DeleteBinding");

   --  48

   procedure Tk_DeleteBindingTable (bindingTable : not null Tk_BindingTable);
   pragma Import (C, Tk_DeleteBindingTable, "Tk_DeleteBindingTable");

   --  49

   procedure Tk_DeleteErrorHandler (handler : not null Tk_ErrorHandler);
   pragma Import (C, Tk_DeleteErrorHandler, "Tk_DeleteErrorHandler");

   --  50

   --  51

   --  52

   procedure Tk_DeleteImage
     (interp : not null Tcl_Interp;
      name   : C.Strings.chars_ptr);
   pragma Import (C, Tk_DeleteImage, "Tk_DeleteImage");

   --  53

   --  54

   procedure Tk_DestroyWindow (tkwin : not null Tk_Window);
   pragma Import (C, Tk_DestroyWindow, "Tk_DestroyWindow");

   --  55

   function Tk_DisplayName
     (tkwin : not null Tk_Window)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tk_DisplayName, "Tk_DisplayName");

   --  56

   function Tk_DistanceToTextLayout
     (layout : not null Tk_TextLayout;
      x      : C.int;
      y      : C.int)
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
     (interp    : not null Tcl_Interp;
      imageName : C.Strings.chars_ptr)
      return      Tk_PhotoHandle;
   pragma Import (C, Tk_FindPhoto, "Tk_FindPhoto");

   --  65

   --  66

   procedure Tk_Free3DBorder (border : not null Tk_3DBorder);
   pragma Import (C, Tk_Free3DBorder, "Tk_Free3DBorder");

   --  67

   --  68

   --  69

   --  70

   --  71

   procedure Tk_FreeFont (f : not null Tk_Font);
   pragma Import (C, Tk_FreeFont, "Tk_FreeFont");

   --  72

   --  73

   procedure Tk_FreeImage (image : not null Tk_Image);
   pragma Import (C, Tk_FreeImage, "Tk_FreeImage");

   --  74

   --  75

   --  76

   procedure Tk_FreeTextLayout (textLayout : not null Tk_TextLayout);
   pragma Import (C, Tk_FreeTextLayout, "Tk_FreeTextLayout");

   --  77

   --  78

   --  79

   procedure Tk_GeometryRequest
     (tkwin     : not null Tk_Window;
      reqWidth  : C.int;
      reqHeight : C.int);
   pragma Import (C, Tk_GeometryRequest, "Tk_GeometryRequest");

   --  80

   function Tk_Get3DBorder
     (interp    : not null Tcl_Interp;
      tkwin     : not null Tk_Window;
      colorName : Tk_Uid)
      return      Tk_3DBorder;
   pragma Import (C, Tk_Get3DBorder, "Tk_Get3DBorder");

   --  81

   procedure Tk_GetAllBindings
     (interp       : not null Tcl_Interp;
      bindingTable : not null Tk_BindingTable;
      object       : ClientData);
   pragma Import (C, Tk_GetAllBindings, "Tk_GetAllBindings");

   --  82

   function Tk_GetAnchor
     (interp    : not null Tcl_Interp;
      str       : C.Strings.chars_ptr;
      anchorPtr : Tk_Anchor)
      return      C.int;
   pragma Import (C, Tk_GetAnchor, "Tk_GetAnchor");

   --  83

   --  84

   function Tk_GetBinding
     (interp       : not null Tcl_Interp;
      bindingTable : not null Tk_BindingTable;
      object       : ClientData;
      eventStr     : C.Strings.chars_ptr)
      return         C.Strings.chars_ptr;
   pragma Import (C, Tk_GetBinding, "Tk_GetBinding");

   --  85

   --  86

   --  87

   function Tk_GetCapStyle
     (interp : not null Tcl_Interp;
      str    : C.Strings.chars_ptr;
      capPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetCapStyle, "Tk_GetCapStyle");

   --  88

   --  89

   --  90

   --  91

   function Tk_GetCursor
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      str    : Tk_Uid)
      return   Tk_Cursor;
   pragma Import (C, Tk_GetCursor, "Tk_GetCursor");

   --  92

   function Tk_GetCursorFromData
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      source : C.Strings.chars_ptr;
      mask   : C.Strings.chars_ptr;
      width  : C.int;
      height : C.int;
      xHot   : C.int;
      yHot   : C.int;
      fg     : Tk_Uid;
      bg     : Tk_Uid)
      return   Tk_Cursor;
   pragma Import (C, Tk_GetCursorFromData, "Tk_GetCursorFromData");

   --  93

   function Tk_GetFont
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      str    : C.Strings.chars_ptr)
      return   Tk_Font;
   pragma Import (C, Tk_GetFont, "Tk_GetFont");

   --  94

   function Tk_GetFontFromObj
     (tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj)
      return   Tk_Font;
   pragma Import (C, Tk_GetFontFromObj, "Tk_GetFontFromObj");

   --  95

   procedure Tk_GetFontMetrics
     (font  : not null Tk_Font;
      fmPtr : not null Tk_FontMetrics);
   pragma Import (C, Tk_GetFontMetrics, "Tk_GetFontMetrics");

   --  96

   --  97

   function Tk_GetImage
     (interp     : not null Tcl_Interp;
      tkwin      : not null Tk_Window;
      name       : C.Strings.chars_ptr;
      changeProc : not null Tk_ImageChangedProc;
      data       : ClientData)
      return       Tk_Image;
   pragma Import (C, Tk_GetImage, "Tk_GetImage");

   --  98

   function Tk_GetImageMasterData
     (interp     : not null Tcl_Interp;
      name       : C.Strings.chars_ptr;
      typePtrPtr : not null Tk_ImageType)
      return       ClientData;
   pragma Import (C, Tk_GetImageMasterData, "Tk_GetImageMasterData");

   --  99

   function Tk_GetItemTypes return Tk_ItemType;
   pragma Import (C, Tk_GetItemTypes, "Tk_GetItemTypes");

   --  100

   function Tk_GetJoinStyle
     (interp  : not null Tcl_Interp;
      str     : C.Strings.chars_ptr;
      joinPtr : access C.int)
      return    C.int;
   pragma Import (C, Tk_GetJoinStyle, "Tk_GetJoinStyle");

   --  101

   function Tk_GetJustify
     (interp     : not null Tcl_Interp;
      str        : C.Strings.chars_ptr;
      justifyPtr : Tk_Justify)
      return       C.int;
   pragma Import (C, Tk_GetJustify, "Tk_GetJustify");

   --  102

   function Tk_GetNumMainWindows return C.int;
   pragma Import (C, Tk_GetNumMainWindows, "Tk_GetNumMainWindows");

   --  103

   function Tk_GetOption
     (tkwin     : not null Tk_Window;
      name      : C.Strings.chars_ptr;
      className : C.Strings.chars_ptr)
      return      Tk_Uid;
   pragma Import (C, Tk_GetOption, "Tk_GetOption");

   --  104

   function Tk_GetPixels
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      str    : C.Strings.chars_ptr;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetPixels, "Tk_GetPixels");

   --  105

   --  106

   function Tk_GetRelief
     (interp    : not null Tcl_Interp;
      name      : C.Strings.chars_ptr;
      reliefPtr : access C.int)
      return      C.int;
   pragma Import (C, Tk_GetRelief, "Tk_GetRelief");

   --  107

   procedure Tk_GetRootCoords
     (tkwin : not null Tk_Window;
      xPtr  : access C.int;
      yPtr  : access C.int);
   pragma Import (C, Tk_GetRootCoords, "Tk_GetRootCoords");

   --  108

   function Tk_GetScrollInfo
     (interp : not null Tcl_Interp;
      argc   : C.int;
      argv   : CArgv.Chars_Ptr_Ptr;
      dblPtr : access C.double;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetScrollInfo, "Tk_GetScrollInfo");

   --  109

   function Tk_GetScreenMM
     (interp    : not null Tcl_Interp;
      tkwin     : not null Tk_Window;
      str       : C.Strings.chars_ptr;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_GetScreenMM, "Tk_GetScreenMM");

   --  110

   --  111

   function Tk_GetUid (str : C.Strings.chars_ptr) return Tk_Uid;
   pragma Import (C, Tk_GetUid, "Tk_GetUid");

   --  112

   --  113

   procedure Tk_GetVRootGeometry
     (tkwin     : not null Tk_Window;
      xPtr      : access C.int;
      yPtr      : access C.int;
      widthPtr  : access C.int;
      heightPtr : access C.int);
   pragma Import (C, Tk_GetVRootGeometry, "Tk_GetVRootGeometry");

   --  114

   function Tk_Grab
     (interp     : not null Tcl_Interp;
      tkwin      : not null Tk_Window;
      grabGlobal : C.int)
      return       C.int;
   pragma Import (C, Tk_Grab, "Tk_Grab");

   --  115

   --  116

   --  117

   procedure Tk_ImageChanged
     (master      : not null Tk_ImageMaster;
      x           : C.int;
      y           : C.int;
      width       : C.int;
      height      : C.int;
      imageWidth  : C.int;
      imageHeight : C.int);
   pragma Import (C, Tk_ImageChanged, "Tk_ImageChanged");

   --  118

   function Tk_Init
     (interp : Tcl_Interp)    -- can be null
     return C.int;
   pragma Import (C, Tk_Init, "Tk_Init");

   --  119

   --  120

   function Tk_IntersectTextLayout
     (layout : not null Tk_TextLayout;
      x      : C.int;
      y      : C.int;
      width  : C.int;
      height : C.int)
      return   C.int;
   pragma Import (C, Tk_IntersectTextLayout, "Tk_IntersectTextLayout");

   --  121

   procedure Tk_MaintainGeometry
     (slave  : not null Tk_Window;
      master : not null Tk_Window;
      x      : C.int;
      y      : C.int;
      width  : C.int;
      height : C.int);
   pragma Import (C, Tk_MaintainGeometry, "Tk_MaintainGeometry");

   --  122

   function Tk_MainWindow (interp : not null Tcl_Interp) return Tk_Window;
   pragma Import (C, Tk_MainWindow, "Tk_MainWindow");

   --  123

   procedure Tk_MakeWindowExist (tkwin : not null Tk_Window);
   pragma Import (C, Tk_MakeWindowExist, "Tk_MakeWindowExist");

   --  124

   procedure Tk_ManageGeometry
     (tkwin  : not null Tk_Window;
      mgrPtr : not null Tk_GeomMgr;
      data   : ClientData);
   pragma Import (C, Tk_ManageGeometry, "Tk_ManageGeometry");

   --  125

   procedure Tk_MapWindow (tkwin : not null Tk_Window);
   pragma Import (C, Tk_MapWindow, "Tk_MapWindow");

   --  126

   function Tk_MeasureChars
     (tkfont    : not null Tk_Font;
      source    : C.Strings.chars_ptr;
      numBytes  : C.int;
      maxPixels : C.int;
      flags     : C.int;
      lengthPtr : access C.int)
      return      C.int;
   pragma Import (C, Tk_MeasureChars, "Tk_MeasureChars");

   --  127

   procedure Tk_MoveResizeWindow
     (tkwin  : not null Tk_Window;
      x      : C.int;
      y      : C.int;
      width  : C.int;
      height : C.int);
   pragma Import (C, Tk_MoveResizeWindow, "Tk_MoveResizeWindow");

   --  128

   procedure Tk_MoveWindow
     (tkwin : not null Tk_Window;
      x     : C.int;
      y     : C.int);
   pragma Import (C, Tk_MoveWindow, "Tk_MoveWindow");

   --  129

   procedure Tk_MoveToplevelWindow
     (tkwin : not null Tk_Window;
      x     : C.int;
      y     : C.int);
   pragma Import (C, Tk_MoveToplevelWindow, "Tk_MoveToplevelWindow");

   --  130

   function Tk_NameOf3DBorder
     (border : not null Tk_3DBorder)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOf3DBorder, "Tk_NameOf3DBorder");

   --  131

   function Tk_NameOfAnchor
     (anchor : Tk_Anchor)
      return   C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfAnchor, "Tk_NameOfAnchor");

   --  132

   --  133

   function Tk_NameOfCapStyle (cap : C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfCapStyle, "Tk_NameOfCapStyle");

   --  134

   --  135

   --  136

   function Tk_NameOfFont (font : not null Tk_Font) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfFont, "Tk_NameOfFont");

   --  137

   function Tk_NameOfImage
     (imageMaster : not null Tk_ImageMaster)
      return        C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfImage, "Tk_NameOfImage");

   --  138

   function Tk_NameOfJoinStyle (join : C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfJoinStyle, "Tk_NameOfJoinStyle");

   --  139

   function Tk_NameOfJustify
     (justify : Tk_Justify)
      return    C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfJustify, "Tk_NameOfJustify");

   --  140

   function Tk_NameOfRelief (relief : C.int) return C.Strings.chars_ptr;
   pragma Import (C, Tk_NameOfRelief, "Tk_NameOfRelief");

   --  141

   function Tk_NameToWindow
     (interp   : not null Tcl_Interp;
      pathName : C.Strings.chars_ptr;
      tkwin    : not null Tk_Window)
      return     Tk_Window;
   pragma Import (C, Tk_NameToWindow, "Tk_NameToWindow");

   --  142

   --  143

   function Tk_ParseArgv
     (interp   : not null Tcl_Interp;
      tkwin    : not null Tk_Window;
      argcPtr  : access C.int;
      argv     : CArgv.Chars_Ptr_Ptr;
      argTable : not null Tk_ArgvInfo;
      flags    : C.int)
      return     C.int;
   pragma Import (C, Tk_ParseArgv, "Tk_ParseArgv");

   --  144

   procedure Tk_PhotoPutBlock
     (handle   : not null Tk_PhotoHandle;
      blockPtr : not null Tk_PhotoImageBlock;
      x        : C.int;
      y        : C.int;
      width    : C.int;
      height   : C.int);
   pragma Import (C, Tk_PhotoPutBlock, "Tk_PhotoPutBlock");

   --  145

   procedure Tk_PhotoPutZoomedBlock
     (handle     : not null Tk_PhotoHandle;
      blockPtr   : not null Tk_PhotoImageBlock;
      x          : C.int;
      y          : C.int;
      width      : C.int;
      height     : C.int;
      zoomX      : C.int;
      zoomY      : C.int;
      subsampleX : C.int;
      subsampleY : C.int);
   pragma Import (C, Tk_PhotoPutZoomedBlock, "Tk_PhotoPutZoomedBlock");

   --  146

   function Tk_PhotoGetImage
     (handle   : not null Tk_PhotoHandle;
      blockPtr : not null Tk_PhotoImageBlock)
      return     C.int;
   pragma Import (C, Tk_PhotoGetImage, "Tk_PhotoGetImage");

   --  147

   procedure Tk_PhotoBlank (handle : not null Tk_PhotoHandle);
   pragma Import (C, Tk_PhotoBlank, "Tk_PhotoBlank");

   --  148

   procedure Tk_PhotoExpand
     (handle : not null Tk_PhotoHandle;
      width  : C.int;
      height : C.int);
   pragma Import (C, Tk_PhotoExpand, "Tk_PhotoExpand");

   --  149

   procedure Tk_PhotoGetSize
     (handle    : not null Tk_PhotoHandle;
      widthPtr  : access C.int;
      heightPtr : access C.int);
   pragma Import (C, Tk_PhotoGetSize, "Tk_PhotoGetSize");

   --  150

   procedure Tk_PhotoSetSize
     (handle : not null Tk_PhotoHandle;
      width  : C.int;
      height : C.int);
   pragma Import (C, Tk_PhotoSetSize, "Tk_PhotoSetSize");

   --  151

   function Tk_PointToChar
     (layout : not null Tk_TextLayout;
      x      : C.int;
      y      : C.int)
      return   C.int;
   pragma Import (C, Tk_PointToChar, "Tk_PointToChar");

   --  152

   function Tk_PostscriptFontName
     (tkfont : not null Tk_Font;
      dsPtr  : not null Tcl_DString)
      return   C.int;
   pragma Import (C, Tk_PostscriptFontName, "Tk_PostscriptFontName");

   --  153

   --  154

   --  155

   --  156

   procedure Tk_ResizeWindow
     (tkwin  : not null Tk_Window;
      width  : C.int;
      height : C.int);
   pragma Import (C, Tk_ResizeWindow, "Tk_ResizeWindow");

   --  157

   function Tk_RestackWindow
     (tkwin      : not null Tk_Window;
      aboveBelow : C.int;
      other      : not null Tk_Window)
      return       C.int;
   pragma Import (C, Tk_RestackWindow, "Tk_RestackWindow");

   --  158

   --  159

   function Tk_SafeInit
     (interp : Tcl_Interp)    -- can be null
     return C.int;
   pragma Import (C, Tk_SafeInit, "Tk_SafeInit");

   --  160

   function Tk_SetAppName
     (tkwin : not null Tk_Window;
      name  : C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;
   pragma Import (C, Tk_SetAppName, "Tk_SetAppName");

   --  161

   procedure Tk_SetBackgroundFromBorder
     (tkwin  : not null Tk_Window;
      border : not null Tk_3DBorder);
   pragma Import
     (C,
      Tk_SetBackgroundFromBorder,
      "Tk_SetBackgroundFromBorder");

   --  162

   procedure Tk_SetClass
     (tkwin     : not null Tk_Window;
      className : C.Strings.chars_ptr);
   pragma Import (C, Tk_SetClass, "Tk_SetClass");

   --  163

   procedure Tk_SetGrid
     (tkwin      : not null Tk_Window;
      reqWidth   : C.int;
      reqHeight  : C.int;
      gridWidth  : C.int;
      gridHeight : C.int);
   pragma Import (C, Tk_SetGrid, "Tk_SetGrid");

   --  164

   procedure Tk_SetInternalBorder (tkwin : not null Tk_Window;
                                   width : C.int);
   pragma Import (C, Tk_SetInternalBorder, "Tk_SetInternalBorder");

   --  165

   procedure Tk_SetWindowBackground
     (tkwin : not null Tk_Window;
      pixel : C.unsigned_long);
   pragma Import (C, Tk_SetWindowBackground, "Tk_SetWindowBackground");

   --  166

   --  167

   procedure Tk_SetWindowBorder
     (tkwin : not null Tk_Window;
      pixel : C.unsigned_long);
   pragma Import (C, Tk_SetWindowBorder, "Tk_SetWindowBorder");

   --  168

   procedure Tk_SetWindowBorderWidth
     (tkwin : not null Tk_Window;
      width : C.int);
   pragma Import (C, Tk_SetWindowBorderWidth, "Tk_SetWindowBorderWidth");

   --  169

   --  170

   --  171

   --  172

   --  173

   procedure Tk_SizeOfImage
     (image     : not null Tk_Image;
      widthPtr  : access C.int;
      heightPtr : access C.int);
   pragma Import (C, Tk_SizeOfImage, "Tk_SizeOfImage");

   --  174

   function Tk_StrictMotif (tkwin : not null Tk_Window) return C.int;
   pragma Import (C, Tk_StrictMotif, "Tk_StrictMotif");

   --  175

   procedure Tk_TextLayoutToPostscript
     (interp : not null Tcl_Interp;
      layout : not null Tk_TextLayout);
   pragma Import (C, Tk_TextLayoutToPostscript, "Tk_TextLayoutToPostscript");

   --  176

   function Tk_TextWidth
     (font     : not null Tk_Font;
      str      : C.Strings.chars_ptr;
      numBytes : C.int)
      return     C.int;
   pragma Import (C, Tk_TextWidth, "Tk_TextWidth");

   --  177

   procedure Tk_UndefineCursor (window : not null Tk_Window);
   pragma Import (C, Tk_UndefineCursor, "Tk_UndefineCursor");

   --  178

   --  179

   --  180

   procedure Tk_Ungrab (tkwin : not null Tk_Window);
   pragma Import (C, Tk_Ungrab, "Tk_Ungrab");

   --  181

   procedure Tk_UnmaintainGeometry
     (slave  : not null Tk_Window;
      master : not null Tk_Window);
   pragma Import (C, Tk_UnmaintainGeometry, "Tk_UnmaintainGeometry");

   --  182

   procedure Tk_UnmapWindow (tkwin : not null Tk_Window);
   pragma Import (C, Tk_UnmapWindow, "Tk_UnmapWindow");

   --  183

   procedure Tk_UnsetGrid (tkwin : not null Tk_Window);
   pragma Import (C, Tk_UnsetGrid, "Tk_UnsetGrid");

   --  184

   procedure Tk_UpdatePointer
     (tkwin : not null Tk_Window;
      x     : C.int;
      y     : C.int;
      state : C.int);
   pragma Import (C, Tk_UpdatePointer, "Tk_UpdatePointer");

   --  185

   --  186

   function Tk_Alloc3DBorderFromObj
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj)
      return   Tk_3DBorder;
   pragma Import (C, Tk_Alloc3DBorderFromObj, "Tk_Alloc3DBorderFromObj");

   --  187

   --  188

   function Tk_AllocCursorFromObj
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj)
      return   Tk_Cursor;
   pragma Import (C, Tk_AllocCursorFromObj, "Tk_AllocCursorFromObj");

   --  189

   function Tk_AllocFontFromObj
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj)
      return   Tk_Font;
   pragma Import (C, Tk_AllocFontFromObj, "Tk_AllocFontFromObj");

   --  190

   function Tk_CreateOptionTable
     (interp      : not null Tcl_Interp;
      templatePtr : not null Tk_OptionSpec)
      return        Tk_OptionTable;
   pragma Import (C, Tk_CreateOptionTable, "Tk_CreateOptionTable");

   --  191

   procedure Tk_DeleteOptionTable (optionTable : not null Tk_OptionTable);
   pragma Import (C, Tk_DeleteOptionTable, "Tk_DeleteOptionTable");

   --  192

   procedure Tk_Free3DBorderFromObj
     (tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj);
   pragma Import (C, Tk_Free3DBorderFromObj, "Tk_Free3DBorderFromObj");

   --  193

   procedure Tk_FreeBitmapFromObj
     (tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj);
   pragma Import (C, Tk_FreeBitmapFromObj, "Tk_FreeBitmapFromObj");

   --  194

   procedure Tk_FreeColorFromObj
     (tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj);
   pragma Import (C, Tk_FreeColorFromObj, "Tk_FreeColorFromObj");

   --  195

   procedure Tk_FreeConfigOptions
     (recordPtr   : C.Strings.chars_ptr;
      optionToken : not null Tk_OptionTable;
      tkwin       : not null Tk_Window);
   pragma Import (C, Tk_FreeConfigOptions, "Tk_FreeConfigOptions");

   --  196

   procedure Tk_FreeSavedOptions (savePtr : not null Tk_SavedOptions);
   pragma Import (C, Tk_FreeSavedOptions, "Tk_FreeSavedOptions");

   --  197

   procedure Tk_FreeCursorFromObj
     (tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj);
   pragma Import (C, Tk_FreeCursorFromObj, "Tk_FreeCursorFromObj");

   --  198

   procedure Tk_FreeFontFromObj (tkwin : not null Tk_Window;
                                 objPtr : not null Tcl_Obj);
   pragma Import (C, Tk_FreeFontFromObj, "Tk_FreeFontFromObj");

   --  199

   function Tk_Get3DBorderFromObj
     (tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj)
      return   Tk_3DBorder;
   pragma Import (C, Tk_Get3DBorderFromObj, "Tk_Get3DBorderFromObj");

   --  200

   function Tk_GetAnchorFromObj
     (interp    : not null Tcl_Interp;
      objPtr    : not null Tcl_Obj;
      anchorPtr : Tk_Anchor)
      return      C.int;
   pragma Import (C, Tk_GetAnchorFromObj, "Tk_GetAnchorFromObj");

   --  201

   --  202

   --  203

   function Tk_GetCursorFromObj
     (tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj)
      return   Tk_Cursor;
   pragma Import (C, Tk_GetCursorFromObj, "Tk_GetCursorFromObj");

   --  204

   function Tk_GetOptionInfo
     (interp      : not null Tcl_Interp;
      recordPtr   : C.Strings.chars_ptr;
      optionTable : not null Tk_OptionTable;
      namePtr     : not null Tcl_Obj;
      tkwin       : not null Tk_Window)
      return        Tcl_Obj;
   pragma Import (C, Tk_GetOptionInfo, "Tk_GetOptionInfo");

   --  205

   function Tk_GetOptionValue
     (interp      : not null Tcl_Interp;
      recordPtr   : C.Strings.chars_ptr;
      optionTable : not null Tk_OptionTable;
      namePtr     : not null Tcl_Obj;
      tkwin       : not null Tk_Window)
      return        Tcl_Obj;
   pragma Import (C, Tk_GetOptionValue, "Tk_GetOptionValue");

   --  206

   function Tk_GetJustifyFromObj
     (interp     : not null Tcl_Interp;
      objPtr     : not null Tcl_Obj;
      justifyPtr : Tk_Justify)
      return       C.int;
   pragma Import (C, Tk_GetJustifyFromObj, "Tk_GetJustifyFromObj");

   --  207

   function Tk_GetMMFromObj
     (interp    : not null Tcl_Interp;
      tkwin     : not null Tk_Window;
      objPtr    : not null Tcl_Obj;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_GetMMFromObj, "Tk_GetMMFromObj");

   --  208

   function Tk_GetPixelsFromObj
     (interp : not null Tcl_Interp;
      tkwin  : not null Tk_Window;
      objPtr : not null Tcl_Obj;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetPixelsFromObj, "Tk_GetPixelsFromObj");

   --  209

   function Tk_GetReliefFromObj
     (interp    : not null Tcl_Interp;
      objPtr    : not null Tcl_Obj;
      resultPtr : access C.int)
      return      C.int;
   pragma Import (C, Tk_GetReliefFromObj, "Tk_GetReliefFromObj");

   --  210

   function Tk_GetScrollInfoObj
     (interp : not null Tcl_Interp;
      objc   : C.int;
      objv   : Tcl_Obj_Array;
      dblPtr : access C.double;
      intPtr : access C.int)
      return   C.int;
   pragma Import (C, Tk_GetScrollInfoObj, "Tk_GetScrollInfoObj");

   --  211

   function Tk_InitOptions
     (interp      : not null Tcl_Interp;
      recordPtr   : C.Strings.chars_ptr;
      optionToken : not null Tk_OptionTable;
      tkwin       : not null Tk_Window)
      return        C.int;
   pragma Import (C, Tk_InitOptions, "Tk_InitOptions");

   --  212

   procedure Tk_MainEx
     (argc        : C.int;
      argv        : CArgv.Chars_Ptr_Ptr;
      appInitProc : not null Tcl_AppInitProc;
      interp      : not null Tcl_Interp);
   pragma Import (C, Tk_MainEx, "Tk_MainEx");

   --  213

   procedure Tk_RestoreSavedOptions (savePtr : not null Tk_SavedOptions);
   pragma Import (C, Tk_RestoreSavedOptions, "Tk_RestoreSavedOptions");

   --  214

   function Tk_SetOptions
     (interp      : not null Tcl_Interp;
      recordPtr   : C.Strings.chars_ptr;
      optionTable : not null Tk_OptionTable;
      objc        : C.int;
      objv        : Tcl_Obj_Array;
      tkwin       : not null Tk_Window;
      savePtr     : not null Tk_SavedOptions;
      maskPtr     : access C.int)
      return        C.int;
   pragma Import (C, Tk_SetOptions, "Tk_SetOptions");

   --  215

   procedure Tk_InitConsoleChannels (interp : not null Tcl_Interp);
   pragma Import (C, Tk_InitConsoleChannels, "Tk_InitConsoleChannels");

   --  216

   function Tk_CreateConsoleWindow (interp : not null Tcl_Interp) return C.int;
   pragma Import (C, Tk_CreateConsoleWindow, "Tk_CreateConsoleWindow");

   --  217

   procedure Tk_CreateSmoothMethod
     (interp : not null Tcl_Interp;
      method : not null Tk_SmoothMethod);
   pragma Import (C, Tk_CreateSmoothMethod, "Tk_CreateSmoothMethod");

   --  Slot 218 is reserved

   --  Slot 219 is reserved

   --  220

   function Tk_GetDash
     (interp : not null Tcl_Interp;
      value  : C.Strings.chars_ptr;
      dash   : not null Tk_Dash)
      return   C.int;
   pragma Import (C, Tk_GetDash, "Tk_GetDash");

   --  221

   procedure Tk_CreateOutline (outline : not null Tk_Outline);
   pragma Import (C, Tk_CreateOutline, "Tk_CreateOutline");

   --  222

   --  223

   --  224

   function Tk_ChangeOutlineGC
     (canvas  : not null Tk_Canvas;
      item    : not null Tk_Item;
      outline : not null Tk_Outline)
      return    C.int;
   pragma Import (C, Tk_ChangeOutlineGC, "Tk_ChangeOutlineGC");

   --  225

   function Tk_ResetOutlineGC
     (canvas  : not null Tk_Canvas;
      item    : not null Tk_Item;
      outline : not null Tk_Outline)
      return    C.int;
   pragma Import (C, Tk_ResetOutlineGC, "Tk_ResetOutlineGC");

   --  226

   function Tk_CanvasPsOutline
     (canvas  : not null Tk_Canvas;
      item    : not null Tk_Item;
      outline : not null Tk_Outline)
      return    C.int;
   pragma Import (C, Tk_CanvasPsOutline, "Tk_CanvasPsOutline");

   --  227

   --  228

   function Tk_CanvasGetCoordFromObj
     (interp    : not null Tcl_Interp;
      canvas    : not null Tk_Canvas;
      obj       : not null Tcl_Obj;
      doublePtr : access C.double)
      return      C.int;
   pragma Import (C, Tk_CanvasGetCoordFromObj, "Tk_CanvasGetCoordFromObj");

   --  229

   --  230

   procedure Tk_DitherPhoto
     (handle : not null Tk_PhotoHandle;
      x      : C.int;
      y      : C.int;
      width  : C.int;
      height : C.int);
   pragma Import (C, Tk_DitherPhoto, "Tk_DitherPhoto");

   --  231

   --  232

   --  233

   function Tk_PostscriptFont
     (interp : not null Tcl_Interp;
      psInfo : not null Tk_PostscriptInfo;
      font   : not null Tk_Font)
      return   C.int;
   pragma Import (C, Tk_PostscriptFont, "Tk_PostscriptFont");

   --  234

   function Tk_PostscriptImage
     (image   : not null Tk_Image;
      interp  : not null Tcl_Interp;
      tkwin   : Tk_Window;
      psinfo  : Tk_PostscriptInfo;
      x       : C.int;
      y       : C.int;
      width   : C.int;
      height  : C.int;
      prepass : C.int)
      return    C.int;
   pragma Import (C, Tk_PostscriptImage, "Tk_PostscriptImage");

   --  235

   procedure Tk_PostscriptPath
     (interp    : not null Tcl_Interp;
      psInfo    : not null Tk_PostscriptInfo;
      coordPtr  : access C.double;
      numPoints : C.int);
   pragma Import (C, Tk_PostscriptPath, "Tk_PostscriptPath");

   --  236

   --  237

   function Tk_PostscriptY
     (y      : C.double;
      psInfo : not null Tk_PostscriptInfo)
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

   type Tk_3DBorder_Rec is null record;

   type Tk_ArgvInfo_rec is null record;                         -- @todo

   type Tk_BindingTable_rec is null record;

   type Tk_Canvas_rec is null record;

   type Tk_Cursor_rec is null record;

   type Tk_ErrorHandler_Rec is null record;

   type Tk_FakeWin_Rec is null record;

   type Tk_Font_Rec is null record;

   type Tk_Image_Rec is null record;

   type Tk_ImageMaster_Rec is null record;

   type Tk_ImageType_Rec is null record;

   type Tk_OptionTable_Rec is null record;

   type Tk_Outline_Rec is null record;

   type Tk_PhotoHandle_Rec is null record;

   type Tk_PhotoImageFormat_rec is null record;

   type Tk_PostscriptInfo_Rec is null record;

   type Tk_SavedOption_rec is record
      --  Points to information that describes the option.
      valuePtr : Tcl_Obj;
      --  The old value of the option, in the form of a Tcl object;
      --  may be NULL if the value wasn't saved as an object.
      internalForm : C.double;
      --  The old value of the option, in some internal representation
      --  such as an int or {XColor *}.  Valid only if
      --  optionPtr->specPtr->objOffset is < 0.  The space must be
      --  large enough to accommodate a double, a long, or a pointer;
      --  right now it looks like a double is big enough.  Also, using
      --  a double guarantees that the field is properly aligned for
      --  storing large values.
   end record;
   pragma Convention (C, Tk_SavedOption_rec);

   type Tk_SavedOption_Array is
     array (CNatural range <>) of aliased Tk_SavedOption;
   pragma Convention (C, Tk_SavedOption_Array);

   TK_NUM_SAVED_OPTIONS : constant := 20;
   type Tk_SavedOptions_Rec is record
      recordPtr : C.Strings.chars_ptr;
      --  The data structure in which to restore configuration
      --  options.
      tkwin : Tk_Window;
      --  Window associated with recordPtr; needed to restore certain
      --  options.
      numItems : C.int;
      --  The number of valid items in items field.
      items : Tk_SavedOption_Array (0 .. 19);
      --  Items used to hold old values.
      nextPtr : Tk_SavedOptions;
      --  Points to next structure in list; needed if too many options
      --  changed to hold all the old values in a single structure.
      --  NULL means no more structures.
   end record;
   pragma Convention (C, Tk_SavedOptions_Rec);

   type Tk_TextLayout_Rec is null record;

   type Tk_Window_Rec is null record;

   type XActivateDeactivateEvent_rec is null record;

   type XVirtualEvent_rec is null record;

end Tcl.Tk;
