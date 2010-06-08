--------------------------------------------------------------------
--
--  tcl-tk-ada.ads -- This package provides the "thick" binding to Tcl.Tk.
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

--------------------------------------------------------------------

with CArgv;

package Tcl.Tk.Ada is

   Version : constant String := "8.1.1a";

   package C renames Interfaces.C;

   procedure Set_Trace (State : in Boolean);
   --  Turn on tracing of Tcl/Tk command execution.

   ---------------------------------------------
   --
   --   The Widget data type, parent of all objects displayed on the screen.
   --
   --   It is abstract because it is just a convenience for creating a Widget
   --   class and for creating non-abstract derived widget types.  Since there
   --   is no such data type in Tk, we make it abstract so that no instance of
   --   type Widget may be created.
   --
   ---------------------------------------------

   type Widget is abstract tagged private;

   ---------------------------------------------
   --
   --   Widget path name constructors
   --
   ---------------------------------------------

   function Widget_Image (Win : in Widget'Class) return String;
   --  Returns the string name of Win.

   function "&"
     (Left  : in Widget'Class;
      Right : in Widget'Class)
      return  String;
   function "&" (Left : in Widget'Class; Right : in String) return String;
   function "&" (Left : in String; Right : in Widget'Class) return String;
   --  Concatenates and returns the string names of Left and Right.
   --  Does not insert the separating dot.

   pragma Inline (Widget_Image, "&");

   procedure Set_Context (Interp : in Tcl_Interp);
   --  Sets the interpreter context for all Tk calls which do not include
   --  either an Interp or Widget parameter.

   function Get_Context return Tcl_Interp;
   --  Gets the current interpreter context.

   function Get_Interp (Widgt : in Widget'Class) return Tcl_Interp;
   --  Gets the interpreter of the specified Widget.

   ---------------------------------------------
   --
   --   Widget constructors
   --
   ---------------------------------------------

   function Create
     (pathName : in String;
      options  : in String := "")
      return     Widget
   is abstract;
   procedure Create
     (Widgt    : out Widget;
      pathName : in String;
      options  : in String := "")
is abstract;
   --  Creates a new widget in the "contextual" interpreter.  Options
   --  may be specified via the "options" parameter or the option
   --  database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     Widget
   is abstract;
   procedure Create
     (Widgt    : out Widget;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
is abstract;
   --  Creates a new widget in the specified interpreter.  Options
   --  may be specified via the "options" parameter or the option
   --  database to configure the widget.

   ---------------------------------------------
   --
   --   Widget destructor
   --
   ---------------------------------------------

   procedure Destroy (Widgt : in out Widget'Class);
   --  Destroys a widget.

   ---------------------------------------------
   --
   --   Widget configuration query and modify
   --
   ---------------------------------------------

   function cget
     (Widgt  : in Widget'Class;
      option : in String)
      return   String;
   --  Returns the current value of the specified configuration option.

   function configure
     (Widgt   : in Widget'Class;
      options : in String := "")
      return    String;
   procedure configure (Widgt : in Widget'Class; options : in String := "");
   --  Queries or modifies the configuration options.  If options is
   --  an empty string, returns a list of all available options
   --  for the widget.

   ---------------------------------------------
   --
   --   Bind associates a Tcl script with an
   --   event.  The script is executed when
   --   the event occurs.
   --
   ---------------------------------------------

   procedure Bind
     (Widgt    : in Widget'Class;
      Sequence : in String;
      Script   : in String);
   --  Associates Tcl script Script with the event Sequence.

   procedure Bind (Widgt : in Widget'Class; Sequence : in String);
   function Bind
     (Widgt    : in Widget'Class;
      Sequence : in String)
      return     String;
   --  Disassociates the binding from the event Sequence.

   procedure Bind_to_Main_Window
     (Interp   : in Tcl_Interp;
      Sequence : in String;
      Script   : in String);
   --  Associates Tcl script Script with the event Sequence in the main window.

   procedure Bind_to_Main_Window
     (Interp   : in Tcl_Interp;
      Sequence : in String);
   function Bind_to_Main_Window
     (Interp   : in Tcl_Interp;
      Sequence : in String)
      return     String;
   --  Disassociates the binding from the event Sequence in the main window.

   ---------------------------------------------
   --
   --   Frame widget
   --
   --   This is a non-abstract type derived directly from Widget.
   --   Each of the derived widgets redefines the Create subprogram
   --   in order to create the correct type of widget.
   --
   ---------------------------------------------

   type Frame is new Widget with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     Frame;
   procedure Create
     (Widgt    : out Frame;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes
   --  it into a frame widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     Frame;
   procedure Create
     (Widgt    : out Frame;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a frame widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Toplevel widget
   --
   ---------------------------------------------

   type Toplevel is new Frame with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     Toplevel;
   procedure Create
     (Widgt    : out Toplevel;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a toplevel widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     Toplevel;
   procedure Create
     (Widgt    : out Toplevel;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a toplevel widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Label widget
   --
   ---------------------------------------------

   type Label is new Frame with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     Label;
   procedure Create
     (Widgt    : out Label;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a label widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     Label;
   procedure Create
     (Widgt    : out Label;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  into a label widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Message widget
   --
   ---------------------------------------------

   type Message is new Frame with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     Message;
   procedure Create
     (Widgt    : out Message;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a message widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     Message;
   procedure Create
     (Widgt    : out Message;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a message widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Button widget
   --
   ---------------------------------------------

   type Button is new Frame with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     Button;
   procedure Create
     (Widgt    : out Button;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a button widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     Button;
   procedure Create
     (Widgt    : out Button;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  into a button widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   procedure Flash (Buttn : in Button'Class);
   --  Flash the button.

   function Invoke
     (Buttn   : in Button'Class;
      options : in String := "")
      return    String;
   --  Invoke the Tcl command associated with the button.

   ---------------------------------------------
   --
   --   RadioButton widget
   --
   ---------------------------------------------

   type RadioButton is new Button with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     RadioButton;
   procedure Create
     (Widgt    : out RadioButton;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a radiobutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     RadioButton;
   procedure Create
     (Widgt    : out RadioButton;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a radiobutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   procedure Deselect (Buttn : in RadioButton);
   --  Deselect the button.

   procedure Tk_Select (Buttn : in RadioButton);
   --  Select the button.

   procedure Toggle (Buttn : in RadioButton);
   --  Toggle the button.

   ---------------------------------------------
   --
   --   CheckButton widget
   --
   ---------------------------------------------

   type CheckButton is new Button with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     CheckButton;
   procedure Create
     (Widgt    : out CheckButton;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a checkbutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     CheckButton;
   procedure Create
     (Widgt    : out CheckButton;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a checkbutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   procedure Deselect (Buttn : in CheckButton);
   --  Deselect the button.

   procedure Tk_Select (Buttn : in CheckButton);
   --  Select the button.

   procedure Toggle (Buttn : in CheckButton);
   --  Toggle the button.

   ---------------------------------------------
   --
   --   Entry widget
   --
   ---------------------------------------------

   type EntryWidget is new Frame with private;

   function Create
     (pathName : in String;
      options  : in String := "")
      return     EntryWidget;
   procedure Create
     (Widgt    : out EntryWidget;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a entry widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function Create
     (Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "")
      return     EntryWidget;
   procedure Create
     (Widgt    : out EntryWidget;
      Interp   : in Tcl_Interp;
      pathName : in String;
      options  : in String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a entry widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function get (Widgt : in EntryWidget) return String;
   --  Returns the entry's string.

   ---------------------------------------------
   --
   --   After commands
   --
   --   These commands delay execution and schedule (and unschedule)
   --   future execution of Tcl commands.
   --
   ---------------------------------------------

   procedure After (Ms : in Natural);
   --  Sleeps for Ms milliseconds in the "contextual" interpreter.

   procedure After (Interp : in Tcl_Interp; Ms : in Natural);
   --  Sleeps for Ms milliseconds in the specified interpreter.

   function After (Ms : in Natural; Script : in String) return String;
   procedure After (Ms : in Natural; Script : in String);
   --  Arranges for the Tcl Script to be executed after Ms milliseconds
   --  in the "contextual" interpreter.  The function returns an
   --  identifier suitable for canceling the command.

   function After
     (Interp : in Tcl_Interp;
      Ms     : in Natural;
      Script : in String)
      return   String;
   procedure After
     (Interp : in Tcl_Interp;
      Ms     : in Natural;
      Script : in String);
   --  Arranges for the Tcl Script to be executed after Ms milliseconds
   --  in the specified interpreter.  The function returns an
   --  identifier suitable for canceling the command.

   procedure Cancel (id_or_script : in String);
   --  Cancels the execution of a delayed command in the "contextual"
   --  interpreter.

   procedure Cancel (Interp : in Tcl_Interp; id_or_script : in String);
   --  Cancels the execution of a delayed command in the specified
   --  interpreter.

   function Idle (Script : in String) return String;
   procedure Idle (Script : in String);
   --  Arranges for the Tcl Script to be executed later as an idle
   --  handler in the "contextual" interpreter.  The function returns
   --  an identifier suitable for canceling the command.

   function Idle (Interp : in Tcl_Interp; Script : in String) return String;
   procedure Idle (Interp : in Tcl_Interp; Script : in String);
   --  Arranges for the Tcl Script to be executed later as an idle
   --  handler in the specified interpreter.  The function returns
   --  an identifier suitable for canceling the command.

   function Info (id : in String := "") return String;
   --  Returns information about existing event handlers in the
   --  "contextual" interpreter.

   function Info
     (Interp : in Tcl_Interp;
      id     : in String := "")
      return   String;
   --  Returns information about existing event handlers in the
   --  "contextual" interpreter.

   ---------------------------------------------
   --
   --   Pack commands
   --
   --   These commands provide for packing widgets within other
   --   widgets and therefore rendering them to the screen.
   --
   ---------------------------------------------

   procedure Pack (Slave : in Widget'Class; Options : in String);
   procedure Pack_Configure (Slave : in Widget'Class; Options : in String);
   --  Tells the packer how to configure the specified Slave window.

   procedure Pack_Forget (Slave : in Widget'Class);
   --  Removes the Slave window from the packing list for its master
   --  and unmaps their windows.

   function Pack_Info (Slave : in Widget'Class) return String;
   --  Returns a list whose elements are the current configuration
   --  state of the specified Slave window.

   procedure Pack_Propagate (Master : in Widget'Class; State : in Boolean);
   --  Enables or disables propagation for the specified Master window.

   function Pack_Propagate (Master : in Widget'Class) return Boolean;
   --  Returns state of propagation in the specified Master window.

   function Pack_Slaves (Master : in Widget'Class) return String;
   --  Returns a list of slaves in the packing order of the specified
   --  Master window.

   function Tk_PathName (tkwin : in Tk_Window) return String;

   procedure Tk_AddOption
     (tkwin    : in Tk_Window;
      name     : in String;
      value    : in String;
      priority : in C.int);

   function Tk_CanvasGetCoord
     (interp    : in Tcl_Interp;
      canvas    : in Tk_Canvas;
      str       : in String;
      doublePtr : access C.double)
      return      C.int;

   function Tk_CanvasTagsParseProc
     (data    : in ClientData;
      interp  : in Tcl_Interp;
      tkwin   : in Tk_Window;
      value   : in String;
      widgRec : in String;
      offset  : in C.int)
      return    C.int;

   function Tk_CanvasTagsPrintProc
     (data        : in ClientData;
      tkwin       : in Tk_Window;
      widgRec     : in String;
      offset      : in C.int;
      freeProcPtr : in Tcl_FreeProc)
      return        String;

   function Tk_ConfigureInfo
     (interp   : in Tcl_Interp;
      tkwin    : in Tk_Window;
      specs    : in Tk_ConfigSpec;
      widgRec  : in String;
      argvName : in String;
      flags    : in C.int)
      return     C.int;

   function Tk_ConfigureValue
     (interp   : in Tcl_Interp;
      tkwin    : in Tk_Window;
      specs    : in Tk_ConfigSpec;
      widgRec  : in String;
      argvName : in String;
      flags    : in C.int)
      return     C.int;

   function Tk_ConfigureWidget
     (interp  : in Tcl_Interp;
      tkwin   : in Tk_Window;
      specs   : in Tk_ConfigSpec;
      argc    : in C.int;
      argv    : in CArgv.Chars_Ptr_Ptr;
      widgRec : in String;
      flags   : in C.int)
      return    C.int;

   function Tk_ComputeTextLayout
     (font       : in Tk_Font;
      str        : in String;
      numChars   : in C.int;
      wrapLength : in C.int;
      justify    : in Tk_Justify;
      flags      : in C.int;
      widthPtr   : access C.int;
      heightPtr  : access C.int)
      return       Tk_TextLayout;

   function Tk_CreateBinding
     (interp       : in Tcl_Interp;
      bindingTable : in Tk_BindingTable;
      object       : in ClientData;
      eventStr     : in String;
      command      : in String;
      append       : in C.int)
      return         C.unsigned_long;

   function Tk_CreateWindow
     (interp     : in Tcl_Interp;
      parent     : in Tk_Window;
      name       : in String;
      screenName : in String)
      return       Tk_Window;

   function Tk_CreateWindowFromPath
     (interp     : in Tcl_Interp;
      tkwin      : in Tk_Window;
      pathName   : in String;
      screenName : in String)
      return       Tk_Window;

   function Tk_DefineBitmap
     (interp : in Tcl_Interp;
      name   : in String;
      source : in String;
      width  : in C.int;
      height : in C.int)
      return   C.int;

   function Tk_DeleteBinding
     (interp       : in Tcl_Interp;
      bindingTable : in Tk_BindingTable;
      object       : in ClientData;
      eventStr     : in String)
      return         C.int;

   procedure Tk_DeleteImage (interp : in Tcl_Interp; name : in String);

   function Tk_DisplayName (tkwin : in Tk_Window) return String;

   function Tk_FindPhoto
     (interp    : in Tcl_Interp;
      imageName : in String)
      return      Tk_PhotoHandle;

   function Tk_GetAnchor
     (interp    : in Tcl_Interp;
      str       : in String;
      anchorPtr : in Tk_Anchor)
      return      C.int;

   function Tk_GetBinding
     (interp       : in Tcl_Interp;
      bindingTable : in Tk_BindingTable;
      object       : in ClientData;
      eventStr     : in String)
      return         String;

   function Tk_GetCapStyle
     (interp : in Tcl_Interp;
      str    : in String;
      capPtr : access C.int)
      return   C.int;

   function Tk_GetCursorFromData
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      source : in String;
      mask   : in String;
      width  : in C.int;
      height : in C.int;
      xHot   : in C.int;
      yHot   : in C.int;
      fg     : in Tk_Uid;
      bg     : in Tk_Uid)
      return   Tk_Cursor;

   function Tk_GetFont
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      str    : in String)
      return   Tk_Font;

   function Tk_GetImage
     (interp     : in Tcl_Interp;
      tkwin      : in Tk_Window;
      name       : in String;
      changeProc : in Tk_ImageChangedProc;
      data       : in ClientData)
      return       Tk_Image;

   function Tk_GetImageMasterData
     (interp     : in Tcl_Interp;
      name       : in String;
      typePtrPtr : in Tk_ImageType)
      return       ClientData;

   function Tk_GetJoinStyle
     (interp  : in Tcl_Interp;
      str     : in String;
      joinPtr : access C.int)
      return    C.int;

   function Tk_GetJustify
     (interp     : in Tcl_Interp;
      str        : in String;
      justifyPtr : in Tk_Justify)
      return       C.int;

   function Tk_GetOption
     (tkwin     : in Tk_Window;
      name      : in String;
      className : in String)
      return      Tk_Uid;

   function Tk_GetPixels
     (interp : in Tcl_Interp;
      tkwin  : in Tk_Window;
      str    : in String;
      intPtr : access C.int)
      return   C.int;

   function Tk_GetRelief
     (interp    : in Tcl_Interp;
      name      : in String;
      reliefPtr : access C.int)
      return      C.int;

   function Tk_GetScreenMM
     (interp    : in Tcl_Interp;
      tkwin     : in Tk_Window;
      str       : in String;
      doublePtr : access C.double)
      return      C.int;

   function Tk_GetUid (str : in String) return Tk_Uid;

   function Tk_MeasureChars
     (tkfont    : in Tk_Font;
      source    : in String;
      numBytes  : in C.int;
      maxPixels : in C.int;
      flags     : in C.int;
      lengthPtr : access C.int)
      return      C.int;

   function Tk_NameOf3DBorder (border : in Tk_3DBorder) return String;

   function Tk_NameOfAnchor (anchor : in Tk_Anchor) return String;

   function Tk_NameOfCapStyle (cap : in C.int) return String;

   function Tk_NameOfFont (font : in Tk_Font) return String;

   function Tk_NameOfImage (imageMaster : in Tk_ImageMaster) return String;

   function Tk_NameOfJoinStyle (join : in C.int) return String;

   function Tk_NameOfJustify (justify : in Tk_Justify) return String;

   function Tk_NameOfRelief (relief : in C.int) return String;

   function Tk_NameToWindow
     (interp   : in Tcl_Interp;
      pathName : in String;
      tkwin    : in Tk_Window)
      return     Tk_Window;

   function Tk_SetAppName
     (tkwin : in Tk_Window;
      name  : in String)
      return  String;

   procedure Tk_SetClass (tkwin : in Tk_Window; className : in String);

   function Tk_TextWidth
     (font     : in Tk_Font;
      str      : in String;
      numBytes : in C.int)
      return     C.int;

   procedure Tk_FreeConfigOptions
     (recordPtr   : in String;
      optionToken : in Tk_OptionTable;
      tkwin       : in Tk_Window);

   function Tk_GetOptionInfo
     (interp      : in Tcl_Interp;
      recordPtr   : in String;
      optionTable : in Tk_OptionTable;
      namePtr     : in Tcl_Obj;
      tkwin       : in Tk_Window)
      return        Tcl_Obj;

   function Tk_GetOptionValue
     (interp      : in Tcl_Interp;
      recordPtr   : in String;
      optionTable : in Tk_OptionTable;
      namePtr     : in Tcl_Obj;
      tkwin       : in Tk_Window)
      return        Tcl_Obj;

   function Tk_InitOptions
     (interp      : in Tcl_Interp;
      recordPtr   : in String;
      optionToken : in Tk_OptionTable;
      tkwin       : in Tk_Window)
      return        C.int;

   function Tk_SetOptions
     (interp      : in Tcl_Interp;
      recordPtr   : in String;
      optionTable : in Tk_OptionTable;
      objc        : in C.int;
      objv        : in Tcl_Obj_Array;
      tkwin       : in Tk_Window;
      savePtr     : in Tk_SavedOptions;
      maskPtr     : access C.int)
      return        C.int;

   function Tk_GetDash
     (interp : in Tcl_Interp;
      value  : in String;
      dash   : in Tk_Dash)
      return   C.int;

private

   type Widget is abstract tagged record
      Name   : C.Strings.chars_ptr;
      Interp : Tcl_Interp;
   end record;

   Context : Tcl_Interp;

   procedure Execute_Widget_Command
     (Widgt   : in Widget'Class;
      command : in String;
      options : in String := "");

   type Frame is new Widget with null record;
   type Toplevel is new Frame with null record;
   type Label is new Frame with null record;
   type Message is new Frame with null record;
   type Button is new Frame with null record;
   type RadioButton is new Button with null record;
   type CheckButton is new Button with null record;
   type EntryWidget is new Frame with null record;

end Tcl.Tk.Ada;
