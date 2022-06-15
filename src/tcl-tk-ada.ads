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

   package C renames Interfaces.C;

   procedure Set_Trace (State : Boolean);
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

   function Widget_Image (Win : Widget'Class) return String;
   --  Returns the string name of Win.

   function "&"
     (Left  : Widget'Class;
      Right : Widget'Class)
      return  String;
   function "&" (Left : Widget'Class; Right : String) return String;
   function "&" (Left : String; Right : Widget'Class) return String;
   --  Concatenates and returns the string names of Left and Right.
   --  Does not insert the separating dot.

   pragma Inline (Widget_Image, "&");

   procedure Set_Context (Interp : Tcl_Interp);
   --  Sets the interpreter context for all Tk calls which do not include
   --  either an Interp or Widget parameter.

   function Get_Context return Tcl_Interp;
   --  Gets the current interpreter context.

   function Get_Interp (Widgt : Widget'Class) return Tcl_Interp;
   --  Gets the interpreter of the specified Widget.

   ---------------------------------------------
   --
   --   Widget constructors
   --
   ---------------------------------------------

   function Create
     (pathName : String;
      options  : String := "")
      return     Widget
     is abstract;
   procedure Create
     (Widgt    : out Widget;
      pathName : String;
      options  : String := "")
     is abstract;
   --  Creates a new widget in the "contextual" interpreter.  Options
   --  may be specified via the "options" parameter or the option
   --  database to configure the widget.

   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Widget
     is abstract;
   procedure Create
     (Widgt    : out Widget;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
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
     (Widgt  : Widget'Class;
      option : String)
      return   String;
   --  Returns the current value of the specified configuration option.

   function configure
     (Widgt   : Widget'Class;
      options : String := "")
      return    String;
   procedure configure (Widgt : Widget'Class; options : String := "");
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
     (Widgt    : Widget'Class;
      Sequence : String;
      Script   : String);
   --  Associates Tcl script Script with the event Sequence.

   procedure Unbind (Widgt : Widget'Class; Sequence : String);
   function Unbind
     (Widgt    : Widget'Class;
      Sequence : String)
      return     String;
   --  Disassociates the binding from the event Sequence.
   procedure Bind (Widgt : Widget'Class; Sequence : String)
     renames Unbind;
   function Bind
     (Widgt    : Widget'Class;
      Sequence : String)
     return     String
     renames Unbind;
   --  Retained for backward compatibility.

   procedure Bind_To_Main_Window
     (Interp   : Tcl_Interp;
      Sequence : String;
      Script   : String);
   --  Associates Tcl script Script with the event Sequence in the main window.

   procedure Unbind_From_Main_Window
     (Interp   : Tcl_Interp;
      Sequence : String);
   function Unbind_From_Main_Window
     (Interp   : Tcl_Interp;
      Sequence : String)
      return     String;
   --  Disassociates the binding from the event Sequence in the main window.
   procedure Bind_To_Main_Window
     (Interp   : Tcl_Interp;
     Sequence : String)
     renames Unbind_From_Main_Window;
   function Bind_To_Main_Window
     (Interp   : Tcl_Interp;
      Sequence : String)
     return     String
     renames Unbind_From_Main_Window;
   --  Retained for backward compatibility.

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

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Frame;
   overriding
   procedure Create
     (Widgt    : out Frame;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes
   --  it into a frame widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Frame;
   overriding
   procedure Create
     (Widgt    : out Frame;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a frame widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Toplevel widget
   --
   ---------------------------------------------

   type Toplevel is new Frame with private;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Toplevel;
   overriding
   procedure Create
     (Widgt    : out Toplevel;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a toplevel widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Toplevel;
   overriding
   procedure Create
     (Widgt    : out Toplevel;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a toplevel widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Label widget
   --
   ---------------------------------------------

   type Label is new Frame with private;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Label;
   overriding
   procedure Create
     (Widgt    : out Label;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a label widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Label;
   overriding
   procedure Create
     (Widgt    : out Label;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  into a label widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Message widget
   --
   ---------------------------------------------

   type Message is new Frame with private;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Message;
   overriding
   procedure Create
     (Widgt    : out Message;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a message widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Message;
   overriding
   procedure Create
     (Widgt    : out Message;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a message widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   ---------------------------------------------
   --
   --   Button widget
   --
   ---------------------------------------------

   type Button is new Frame with private;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Button;
   overriding

   procedure Create
     (Widgt    : out Button;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a button widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding

   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Button;
   overriding

   procedure Create
     (Widgt    : out Button;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  into a button widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   procedure Flash (Buttn : Button'Class);
   --  Flash the button.

   function Invoke
     (Buttn   : Button'Class;
      options : String := "")
      return    String;
   --  Invoke the Tcl command associated with the button.

   ---------------------------------------------
   --
   --   RadioButton widget
   --
   ---------------------------------------------

   type RadioButton is new Button with private;

   overriding

   function Create
     (pathName : String;
      options  : String := "")
      return     RadioButton;
   overriding

   procedure Create
     (Widgt    : out RadioButton;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a radiobutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding

   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     RadioButton;
   overriding

   procedure Create
     (Widgt    : out RadioButton;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a radiobutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   procedure Deselect (Buttn : RadioButton);
   --  Deselect the button.

   procedure Tk_Select (Buttn : RadioButton);
   --  Select the button.

   procedure Toggle (Buttn : RadioButton);
   --  Toggle the button.

   ---------------------------------------------
   --
   --   CheckButton widget
   --
   ---------------------------------------------

   type CheckButton is new Button with private;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     CheckButton;
   overriding
   procedure Create
     (Widgt    : out CheckButton;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a checkbutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     CheckButton;
   overriding
   procedure Create
     (Widgt    : out CheckButton;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a checkbutton widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   procedure Deselect (Buttn : CheckButton);
   --  Deselect the button.

   procedure Tk_Select (Buttn : CheckButton);
   --  Select the button.

   procedure Toggle (Buttn : CheckButton);
   --  Toggle the button.

   ---------------------------------------------
   --
   --   Entry widget
   --
   ---------------------------------------------

   type EntryWidget is new Frame with private;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     EntryWidget;
   overriding
   procedure Create
     (Widgt    : out EntryWidget;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the "contextual" interpreter and makes it
   --  into a entry widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     EntryWidget;
   overriding
   procedure Create
     (Widgt    : out EntryWidget;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "");
   --  Creates a new widget in the specified interpreter and makes it
   --  into a entry widget.  Options may be specified via the "options"
   --  parameter or the option database to configure the widget.

   function get (Widgt : EntryWidget) return String;
   --  Returns the entry's string.

   ---------------------------------------------
   --
   --   After commands
   --
   --   These commands delay execution and schedule (and unschedule)
   --   future execution of Tcl commands.
   --
   ---------------------------------------------

   procedure After (Ms : Natural);
   --  Sleeps for Ms milliseconds in the "contextual" interpreter.

   procedure After (Interp : Tcl_Interp; Ms : Natural);
   --  Sleeps for Ms milliseconds in the specified interpreter.

   function After (Ms : Natural; Script : String) return String;
   procedure After (Ms : Natural; Script : String);
   --  Arranges for the Tcl Script to be executed after Ms milliseconds
   --  in the "contextual" interpreter.  The function returns an
   --  identifier suitable for canceling the command.

   function After
     (Interp : Tcl_Interp;
      Ms     : Natural;
      Script : String)
      return   String;
   procedure After
     (Interp : Tcl_Interp;
      Ms     : Natural;
      Script : String);
   --  Arranges for the Tcl Script to be executed after Ms milliseconds
   --  in the specified interpreter.  The function returns an
   --  identifier suitable for canceling the command.

   procedure Cancel (id_or_script : String);
   --  Cancels the execution of a delayed command in the "contextual"
   --  interpreter.

   procedure Cancel (Interp : Tcl_Interp; id_or_script : String);
   --  Cancels the execution of a delayed command in the specified
   --  interpreter.

   function Idle (Script : String) return String;
   procedure Idle (Script : String);
   --  Arranges for the Tcl Script to be executed later as an idle
   --  handler in the "contextual" interpreter.  The function returns
   --  an identifier suitable for canceling the command.

   function Idle (Interp : Tcl_Interp; Script : String) return String;
   procedure Idle (Interp : Tcl_Interp; Script : String);
   --  Arranges for the Tcl Script to be executed later as an idle
   --  handler in the specified interpreter.  The function returns
   --  an identifier suitable for canceling the command.

   function Info (id : String := "") return String;
   --  Returns information about existing event handlers in the
   --  "contextual" interpreter.

   function Info
     (Interp : Tcl_Interp;
      id     : String := "")
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

   procedure Pack (Slave : Widget'Class; Options : String);
   procedure Pack_Configure (Slave : Widget'Class; Options : String);
   --  Tells the packer how to configure the specified Slave window.

   procedure Pack_Forget (Slave : Widget'Class);
   --  Removes the Slave window from the packing list for its master
   --  and unmaps their windows.

   function Pack_Info (Slave : Widget'Class) return String;
   --  Returns a list whose elements are the current configuration
   --  state of the specified Slave window.

   procedure Pack_Propagate (Master : Widget'Class; State : Boolean);
   --  Enables or disables propagation for the specified Master window.

   function Pack_Propagate (Master : Widget'Class) return Boolean;
   --  Returns state of propagation in the specified Master window.

   function Pack_Slaves (Master : Widget'Class) return String;
   --  Returns a list of slaves in the packing order of the specified
   --  Master window.

   function Tk_PathName (tkwin : Tk_Window) return String;

   procedure Tk_AddOption
     (tkwin    : Tk_Window;
      name     : String;
      value    : String;
      priority : C.int);

   function Tk_CanvasGetCoord
     (interp    : Tcl_Interp;
      canvas    : Tk_Canvas;
      str       : String;
      doublePtr : access C.double)
      return      C.int;

   function Tk_CanvasTagsParseProc
     (data    : ClientData;
      interp  : Tcl_Interp;
      tkwin   : Tk_Window;
      value   : String;
      widgRec : String;
      offset  : C.int)
      return    C.int;

   function Tk_CanvasTagsPrintProc
     (data        : ClientData;
      tkwin       : Tk_Window;
      widgRec     : String;
      offset      : C.int;
      freeProcPtr : Tcl_FreeProc)
      return        String;

   function Tk_ConfigureInfo
     (interp   : Tcl_Interp;
      tkwin    : Tk_Window;
      specs    : Tk_ConfigSpec;
      widgRec  : String;
      argvName : String;
      flags    : C.int)
      return     C.int;

   function Tk_ConfigureValue
     (interp   : Tcl_Interp;
      tkwin    : Tk_Window;
      specs    : Tk_ConfigSpec;
      widgRec  : String;
      argvName : String;
      flags    : C.int)
      return     C.int;

   function Tk_ConfigureWidget
     (interp  : Tcl_Interp;
      tkwin   : Tk_Window;
      specs   : Tk_ConfigSpec;
      argc    : C.int;
      argv    : CArgv.Chars_Ptr_Ptr;
      widgRec : String;
      flags   : C.int)
      return    C.int;

   function Tk_ComputeTextLayout
     (font       : Tk_Font;
      str        : String;
      numChars   : C.int;
      wrapLength : C.int;
      justify    : Tk_Justify;
      flags      : C.int;
      widthPtr   : access C.int;
      heightPtr  : access C.int)
      return       Tk_TextLayout;

   function Tk_CreateBinding
     (interp       : Tcl_Interp;
      bindingTable : Tk_BindingTable;
      object       : ClientData;
      eventStr     : String;
      command      : String;
      append       : C.int)
      return         C.unsigned_long;

   function Tk_CreateWindow
     (interp     : Tcl_Interp;
      parent     : Tk_Window;
      name       : String;
      screenName : String)
      return       Tk_Window;

   function Tk_CreateWindowFromPath
     (interp     : Tcl_Interp;
      tkwin      : Tk_Window;
      pathName   : String;
      screenName : String)
      return       Tk_Window;

   function Tk_DefineBitmap
     (interp : Tcl_Interp;
      name   : String;
      source : String;
      width  : C.int;
      height : C.int)
      return   C.int;

   function Tk_DeleteBinding
     (interp       : Tcl_Interp;
      bindingTable : Tk_BindingTable;
      object       : ClientData;
      eventStr     : String)
      return         C.int;

   procedure Tk_DeleteImage (interp : Tcl_Interp; name : String);

   function Tk_DisplayName (tkwin : Tk_Window) return String;

   function Tk_FindPhoto
     (interp    : Tcl_Interp;
      imageName : String)
      return      Tk_PhotoHandle;

   function Tk_GetAnchor
     (interp    : Tcl_Interp;
      str       : String;
      anchorPtr : Tk_Anchor)
      return      C.int;

   function Tk_GetBinding
     (interp       : Tcl_Interp;
      bindingTable : Tk_BindingTable;
      object       : ClientData;
      eventStr     : String)
      return         String;

   function Tk_GetCapStyle
     (interp : Tcl_Interp;
      str    : String;
      capPtr : access C.int)
      return   C.int;

   function Tk_GetCursorFromData
     (interp : Tcl_Interp;
      tkwin  : Tk_Window;
      source : String;
      mask   : String;
      width  : C.int;
      height : C.int;
      xHot   : C.int;
      yHot   : C.int;
      fg     : Tk_Uid;
      bg     : Tk_Uid)
      return   Tk_Cursor;

   function Tk_GetFont
     (interp : Tcl_Interp;
      tkwin  : Tk_Window;
      str    : String)
      return   Tk_Font;

   function Tk_GetImage
     (interp     : Tcl_Interp;
      tkwin      : Tk_Window;
      name       : String;
      changeProc : Tk_ImageChangedProc;
      data       : ClientData)
      return       Tk_Image;

   function Tk_GetImageMasterData
     (interp     : Tcl_Interp;
      name       : String;
      typePtrPtr : Tk_ImageType)
      return       ClientData;

   function Tk_GetJoinStyle
     (interp  : Tcl_Interp;
      str     : String;
      joinPtr : access C.int)
      return    C.int;

   function Tk_GetJustify
     (interp     : Tcl_Interp;
      str        : String;
      justifyPtr : Tk_Justify)
      return       C.int;

   function Tk_GetOption
     (tkwin     : Tk_Window;
      name      : String;
      className : String)
      return      Tk_Uid;

   function Tk_GetPixels
     (interp : Tcl_Interp;
      tkwin  : Tk_Window;
      str    : String;
      intPtr : access C.int)
      return   C.int;

   function Tk_GetRelief
     (interp    : Tcl_Interp;
      name      : String;
      reliefPtr : access C.int)
      return      C.int;

   function Tk_GetScreenMM
     (interp    : Tcl_Interp;
      tkwin     : Tk_Window;
      str       : String;
      doublePtr : access C.double)
      return      C.int;

   function Tk_GetUid (str : String) return Tk_Uid;

   function Tk_MeasureChars
     (tkfont    : Tk_Font;
      source    : String;
      numBytes  : C.int;
      maxPixels : C.int;
      flags     : C.int;
      lengthPtr : access C.int)
      return      C.int;

   function Tk_NameOf3DBorder (border : Tk_3DBorder) return String;

   function Tk_NameOfAnchor (anchor : Tk_Anchor) return String;

   function Tk_NameOfCapStyle (cap : C.int) return String;

   function Tk_NameOfFont (font : Tk_Font) return String;

   function Tk_NameOfImage (imageMaster : Tk_ImageMaster) return String;

   function Tk_NameOfJoinStyle (join : C.int) return String;

   function Tk_NameOfJustify (justify : Tk_Justify) return String;

   function Tk_NameOfRelief (relief : C.int) return String;

   function Tk_NameToWindow
     (interp   : Tcl_Interp;
      pathName : String;
      tkwin    : Tk_Window)
      return     Tk_Window;

   function Tk_SetAppName
     (tkwin : Tk_Window;
      name  : String)
      return  String;

   procedure Tk_SetClass (tkwin : Tk_Window; className : String);

   function Tk_TextWidth
     (font     : Tk_Font;
      str      : String;
      numBytes : C.int)
      return     C.int;

   procedure Tk_FreeConfigOptions
     (recordPtr   : String;
      optionToken : Tk_OptionTable;
      tkwin       : Tk_Window);

   function Tk_GetOptionInfo
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionTable : Tk_OptionTable;
      namePtr     : Tcl_Obj;
      tkwin       : Tk_Window)
      return        Tcl_Obj;

   function Tk_GetOptionValue
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionTable : Tk_OptionTable;
      namePtr     : Tcl_Obj;
      tkwin       : Tk_Window)
      return        Tcl_Obj;

   function Tk_InitOptions
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionToken : Tk_OptionTable;
      tkwin       : Tk_Window)
      return        C.int;

   function Tk_SetOptions
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionTable : Tk_OptionTable;
      objc        : C.int;
      objv        : Tcl_Obj_Array;
      tkwin       : Tk_Window;
      savePtr     : Tk_SavedOptions;
      maskPtr     : access C.int)
      return        C.int;

   function Tk_GetDash
     (interp : Tcl_Interp;
      value  : String;
      dash   : Tk_Dash)
      return   C.int;

private

   type Widget is abstract tagged record
      Name   : C.Strings.chars_ptr;
      Interp : Tcl_Interp;
   end record;

   Context : Tcl_Interp;

   procedure Execute_Widget_Command
     (Widgt   : Widget'Class;
      command : String;
      options : String := "");

   type Frame is new Widget with null record;
   type Toplevel is new Frame with null record;
   type Label is new Frame with null record;
   type Message is new Frame with null record;
   type Button is new Frame with null record;
   type RadioButton is new Button with null record;
   type CheckButton is new Button with null record;
   type EntryWidget is new Frame with null record;

end Tcl.Tk.Ada;
