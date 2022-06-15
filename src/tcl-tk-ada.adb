--------------------------------------------------------------------
--
-- tcl-tk-ada.adb --
--
--  Copyright (c) 1995-2000 Terry J. Westley
--  Copyright (c) 2006-2020 Simon Wright <simon@pushface.org>
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

with Ada.Text_IO;
with Tcl.Ada;

package body Tcl.Tk.Ada is

   Trace : Boolean := False;

   procedure Tcl_Eval (Interp : Tcl_Interp; Cmd : String);
   function Get_Main_Window (Interp : Tcl_Interp) return Frame;
   pragma Unreferenced (Get_Main_Window);  --  XXX what is it for, then?

   procedure Tcl_Eval (Interp : Tcl_Interp; Cmd : String) is
   begin --  Tcl_Eval
      if Trace then
         Standard.Ada.Text_IO.Put_Line (Cmd);
      end if;
      Tcl.Ada.Tcl_Eval (Interp, Cmd);
   end Tcl_Eval;

   procedure Set_Trace (State : Boolean) is
   begin --  Set_Trace
      Trace := State;
   end Set_Trace;

   function Widget_Image (Win : Widget'Class) return String is
   begin --  Widget_Image
      return CHelper.Value (Win.Name);
   end Widget_Image;

   function "&"
     (Left  : Widget'Class;
      Right : Widget'Class)
      return  String
   is
   begin --  "&"
      return Widget_Image (Left) & Widget_Image (Right);
   end "&";

   function "&" (Left : Widget'Class; Right : String) return String is
   begin --  "&"
      return Widget_Image (Left) & Right;
   end "&";

   function "&" (Left : String; Right : Widget'Class) return String is
   begin --  "&"
      return Left & Widget_Image (Right);
   end "&";

   procedure Set_Context (Interp : Tcl_Interp) is
   begin --  Set_Context
      Context := Interp;
   end Set_Context;

   function Get_Context return Tcl_Interp is
   begin --  Get_Context
      return Context;
   end Get_Context;

   function Get_Interp (Widgt : Widget'Class) return Tcl_Interp is
   begin --  Get_Interp
      return Widgt.Interp;
   end Get_Interp;

   procedure Destroy (Widgt : in out Widget'Class) is
   begin --  Destroy
      Execute_Widget_Command (Widgt, "destroy");
      C.Strings.Free (Widgt.Name);
   end Destroy;

   function cget
     (Widgt  : Widget'Class;
      option : String)
      return   String
   is
   begin --  cget
      Execute_Widget_Command (Widgt, "cget", option);
      return Tcl.Ada.Tcl_GetResult (Widgt.Interp);
   end cget;

   function configure
     (Widgt   : Widget'Class;
      options : String := "")
      return    String
   is
   begin --  configure
      Execute_Widget_Command (Widgt, "configure", options);
      return Tcl.Ada.Tcl_GetResult (Widgt.Interp);
   end configure;

   procedure configure (Widgt : Widget'Class; options : String := "") is
   begin --  configure
      Execute_Widget_Command (Widgt, "configure", options);
   end configure;

   procedure Bind
     (Widgt    : Widget'Class;
      Sequence : String;
      Script   : String)
   is
   begin --  Bind
      Tcl_Eval
        (Widgt.Interp,
         "bind " & Widget_Image (Widgt) & " " & Sequence & " " & Script);
   end Bind;

   procedure Unbind (Widgt : Widget'Class; Sequence : String) is
   begin --  Unbind
      Tcl_Eval
        (Widgt.Interp,
         "bind " & Widget_Image (Widgt) & " " & Sequence);
   end Unbind;

   function Unbind
     (Widgt    : Widget'Class;
      Sequence : String)
      return     String
   is
   begin --  Unbind
      Tcl_Eval
        (Widgt.Interp,
         "bind " & Widget_Image (Widgt) & " " & Sequence);
      return Tcl.Ada.Tcl_GetResult (Widgt.Interp);
   end Unbind;

   procedure Bind_To_Main_Window
     (Interp   : Tcl_Interp;
      Sequence : String;
      Script   : String)
   is
   begin --  Bind_To_Main_Window
      Tcl_Eval (Interp, "bind . " & Sequence & " " & Script);
   end Bind_To_Main_Window;

   procedure Unbind_From_Main_Window
     (Interp   : Tcl_Interp;
      Sequence : String)
   is
   begin --  Unbind_From_Main_Window
      Tcl_Eval (Interp, "bind . " & Sequence);
   end Unbind_From_Main_Window;

   function Unbind_From_Main_Window
     (Interp   : Tcl_Interp;
      Sequence : String)
      return     String
   is
   begin --  Unbind_From_Main_Window
      Tcl_Eval (Interp, "bind . " & Sequence);
      return Tcl.Ada.Tcl_GetResult (Interp);
   end Unbind_From_Main_Window;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Frame
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out Frame;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Frame
   is
      --
      The_Widget : Frame;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "frame " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out Frame;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   function Get_Main_Window (Interp : Tcl_Interp) return Frame is
      --
      Prime_Window : aliased C.char_array := C.To_C (".");
   begin --  Get_Main_Window
      return (C.Strings.To_Chars_Ptr (Prime_Window'Unchecked_Access), Interp);
   end Get_Main_Window;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Toplevel
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out Toplevel;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Toplevel
   is
      --
      The_Widget : Toplevel;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "toplevel " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out Toplevel;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Label
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out Label;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Label
   is
      --
      The_Widget : Label;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "label " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out Label;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Message
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out Message;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Message
   is
      --
      The_Widget : Message;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "message " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out Message;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     Button
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out Button;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     Button
   is
      --
      The_Widget : Button;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "button " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out Button;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     RadioButton
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out RadioButton;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     RadioButton
   is
      --
      The_Widget : RadioButton;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "radiobutton " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out RadioButton;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     CheckButton
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out CheckButton;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     CheckButton
   is
      --
      The_Widget : CheckButton;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "checkbutton " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out CheckButton;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   procedure Flash (Buttn : Button'Class) is
   begin --  Flash
      Execute_Widget_Command (Widget'Class (Buttn), "flash");
   end Flash;

   function Invoke
     (Buttn   : Button'Class;
      options : String := "")
      return    String
   is
   begin --  Invoke
      Execute_Widget_Command (Widget'Class (Buttn), "invoke", options);
      return Tcl.Ada.Tcl_GetResult (Buttn.Interp);
   end Invoke;

   procedure Deselect (Buttn : RadioButton) is
   begin --  Deselect
      Execute_Widget_Command (Widget'Class (Buttn), "deselect");
   end Deselect;

   procedure Tk_Select (Buttn : RadioButton) is
   begin --  Tk_Select
      Execute_Widget_Command (Widget'Class (Buttn), "select");
   end Tk_Select;

   procedure Toggle (Buttn : RadioButton) is
   begin --  Toggle
      Execute_Widget_Command (Widget'Class (Buttn), "toggle");
   end Toggle;

   procedure Deselect (Buttn : CheckButton) is
   begin --  Deselect
      Execute_Widget_Command (Widget'Class (Buttn), "deselect");
   end Deselect;

   procedure Tk_Select (Buttn : CheckButton) is
   begin --  Tk_Select
      Execute_Widget_Command (Widget'Class (Buttn), "select");
   end Tk_Select;

   procedure Toggle (Buttn : CheckButton) is
   begin --  Toggle
      Execute_Widget_Command (Widget'Class (Buttn), "toggle");
   end Toggle;

   overriding
   function Create
     (pathName : String;
      options  : String := "")
      return     EntryWidget
   is
   begin --  Create
      return Create (Context, pathName, options);
   end Create;

   overriding
   procedure Create
     (Widgt    : out EntryWidget;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Context, pathName, options);
   end Create;

   overriding
   function Create
     (Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
      return     EntryWidget
   is
      --
      The_Widget : EntryWidget;
   begin --  Create
      The_Widget.Interp := Interp;
      The_Widget.Name   := C.Strings.New_String (pathName);
      Tcl_Eval (The_Widget.Interp, "entry " & pathName & " " & options);
      return The_Widget;
   end Create;

   overriding
   procedure Create
     (Widgt    : out EntryWidget;
      Interp   : Tcl_Interp;
      pathName : String;
      options  : String := "")
   is
   begin --  Create
      Widgt := Create (Interp, pathName, options);
   end Create;

   function get (Widgt : EntryWidget) return String is
   begin --  get
      Execute_Widget_Command (Widgt, "get");
      return Tcl.Ada.Tcl_GetResult (Widgt.Interp);
   end get;

   procedure After (Ms : Natural) is
      --
      use CArgv;
      Argv : CArgv.Chars_Ptr_Ptr := CArgv.Empty & Natural'Image (Ms);
   begin --  After
      Tcl_Eval (Context, "after " & Natural'Image (Ms));
      CArgv.Free (Argv);
   end After;

   procedure After (Interp : Tcl_Interp; Ms : Natural) is
   begin --  After
      Tcl_Eval (Interp, "after " & Natural'Image (Ms));
   end After;

   function After (Ms : Natural; Script : String) return String is
   begin --  After
      Tcl_Eval (Context, "after " & Natural'Image (Ms) & " " & Script);
      return Tcl.Ada.Tcl_GetResult (Context);
   end After;

   procedure After (Ms : Natural; Script : String) is
   begin --  After
      Tcl_Eval (Context, "after " & Natural'Image (Ms) & " " & Script);
   end After;

   function After
     (Interp : Tcl_Interp;
      Ms     : Natural;
      Script : String)
      return   String
   is
   begin --  After
      Tcl_Eval (Interp, "after " & Natural'Image (Ms) & " " & Script);
      return Tcl.Ada.Tcl_GetResult (Context);
   end After;

   procedure After
     (Interp : Tcl_Interp;
      Ms     : Natural;
      Script : String)
   is
   begin --  After
      Tcl_Eval (Interp, "after " & Natural'Image (Ms) & " " & Script);
   end After;

   procedure Cancel (id_or_script : String) is
   begin --  Cancel
      Tcl_Eval (Context, "after cancel " & id_or_script);
   end Cancel;

   procedure Cancel (Interp : Tcl_Interp; id_or_script : String) is
   begin --  Cancel
      Tcl_Eval (Interp, "after cancel " & id_or_script);
   end Cancel;

   function Idle (Script : String) return String is
   begin --  Idle
      Tcl_Eval (Context, "after idle " & Script);
      return Tcl.Ada.Tcl_GetResult (Context);
   end Idle;

   procedure Idle (Script : String) is
   begin --  Idle
      Tcl_Eval (Context, "after idle " & Script);
   end Idle;

   function Idle (Interp : Tcl_Interp; Script : String) return String is
   begin --  Idle
      Tcl_Eval (Interp, "after idle " & Script);
      return Tcl.Ada.Tcl_GetResult (Context);
   end Idle;

   procedure Idle (Interp : Tcl_Interp; Script : String) is
   begin --  Idle
      Tcl_Eval (Interp, "after idle " & Script);
   end Idle;

   function Info (id : String := "") return String is
   begin --  Info
      Tcl_Eval (Context, "after info " & id);
      return Tcl.Ada.Tcl_GetResult (Context);
   end Info;

   function Info
     (Interp : Tcl_Interp;
      id     : String := "")
      return   String
   is
   begin --  Info
      Tcl_Eval (Interp, "after info " & id);
      return Tcl.Ada.Tcl_GetResult (Context);
   end Info;

   procedure Pack (Slave : Widget'Class; Options : String) is
   begin --  Pack
      Tcl_Eval
        (Slave.Interp,
         "pack " & Widget_Image (Slave) & " " & Options);
   end Pack;

   procedure Pack_Configure (Slave : Widget'Class; Options : String) is
   begin --  Pack_Configure
      Tcl_Eval
        (Slave.Interp,
         "pack configure " & Widget_Image (Slave) & " " & Options);
   end Pack_Configure;

   procedure Pack_Forget (Slave : Widget'Class) is
   begin --  Pack_Forget
      Tcl_Eval (Slave.Interp, "pack forget " & Widget_Image (Slave));
   end Pack_Forget;

   function Pack_Info (Slave : Widget'Class) return String is
   begin --  Pack_Info
      Tcl_Eval (Slave.Interp, "pack info " & Widget_Image (Slave));
      return Tcl.Ada.Tcl_GetResult (Slave.Interp);
   end Pack_Info;

   procedure Pack_Propagate (Master : Widget'Class; State : Boolean) is
   begin --  Pack_Propagate
      Tcl_Eval
        (Master.Interp,
         "pack propagate " &
         Widget_Image (Master) &
         " " &
         Integer'Image (Boolean'Pos (State)));
   end Pack_Propagate;

   function Pack_Propagate (Master : Widget'Class) return Boolean is
   begin --  Pack_Propagate
      Tcl_Eval (Master.Interp, "pack propagate " & Widget_Image (Master));
      return Integer'Value (Tcl.Ada.Tcl_GetResult (Master.Interp)) = 1;
   end Pack_Propagate;

   function Pack_Slaves (Master : Widget'Class) return String is
   begin --  Pack_Slaves
      Tcl_Eval (Master.Interp, "pack slaves " & Widget_Image (Master));
      return Tcl.Ada.Tcl_GetResult (Master.Interp);
   end Pack_Slaves;

   procedure Execute_Widget_Command
     (Widgt   : Widget'Class;
      command : String;
      options : String := "")
   is
   begin --  Execute_Widget_Command
      Tcl_Eval
        (Widgt.Interp,
         Widget_Image (Widgt) & " " & command & " " & options);
   end Execute_Widget_Command;

   function Tk_PathName (tkwin : Tk_Window) return String is
   begin --  Tk_PathName
      return CHelper.Value (Tcl.Tk.Tk_PathName (tkwin));
   end Tk_PathName;

   procedure Tk_AddOption
     (tkwin    : Tk_Window;
      name     : String;
      value    : String;
      priority : C.int)
   is
      C_name  : aliased C.char_array := C.To_C (name);
      C_value : aliased C.char_array := C.To_C (value);
   begin --  Tk_AddOption
      Tcl.Tk.Tk_AddOption
        (tkwin,
         C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
         C.Strings.To_Chars_Ptr (C_value'Unchecked_Access),
         priority);
   end Tk_AddOption;

   function Tk_CanvasGetCoord
     (interp    : Tcl_Interp;
      canvas    : Tk_Canvas;
      str       : String;
      doublePtr : access C.double)
      return      C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_CanvasGetCoord
      return Tcl.Tk.Tk_CanvasGetCoord
               (interp,
                canvas,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                doublePtr);
   end Tk_CanvasGetCoord;

   function Tk_CanvasTagsParseProc
     (data    : ClientData;
      interp  : Tcl_Interp;
      tkwin   : Tk_Window;
      value   : String;
      widgRec : String;
      offset  : C.int)
      return    C.int
   is
      C_value   : aliased C.char_array := C.To_C (value);
      C_widgRec : aliased C.char_array := C.To_C (widgRec);
   begin --  Tk_CanvasTagsParseProc
      return Tcl.Tk.Tk_CanvasTagsParseProc
               (data,
                interp,
                tkwin,
                C.Strings.To_Chars_Ptr (C_value'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_widgRec'Unchecked_Access),
                offset);
   end Tk_CanvasTagsParseProc;

   function Tk_CanvasTagsPrintProc
     (data        : ClientData;
      tkwin       : Tk_Window;
      widgRec     : String;
      offset      : C.int;
      freeProcPtr : Tcl_FreeProc)
      return        String
   is
      C_widgRec : aliased C.char_array := C.To_C (widgRec);
   begin --  Tk_CanvasTagsPrintProc
      return CHelper.Value
               (Tcl.Tk.Tk_CanvasTagsPrintProc
                   (data,
                    tkwin,
                    C.Strings.To_Chars_Ptr (C_widgRec'Unchecked_Access),
                    offset,
                    freeProcPtr));
   end Tk_CanvasTagsPrintProc;

   function Tk_ConfigureInfo
     (interp   : Tcl_Interp;
      tkwin    : Tk_Window;
      specs    : Tk_ConfigSpec;
      widgRec  : String;
      argvName : String;
      flags    : C.int)
      return     C.int
   is
      C_widgRec  : aliased C.char_array := C.To_C (widgRec);
      C_argvName : aliased C.char_array := C.To_C (argvName);
   begin --  Tk_ConfigureInfo
      return Tcl.Tk.Tk_ConfigureInfo
               (interp,
                tkwin,
                specs,
                C.Strings.To_Chars_Ptr (C_widgRec'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_argvName'Unchecked_Access),
                flags);
   end Tk_ConfigureInfo;

   function Tk_ConfigureValue
     (interp   : Tcl_Interp;
      tkwin    : Tk_Window;
      specs    : Tk_ConfigSpec;
      widgRec  : String;
      argvName : String;
      flags    : C.int)
      return     C.int
   is
      C_widgRec  : aliased C.char_array := C.To_C (widgRec);
      C_argvName : aliased C.char_array := C.To_C (argvName);
   begin --  Tk_ConfigureValue
      return Tcl.Tk.Tk_ConfigureValue
               (interp,
                tkwin,
                specs,
                C.Strings.To_Chars_Ptr (C_widgRec'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_argvName'Unchecked_Access),
                flags);
   end Tk_ConfigureValue;

   function Tk_ConfigureWidget
     (interp  : Tcl_Interp;
      tkwin   : Tk_Window;
      specs   : Tk_ConfigSpec;
      argc    : C.int;
      argv    : CArgv.Chars_Ptr_Ptr;
      widgRec : String;
      flags   : C.int)
      return    C.int
   is
      C_widgRec : aliased C.char_array := C.To_C (widgRec);
   begin --  Tk_ConfigureWidget
      return Tcl.Tk.Tk_ConfigureWidget
               (interp,
                tkwin,
                specs,
                argc,
                argv,
                C.Strings.To_Chars_Ptr (C_widgRec'Unchecked_Access),
                flags);
   end Tk_ConfigureWidget;

   function Tk_ComputeTextLayout
     (font       : Tk_Font;
      str        : String;
      numChars   : C.int;
      wrapLength : C.int;
      justify    : Tk_Justify;
      flags      : C.int;
      widthPtr   : access C.int;
      heightPtr  : access C.int)
      return       Tk_TextLayout
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_ComputeTextLayout
      return Tcl.Tk.Tk_ComputeTextLayout
               (font,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                numChars,
                wrapLength,
                justify,
                flags,
                widthPtr,
                heightPtr);
   end Tk_ComputeTextLayout;

   function Tk_CreateBinding
     (interp       : Tcl_Interp;
      bindingTable : Tk_BindingTable;
      object       : ClientData;
      eventStr     : String;
      command      : String;
      append       : C.int)
      return         C.unsigned_long
   is
      C_eventStr : aliased C.char_array := C.To_C (eventStr);
      C_command  : aliased C.char_array := C.To_C (command);
   begin --  Tk_CreateBinding
      return Tcl.Tk.Tk_CreateBinding
               (interp,
                bindingTable,
                object,
                C.Strings.To_Chars_Ptr (C_eventStr'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_command'Unchecked_Access),
                append);
   end Tk_CreateBinding;

   function Tk_CreateWindow
     (interp     : Tcl_Interp;
      parent     : Tk_Window;
      name       : String;
      screenName : String)
      return       Tk_Window
   is
      C_name       : aliased C.char_array := C.To_C (name);
      C_screenName : aliased C.char_array := C.To_C (screenName);
   begin --  Tk_CreateWindow
      return Tcl.Tk.Tk_CreateWindow
               (interp,
                parent,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_screenName'Unchecked_Access));
   end Tk_CreateWindow;

   function Tk_CreateWindowFromPath
     (interp     : Tcl_Interp;
      tkwin      : Tk_Window;
      pathName   : String;
      screenName : String)
      return       Tk_Window
   is
      C_pathName   : aliased C.char_array := C.To_C (pathName);
      C_screenName : aliased C.char_array := C.To_C (screenName);
   begin --  Tk_CreateWindowFromPath
      return Tcl.Tk.Tk_CreateWindowFromPath
               (interp,
                tkwin,
                C.Strings.To_Chars_Ptr (C_pathName'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_screenName'Unchecked_Access));
   end Tk_CreateWindowFromPath;

   function Tk_DefineBitmap
     (interp : Tcl_Interp;
      name   : String;
      source : String;
      width  : C.int;
      height : C.int)
      return   C.int
   is
      C_name   : aliased C.char_array := C.To_C (name);
      C_source : aliased C.char_array := C.To_C (source);
   begin --  Tk_DefineBitmap
      return Tcl.Tk.Tk_DefineBitmap
               (interp,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_source'Unchecked_Access),
                width,
                height);
   end Tk_DefineBitmap;

   function Tk_DeleteBinding
     (interp       : Tcl_Interp;
      bindingTable : Tk_BindingTable;
      object       : ClientData;
      eventStr     : String)
      return         C.int
   is
      C_eventStr : aliased C.char_array := C.To_C (eventStr);
   begin --  Tk_DeleteBinding
      return Tcl.Tk.Tk_DeleteBinding
               (interp,
                bindingTable,
                object,
                C.Strings.To_Chars_Ptr (C_eventStr'Unchecked_Access));
   end Tk_DeleteBinding;

   procedure Tk_DeleteImage (interp : Tcl_Interp; name : String) is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tk_DeleteImage
      Tcl.Tk.Tk_DeleteImage
        (interp,
         C.Strings.To_Chars_Ptr (C_name'Unchecked_Access));
   end Tk_DeleteImage;

   function Tk_DisplayName (tkwin : Tk_Window) return String is
   begin --  Tk_DisplayName
      return CHelper.Value (Tcl.Tk.Tk_DisplayName (tkwin));
   end Tk_DisplayName;

   function Tk_FindPhoto
     (interp    : Tcl_Interp;
      imageName : String)
      return      Tk_PhotoHandle
   is
      C_imageName : aliased C.char_array := C.To_C (imageName);
   begin --  Tk_FindPhoto
      return Tcl.Tk.Tk_FindPhoto
               (interp,
                C.Strings.To_Chars_Ptr (C_imageName'Unchecked_Access));
   end Tk_FindPhoto;

   function Tk_GetAnchor
     (interp    : Tcl_Interp;
      str       : String;
      anchorPtr : Tk_Anchor)
      return      C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetAnchor
      return Tcl.Tk.Tk_GetAnchor
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                anchorPtr);
   end Tk_GetAnchor;

   function Tk_GetBinding
     (interp       : Tcl_Interp;
      bindingTable : Tk_BindingTable;
      object       : ClientData;
      eventStr     : String)
      return         String
   is
      C_eventStr : aliased C.char_array := C.To_C (eventStr);
   begin --  Tk_GetBinding
      return CHelper.Value
               (Tcl.Tk.Tk_GetBinding
                   (interp,
                    bindingTable,
                    object,
                    C.Strings.To_Chars_Ptr (C_eventStr'Unchecked_Access)));
   end Tk_GetBinding;

   function Tk_GetCapStyle
     (interp : Tcl_Interp;
      str    : String;
      capPtr : access C.int)
      return   C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetCapStyle
      return Tcl.Tk.Tk_GetCapStyle
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                capPtr);
   end Tk_GetCapStyle;

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
      return   Tk_Cursor
   is
      C_source : aliased C.char_array := C.To_C (source);
      C_mask   : aliased C.char_array := C.To_C (mask);
   begin --  Tk_GetCursorFromData
      return Tcl.Tk.Tk_GetCursorFromData
               (interp,
                tkwin,
                C.Strings.To_Chars_Ptr (C_source'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_mask'Unchecked_Access),
                width,
                height,
                xHot,
                yHot,
                fg,
                bg);
   end Tk_GetCursorFromData;

   function Tk_GetFont
     (interp : Tcl_Interp;
      tkwin  : Tk_Window;
      str    : String)
      return   Tk_Font
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetFont
      return Tcl.Tk.Tk_GetFont
               (interp,
                tkwin,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access));
   end Tk_GetFont;

   function Tk_GetImage
     (interp     : Tcl_Interp;
      tkwin      : Tk_Window;
      name       : String;
      changeProc : Tk_ImageChangedProc;
      data       : ClientData)
      return       Tk_Image
   is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tk_GetImage
      return Tcl.Tk.Tk_GetImage
               (interp,
                tkwin,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                changeProc,
                data);
   end Tk_GetImage;

   function Tk_GetImageMasterData
     (interp     : Tcl_Interp;
      name       : String;
      typePtrPtr : Tk_ImageType)
      return       ClientData
   is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tk_GetImageMasterData
      return Tcl.Tk.Tk_GetImageMasterData
               (interp,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                typePtrPtr);
   end Tk_GetImageMasterData;

   function Tk_GetJoinStyle
     (interp  : Tcl_Interp;
      str     : String;
      joinPtr : access C.int)
      return    C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetJoinStyle
      return Tcl.Tk.Tk_GetJoinStyle
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                joinPtr);
   end Tk_GetJoinStyle;

   function Tk_GetJustify
     (interp     : Tcl_Interp;
      str        : String;
      justifyPtr : Tk_Justify)
      return       C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetJustify
      return Tcl.Tk.Tk_GetJustify
               (interp,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                justifyPtr);
   end Tk_GetJustify;

   function Tk_GetOption
     (tkwin     : Tk_Window;
      name      : String;
      className : String)
      return      Tk_Uid
   is
      C_name      : aliased C.char_array := C.To_C (name);
      C_className : aliased C.char_array := C.To_C (className);
   begin --  Tk_GetOption
      return Tcl.Tk.Tk_GetOption
               (tkwin,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                C.Strings.To_Chars_Ptr (C_className'Unchecked_Access));
   end Tk_GetOption;

   function Tk_GetPixels
     (interp : Tcl_Interp;
      tkwin  : Tk_Window;
      str    : String;
      intPtr : access C.int)
      return   C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetPixels
      return Tcl.Tk.Tk_GetPixels
               (interp,
                tkwin,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                intPtr);
   end Tk_GetPixels;

   function Tk_GetRelief
     (interp    : Tcl_Interp;
      name      : String;
      reliefPtr : access C.int)
      return      C.int
   is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tk_GetRelief
      return Tcl.Tk.Tk_GetRelief
               (interp,
                C.Strings.To_Chars_Ptr (C_name'Unchecked_Access),
                reliefPtr);
   end Tk_GetRelief;

   function Tk_GetScreenMM
     (interp    : Tcl_Interp;
      tkwin     : Tk_Window;
      str       : String;
      doublePtr : access C.double)
      return      C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetScreenMM
      return Tcl.Tk.Tk_GetScreenMM
               (interp,
                tkwin,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                doublePtr);
   end Tk_GetScreenMM;

   function Tk_GetUid (str : String) return Tk_Uid is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_GetUid
      return Tcl.Tk.Tk_GetUid
               (C.Strings.To_Chars_Ptr (C_str'Unchecked_Access));
   end Tk_GetUid;

   function Tk_MeasureChars
     (tkfont    : Tk_Font;
      source    : String;
      numBytes  : C.int;
      maxPixels : C.int;
      flags     : C.int;
      lengthPtr : access C.int)
      return      C.int
   is
      C_source : aliased C.char_array := C.To_C (source);
   begin --  Tk_MeasureChars
      return Tcl.Tk.Tk_MeasureChars
               (tkfont,
                C.Strings.To_Chars_Ptr (C_source'Unchecked_Access),
                numBytes,
                maxPixels,
                flags,
                lengthPtr);
   end Tk_MeasureChars;

   function Tk_NameOf3DBorder (border : Tk_3DBorder) return String is
   begin --  Tk_NameOf3DBorder
      return CHelper.Value (Tcl.Tk.Tk_NameOf3DBorder (border));
   end Tk_NameOf3DBorder;

   function Tk_NameOfAnchor (anchor : Tk_Anchor) return String is
   begin --  Tk_NameOfAnchor
      return CHelper.Value (Tcl.Tk.Tk_NameOfAnchor (anchor));
   end Tk_NameOfAnchor;

   function Tk_NameOfCapStyle (cap : C.int) return String is
   begin --  Tk_NameOfCapStyle
      return CHelper.Value (Tcl.Tk.Tk_NameOfCapStyle (cap));
   end Tk_NameOfCapStyle;

   function Tk_NameOfFont (font : Tk_Font) return String is
   begin --  Tk_NameOfFont
      return CHelper.Value (Tcl.Tk.Tk_NameOfFont (font));
   end Tk_NameOfFont;

   function Tk_NameOfImage (imageMaster : Tk_ImageMaster) return String is
   begin --  Tk_NameOfImage
      return CHelper.Value (Tcl.Tk.Tk_NameOfImage (imageMaster));
   end Tk_NameOfImage;

   function Tk_NameOfJoinStyle (join : C.int) return String is
   begin --  Tk_NameOfJoinStyle
      return CHelper.Value (Tcl.Tk.Tk_NameOfJoinStyle (join));
   end Tk_NameOfJoinStyle;

   function Tk_NameOfJustify (justify : Tk_Justify) return String is
   begin --  Tk_NameOfJustify
      return CHelper.Value (Tcl.Tk.Tk_NameOfJustify (justify));
   end Tk_NameOfJustify;

   function Tk_NameOfRelief (relief : C.int) return String is
   begin --  Tk_NameOfRelief
      return CHelper.Value (Tcl.Tk.Tk_NameOfRelief (relief));
   end Tk_NameOfRelief;

   function Tk_NameToWindow
     (interp   : Tcl_Interp;
      pathName : String;
      tkwin    : Tk_Window)
      return     Tk_Window
   is
      C_pathName : aliased C.char_array := C.To_C (pathName);
   begin --  Tk_NameToWindow
      return Tcl.Tk.Tk_NameToWindow
               (interp,
                C.Strings.To_Chars_Ptr (C_pathName'Unchecked_Access),
                tkwin);
   end Tk_NameToWindow;

   function Tk_SetAppName
     (tkwin : Tk_Window;
      name  : String)
      return  String
   is
      C_name : aliased C.char_array := C.To_C (name);
   begin --  Tk_SetAppName
      return CHelper.Value
               (Tcl.Tk.Tk_SetAppName
                   (tkwin,
                    C.Strings.To_Chars_Ptr (C_name'Unchecked_Access)));
   end Tk_SetAppName;

   procedure Tk_SetClass (tkwin : Tk_Window; className : String) is
      C_className : aliased C.char_array := C.To_C (className);
   begin --  Tk_SetClass
      Tcl.Tk.Tk_SetClass
        (tkwin,
         C.Strings.To_Chars_Ptr (C_className'Unchecked_Access));
   end Tk_SetClass;

   function Tk_TextWidth
     (font     : Tk_Font;
      str      : String;
      numBytes : C.int)
      return     C.int
   is
      C_str : aliased C.char_array := C.To_C (str);
   begin --  Tk_TextWidth
      return Tcl.Tk.Tk_TextWidth
               (font,
                C.Strings.To_Chars_Ptr (C_str'Unchecked_Access),
                numBytes);
   end Tk_TextWidth;

   procedure Tk_FreeConfigOptions
     (recordPtr   : String;
      optionToken : Tk_OptionTable;
      tkwin       : Tk_Window)
   is
      C_recordPtr : aliased C.char_array := C.To_C (recordPtr);
   begin --  Tk_FreeConfigOptions
      Tcl.Tk.Tk_FreeConfigOptions
        (C.Strings.To_Chars_Ptr (C_recordPtr'Unchecked_Access),
         optionToken,
         tkwin);
   end Tk_FreeConfigOptions;

   function Tk_GetOptionInfo
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionTable : Tk_OptionTable;
      namePtr     : Tcl_Obj;
      tkwin       : Tk_Window)
      return        Tcl_Obj
   is
      C_recordPtr : aliased C.char_array := C.To_C (recordPtr);
   begin --  Tk_GetOptionInfo
      return Tcl.Tk.Tk_GetOptionInfo
               (interp,
                C.Strings.To_Chars_Ptr (C_recordPtr'Unchecked_Access),
                optionTable,
                namePtr,
                tkwin);
   end Tk_GetOptionInfo;

   function Tk_GetOptionValue
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionTable : Tk_OptionTable;
      namePtr     : Tcl_Obj;
      tkwin       : Tk_Window)
      return        Tcl_Obj
   is
      C_recordPtr : aliased C.char_array := C.To_C (recordPtr);
   begin --  Tk_GetOptionValue
      return Tcl.Tk.Tk_GetOptionValue
               (interp,
                C.Strings.To_Chars_Ptr (C_recordPtr'Unchecked_Access),
                optionTable,
                namePtr,
                tkwin);
   end Tk_GetOptionValue;

   function Tk_InitOptions
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionToken : Tk_OptionTable;
      tkwin       : Tk_Window)
      return        C.int
   is
      C_recordPtr : aliased C.char_array := C.To_C (recordPtr);
   begin --  Tk_InitOptions
      return Tcl.Tk.Tk_InitOptions
               (interp,
                C.Strings.To_Chars_Ptr (C_recordPtr'Unchecked_Access),
                optionToken,
                tkwin);
   end Tk_InitOptions;

   function Tk_SetOptions
     (interp      : Tcl_Interp;
      recordPtr   : String;
      optionTable : Tk_OptionTable;
      objc        : C.int;
      objv        : Tcl_Obj_Array;
      tkwin       : Tk_Window;
      savePtr     : Tk_SavedOptions;
      maskPtr     : access C.int)
      return        C.int
   is
      C_recordPtr : aliased C.char_array := C.To_C (recordPtr);
   begin --  Tk_SetOptions
      return Tcl.Tk.Tk_SetOptions
               (interp,
                C.Strings.To_Chars_Ptr (C_recordPtr'Unchecked_Access),
                optionTable,
                objc,
                objv,
                tkwin,
                savePtr,
                maskPtr);
   end Tk_SetOptions;

   function Tk_GetDash
     (interp : Tcl_Interp;
      value  : String;
      dash   : Tk_Dash)
      return   C.int
   is
      C_value : aliased C.char_array := C.To_C (value);
   begin --  Tk_GetDash
      return Tcl.Tk.Tk_GetDash
               (interp,
                C.Strings.To_Chars_Ptr (C_value'Unchecked_Access),
                dash);
   end Tk_GetDash;

end Tcl.Tk.Ada;
