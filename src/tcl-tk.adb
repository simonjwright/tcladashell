--------------------------------------------------------------------
--
-- tcl-tk.adb --
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

package body Tcl.Tk is
   function Is_Null (Ptr : in Tk_BindingTable) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Canvas) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Cursor) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_ErrorHandler) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Font) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Image) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_ImageMaster) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_OptionTable) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_PostscriptInfo) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_TextLayout) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Window) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_3DBorder) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_OptionSpec) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_SavedOption) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_SavedOptions) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_CustomOption) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_ConfigSpec) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_ArgvInfo) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_FontMetrics) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_GeomMgr) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in XVirtualEvent) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in XActivateDeactivateEvent) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_FakeWin) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_SmoothMethod) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Item) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_ItemType) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_CanvasTextInfo) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Dash) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_TSOffset) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_Outline) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_ImageType) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_PhotoHandle) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_PhotoImageBlock) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

   function Is_Null (Ptr : in Tk_PhotoImageFormat) return Boolean is
   begin --  Is_Null
      return Ptr = null;
   end Is_Null;

end Tcl.Tk;
