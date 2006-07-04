--------------------------------------------------------------------
--
--  Unit Name:    Tash spec
--
--  File Name:    tash.ads
--
--  Purpose:      This package is the root of a family of packages
--                which implement a binding to Tcl.  Specifically,
--                this package contains the parent Tash data type,
--                Tash_Object.
--
--  Copyright (c) 1999-2000 Terry J. Westley
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
--  TASH is organized into the following packages:
--
--  Package Name                 Purpose
-- ------------                 -------
--  Tash.Strings                 Defines the Tash string type and provides
--                              operations supporting string manipulation
--                              in the style of Ada.Strings packages.
--
--  Tash.Integers                Defines the Tash integer type.
--
--  Tash.Floats                  Defines the Tash float type.
--
--  Tash.Lists                   Defines the Tash list type which may
--                              contain any Tash object, including lists.
--
--  Tash.Regexp                  Provides regular expression pattern matching
--                              for Tash and Ada strings.
--
--  Tash.Format                  Provides C printf-style output formatting.
--
--------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Finalization;
with Interfaces.C;
with Tcl;

package Tash is

   Tcl_Error : exception;

   --------------------------------------------------------
   --  Tash_Object is the base type of all Tash types.  It is
   --  a tagged type and is controlled.  The underlying Tcl
   --  object is reference counted.
   --------------------------------------------------------

   type Tash_Object is abstract tagged private;

   --------------------------------------------------
   --  Is_Null returns true if TObject has not yet been
   --  initialized (or has been set to a null Tash
   --  object such as Tash.Strings.Null_Tash_String).
   --
   --  In other words, Is_Null is short-hand for
   --  Is_Uninitialized.
   --------------------------------------------------

   function Is_Null (TObject : in Tash_Object) return Boolean;

   --------------------------------------------------------
   --  All Tash objects (including integers, floats, and lists)
   --  have a string representation.  To_String fetches the
   --  string representation of a Tash object.
   --
   --  String representations are updated lazily, that is, only
   --  when needed.  For example, as long as the string
   --  representation of a float is not fetched, it will not be
   --  maintained although many float operations may be performed
   --  and the value of the float changes many times.  When the
   --  value of the float is changed, the internal string
   --  representation is marked as out of date.  On the next call
   --  to fetch the string representation, it will be computed.
   --------------------------------------------------------

   function To_String (TObject : in Tash_Object) return String is abstract;

   --------------------------------------------------------
   --  Following are several subprograms for examining the
   --  internal representation of a Tash object.  They are
   --  primarily used in verification of the Tash interface.
   --------------------------------------------------------

   function Internal_Rep (TObject : in Tash_Object) return String;
   --  Returns an image of the internal representation,
   --  including the string representation, tag, and
   --  reference count.

   function Type_Of (TObject : in Tash_Object'Class) return String;
   --  Returns the underlying Tcl type of TObject.

   function Ref_Count (TObject : in Tash_Object'Class) return Natural;
   --  Returns the Tcl_Obj reference count.  Returns 0 if TObject
   --  is not initialized (see Is_Null above).

   procedure PrintObj (TObject : in Tash_Object'Class);
   --  Calls Tcl.Tcl_PrintObj

private

   type Tash_Object is abstract new Ada.Finalization.Controlled with record
      Obj : Tcl.Tcl_Obj;
   end record;

   procedure Finalize (Obj : in out Tcl.Tcl_Obj);
   procedure Finalize (TObject : in out Tash_Object);
   procedure Adjust (TObject : in out Tash_Object);

   protected Tash_Interp is

      entry Get (Interp : out Tcl.Tcl_Interp);
      --  Gets the Tcl interpreter reference and seizes
      --  a semaphore to guarantee sequentialized access.

      procedure Release (Interp : in Tcl.Tcl_Interp);
      --  Releases the interpreter semaphore.

      procedure Assert
        (Interp      : in Tcl.Tcl_Interp;
         Return_Code : in Interfaces.C.int;
         E           : in Ada.Exceptions.Exception_Id);
      --  Raises indicated exception if Return_Code = Tcl.TCL_ERROR.
      --  Uses the string result of Interp as the message.
      --  Releases the semaphore only if exception is raised.

      procedure Raise_Exception
        (Interp : in Tcl.Tcl_Interp;
         E      : in Ada.Exceptions.Exception_Id);
      --  Raises indicated exception.
      --  Uses the string result of Interp as the message.
      --  Releases the semaphore only if exception is raised.

      procedure Raise_Exception
        (Interp  : in Tcl.Tcl_Interp;
         E       : in Ada.Exceptions.Exception_Id;
         Message : in String);
      --  Raises indicated exception.
      --  Releases the semaphore only if exception is raised.

   private

      Tcl_Interp : Tcl.Tcl_Interp;
      Seized     : Boolean := False;

      pragma Inline (Release, Assert, Raise_Exception);

   end Tash_Interp;

   Verbose : Boolean := False;

   function Image (TObject : in Tcl.Tcl_Obj) return String;

   function Internal_Rep (TObj : in Tcl.Tcl_Obj) return String;

   function To_Tcl_Obj (Str : in String) return Tcl.Tcl_Obj;

   function To_Tcl_Obj (Num : in Integer) return Tcl.Tcl_Obj;

end Tash;
