--------------------------------------------------------------------
--
--  Unit Name:    Tash.Floats spec
--
--  File Name:    tash-floats.ads
--
--  Purpose:      This package exports a Tash float type along with
--               its operations.
--
--  Copyright (c) 1999 Terry J. Westley
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

with Ada.Finalization;

package Tash.Floats is

   --------------------------------------------------------
   --  A Tash float is derived from a Tash object.
   --
   --  When used in an expression, an uninitialized Tash float
   --  evaluates to 0.0.
   --------------------------------------------------------

   type Tash_Float is new Tash.Tash_Object with null record;

   Null_Tash_Float : constant Tash_Float;

   -----------------------------------------------
   --  Inherits Is_Null function from Tash.  It
   --  returns True if TFloat has not been initialized
   --  or has been set to Null_Tash_Float.
   -----------------------------------------------

   --  function Is_Null (
   --   TFloat : in Tash_Float) return Boolean;

   -----------------------------------------------
   --  Returns True if Is_Null(TFloat) is true or
   --  To_Float(TFloat) = 0.0.
   -----------------------------------------------

   function Is_Zero (TFloat : in Tash_Float) return Boolean;

   -----------------------------------------------
   --  Convert a string to a Tash float.
   --  Raises Constraint_Error if number is invalid
   --  or outside the range of Tash_Float.
   -----------------------------------------------

   function To_Tash_Float (Str : in String) return Tash_Float;

   --------------------------------------------------------
   --  Convert a Tash float to a string.  Returns "0.0" for
   --  an uninitialized Tash_Float.
   --
   --  Recall that all non-string Tash data types have a dual
   --  representation.  At any time, you may fetch either the
   --  string representation or the native (i.e. Float)
   --  representation.  The string representation is updated
   --  to correspond with the native data type only when the
   --  string is fetched.
   --------------------------------------------------------

   function To_String (TFloat : in Tash_Float) return String;

   --------------------------------------------------------
   --  Convert Ada floats to and from Tash floats.
   --------------------------------------------------------

   subtype Tash_Float_Range is Long_Float range
      Long_Float (Interfaces.C.double'First) ..
      Long_Float (Interfaces.C.double'Last);
   --  This subtype will not compile if standard Ada Long_Float range
   --  is smaller than Tash float range.  It is also used to assure
   --  that we don't attempt to convert Ada floats outside the Tash
   --  float range to/from Tash floats.

   function To_Tash_Float (LFloat : in Tash_Float_Range) return Tash_Float;

   function "+" (LFloat : in Tash_Float_Range) return Tash_Float;

   function "-" (LFloat : in Tash_Float_Range) return Tash_Float;

   function To_Float (TFloat : in Tash_Float) return Tash_Float_Range;

   --------------------------------------------------------
   --  Compare Tash and standard Ada floats
   --------------------------------------------------------

   function "="
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean;

   function "="
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean;

   function "="
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean;

   function "<"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean;

   function "<"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean;

   function "<"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean;

   function "<="
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean;

   function "<="
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean;

   function "<="
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean;

   function ">"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean;

   function ">"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean;

   function ">"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean;

   function ">="
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Boolean;

   function ">="
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Boolean;

   function ">="
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Boolean;

   --------------------------------------------------------
   --  Tash float arithmetic operations
   --------------------------------------------------------

   function "abs" (Right : in Tash_Float) return Tash_Float;

   function "+"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float;

   function "+"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float;

   function "+"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float;

   function "-"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float;

   function "-"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float;

   function "-"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float;

   function "*"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float;

   function "*"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float;

   function "*"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float;

   function "/"
     (Left  : in Tash_Float;
      Right : in Tash_Float)
      return  Tash_Float;

   function "/"
     (Left  : in Tash_Float;
      Right : in Tash_Float_Range)
      return  Tash_Float;

   function "/"
     (Left  : in Tash_Float_Range;
      Right : in Tash_Float)
      return  Tash_Float;

   function "**"
     (Left  : in Tash_Float;
      Right : in Integer)
      return  Tash_Float;

private

   Null_Tash_Float : constant Tash_Float := (Ada.Finalization.Controlled with
                                             Obj => null);

   Verbose : Boolean := False;

end Tash.Floats;
