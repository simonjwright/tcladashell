--------------------------------------------------------------------
--
--  Unit Name:    Tash.Regexp spec
--
--  File Name:    tash-regexp.ads
--
--  Purpose:      Provides regular expression pattern matching
--               on Tash strings and lists.
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

with Ada.Strings;
with Tash.Lists;

package Tash.Regexp is

   Regexp_Error : exception;

   --------------------------------------------------
   --  A Tash_Regexp is a compiled regexp which is
   --  faster in regular expression pattern matching
   --  than just using a string.
   --------------------------------------------------

   type Tash_Regexp is new Tash.Tash_Object with null record;

   --------------------------------------------------------
   --  Create a Tash_Regexp
   --------------------------------------------------------

   function To_Tash_Regexp (Str : in String) return Tash_Regexp;

   function "+" (Str : in String) return Tash_Regexp;

   --------------------------------------------------------
   --  Convert a Tash regexp to a string.  Returns an emtpy
   --  string for an uninitialized Tash_Regexp.
   --------------------------------------------------------

   function To_String (Regexp : in Tash_Regexp) return String;

   ----------------------------------------------------------------
   --  Regular expression modes.  See the Tcl re_syntax(n) man page.
   ----------------------------------------------------------------

   type Regexp_Mode is (
      Basic,    --  Regexp syntax in common Unix utilities such as sed and grep
      Extended, --  Regexp syntax in Tcl 8.0 and earlier
      Advanced, --  Normal regexp syntax of Tcl regexp and regsub commands
      Quote     --  No special characters
     );

   -----------------------------------------------------------------------
   --  Match returns True if Source matches the regular expression, Pattern.
   --
   --  Setting Expanded to True enables use of the expanded regular
   --  expression syntax where whitespace and comments are ignored.
   --
   --  Setting Ignore_Case to True causes upper-case characters in string to
   --  be treated as lower case during the matching process.
   --
   --  Setting Line to True enables  newline-sensitive matching.  By
   --  default, newline is a completely ordinary character with no special
   --  meaning. With this flag, `[^' bracket expressions and `.' never
   --  match newline, `^' matches an empty string after any newline in
   --  addition to its normal function, and `$' matches an empty string
   --  before any newline in addition to its normal function. This flag is
   --  equivalent to setting both Line_Stop and Line_Anchor True.
   --
   --  Setting Line_Stop to True changes  the  behavior of `[^' bracket
   --  expressions and `.' so that they stop at newlines.
   --
   --  Setting Line_Anchor to True changes the behavior of `^' and `$'
   --  (the ``anchors'') so they match the beginning and end of a line
   --  respectively.
   --
   --  For more information on regular expressions, See the Tcl
   --  re_syntax(n) man page.
   --
   --  Raises Regexp_Error if Pattern is not a valid regular expression.
   --------------------------------------------------------------------

   function Match
     (Source      : in String;
      Pattern     : in String;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Boolean;

   function Match
     (Source      : in String;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Boolean;

   -------------------------------------------------------------------
   --  Match_Element returns the index of the element of Source (in the
   --  range 1..Length(Source)) which matches the regular expression,
   --  Pattern.  Returns 0 if Pattern is not found in any of the elements.
   --  Going provides the capability to search either forward from the
   --  first element to the last or backward from the last element to
   --  the first.
   --
   --  Note that the string representation of each element in Source
   --  is searched for Pattern.
   --
   --  Raises Regexp_Error if Pattern is not a valid regular expression.
   -------------------------------------------------------------------

   function Match_Element
     (Source      : in Tash.Lists.Tash_List;
      Pattern     : in String;
      Going       : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mode        : in Regexp_Mode           := Advanced;
      Expanded    : in Boolean               := False;
      Ignore_Case : in Boolean               := False;
      Line        : in Boolean               := False;
      Line_Stop   : in Boolean               := False;
      Line_Anchor : in Boolean               := False)
      return        Natural;

   function Match_Element
     (Source      : in Tash.Lists.Tash_List;
      Pattern     : in Tash_Regexp;
      Going       : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mode        : in Regexp_Mode           := Advanced;
      Expanded    : in Boolean               := False;
      Ignore_Case : in Boolean               := False;
      Line        : in Boolean               := False;
      Line_Stop   : in Boolean               := False;
      Line_Anchor : in Boolean               := False)
      return        Natural;

   ------------------------------------------------------------
   --  This version of Match also searches for Pattern in Source.
   --  However, instead of returning True, it returns the portions
   --  of the string which matched the whole Pattern and the
   --  parenthesized subexpressions within Pattern.
   --
   --  The first element of the returned Tash list is the portion
   --  of Source matched by the whole Pattern.  The subsequent
   --  elements of the returned list are the portions of Source
   --  which match the parenthesized subexpressions in Pattern.
   --
   --  If there was no match, the returned Tash list is empty.
   --
   --  Raises Regexp_Error if Pattern is not a valid regular expression.
   --
   --  Example:
   --    Call Match to find the root and extension portions of
   --    a file name:
   --
   --    declare
   --       L : Tash.Lists.Tash_List;
   --    begin
   --       L := Match ("/etc/rc2.d", "/([^/]*)\.([^/]*)$");
   --
   --       L is a 3-element Tash list which contains
   --       the elements: "/rc2.d", "rc2", and "d".
   -------------------------------------------------------------

   function Match
     (Source      : in String;
      Pattern     : in String;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Tash.Lists.Tash_List;

   function Match
     (Source      : in String;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Tash.Lists.Tash_List;

   --------------------------------------------------------------------
   --  Substitute matches the regular expression Pattern against
   --  Source.  Regular expression  matching is described in the
   --  Tcl re_syntax(n) man page.  If there is a match, Source is
   --  returned with the portion that matched Pattern replaced with
   --  Sub_Spec.  If Sub_Spec contains a "&" or "\0", then it is replaced
   --  in the substitution with the portion of string that matched
   --  Pattern.  If Sub_Spec contains a "\n", where n is a digit
   --  between 1 and 9, then it is replaced in the substitution
   --  with the portion of string that matched the n-th parenthe-
   --  sized subexpression of Pattern.  Additional backslashes may be
   --  used in Sub_Spec to prevent special interpretation of "&"
   --  or "\0" or "\n" or backslash.
   --
   --  Advanced Regexp_Mode is always used for the pattern matching.
   --
   --  Normally, only the first occurrence of Pattern is substituted.
   --  Setting Sub_All to True finds and substitutes all occurrences
   --  of Pattern.  Special characters "&", "\0", and "\n" are handled
   --  for each substitution using the information from the
   --  corresponding match.
   --
   --  Use of Ignore_Case, Line, Line_Stop, and Line_Anchor in
   --  performing the match is the same as for function Match above.
   --
   --  Raises Regexp_Error if Pattern is not a valid regular expression.
   --------------------------------------------------------------------

   function Substitute
     (Source      : in String;
      Pattern     : in String;
      Sub_Spec    : in String;
      Sub_All     : in Boolean := False;
      Expanded    : in Boolean := False;
      Ignore_Case : in Boolean := False;
      Line        : in Boolean := False;
      Line_Stop   : in Boolean := False;
      Line_Anchor : in Boolean := False)
      return        String;

end Tash.Regexp;
