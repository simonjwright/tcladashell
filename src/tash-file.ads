-------------------------------------------------------------------
--
--  Unit Name:    Tash.File spec
--
--  File Name:    tash-file.ads
--
--  Purpose:      Provides file information and manipulation routines.
--
--  Copyright (c) 2000 Terry J. Westley
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
--        Tcl Command Cross Reference
--
--    Tcl command               TASH Ada subprogram
--    -----------               -------------------
--    file atime                Get_Access_Time, Set_Access_Time
--    file attributes           Get_Attribute,  Set_Attribute
--    file copy                 Copy
--    file delete               Delete
--    file dirname              Directory_Name, Dirname
--    file executable           Executable
--    file exists               Exists
--    file extension            Extension
--    file isdirectory          Is_Directory
--    file isfile               Is_File
--    file join                 Join
--    file lstat                Not Implemented
--    file mkdir                Make_Directory, Mkdir
--    file mtime                Get_Modified_Time, Set_Modified_Time
--    file nativename           Native_Name
--    file owned                Owned
--    file pathtype             Get_Path_Type
--    file readable             Readable
--    file readlink             Read_Link
--    file rename               Rename
--    file rootname             Root_Name
--    file size                 Size
--    file split                Split
--    file stat                 Not Implemented
--    file tail                 Tail
--    file type                 Type
--    file volume               Volume
--    file writable             Writable
--    cd                        Change_Directory, cd
--    glob                      Match, glob
--    pwd                       Get_Working_Directory, pwd
--
--------------------------------------------------------------------
--
--  This section describes Tash (really, Tcl) file name conventions.
--
--  INTRODUCTION
--  -============
--  All Tash subprograms that take file names as parameters expect
--  the file names to be in one of three forms, depending on the
--  current platform.  On each platform, Tcl supports file names
--  in the standard forms(s) for that platform.  In addition, on
--  all platforms, Tcl supports a Unix-like syntax intended to
--  provide a convenient way of constructing simple file names.
--  However, programs that are intended to be portable should not
--  assume a particular form for file names.  Instead, portable
--  programs must use the Split and Join subprograms to manipulate
--  file names.
--
--  PATH SYNTAX
--  -===========
--  The rules for native names depend on the value reported in the Tcl
--  array element tcl_platform(platform):
--
--  tcl_platform(platform) = "mac"
---------------------------------
--  This platform is reported on Mac OS 9 (and possibly earlier). Mac
--  OS X reports "unix" (see below).
--
--  On these systems, Tcl supports two forms of path names.  The
--  normal Mac style names use colons as path separators.  Paths may
--  be relative or absolute, and file names may contain any character
--  other than colon.  A leading colon causes the rest of the path to
--  be interpreted relative to the current directory.  If a path
--  contains a colon that is not at the beginning, then the path is
--  interpreted as an absolute path.  Sequences of two or more colons
--  anywhere in the path are used to construct relative paths where ::
--  refers to the parent of the current directory, ::: refers to the
--  parent of the parent, and so forth.
--
--  In addition to Macintosh style names, Tcl also supports a subset of
--  Unix-like names.  If a path contains no colons, then it is interpreted
--  like a Unix path.  Slash is used as the path separator.  The file name
--  . (dot) refers to the current directory, and .. refers to the parent
--  of the current directory.  However, some names like / or /.. have no
--  mapping, and are interpreted as Macintosh names.  In general, commands
--  that generate file names will return Macintosh style names, but commands
--  that accept file names will take both Macintosh and Unix-style names.
--
--  The following examples illustrate various forms of path names:
--
--  :               Relative path to the current folder.
--  MyFile          Relative path to a file named MyFile in the current folder.
--  MyDisk:MyFile   Absolute path to a file named MyFile on the device Named
--                    MyDisk.
--  :MyDir:MyFile   Relative path to a file name MyFile in a folder named
--                    MyDir in the current folder.
--  ::MyFile        Relative path to a file named MyFile in the folder above
--                    the current folder.
--  :::MyFile       Relative path to a file named MyFile in the folder two
--                    levels above the current folder.
--  /MyDisk/MyFile  Absolute path to a file named MyFile on the device named
--                    MyDisk.
--  ../MyFile       Relative path to a file named MyFile in the folder above
--                    the current folder.
--
--  tcl_platform(platform) = "unix"
----------------------------------
--  On Unix platforms, Tcl uses path names where the components are separated
--  by slashes.  Path names may be relative or absolute, and file names may
--  contain any character other than slash.  The file names . and .. are
--  special and refer to the current directory and the parent of the current
--  directory respectively.  Multiple adjacent slash characters are interpreted
--  as a single separator. The following examples illustrate various forms of
--  path names:
--
--  /               Absolute path to the root directory.
--  /etc/passwd     Absolute path to the file named passwd in the directory
--                    etc in the root directory.
--  .               Relative path to the current directory.
--  foo             Relative path to the file foo in the current directory.
--  foo/bar         Relative path to the file bar in the directory foo in the
--                    current directory.
--  ../foo          Relative path to the file foo in the directory above the
--                    current directory.
--
--  tcl_platform(platform) = "windows"
-------------------------------------
--  On Microsoft Windows platforms, Tcl supports both drive-relative and UNC
--  style names.  Both / and \ may be used as directory separators in either
--  type of name.  Drive-relative names consist of an optional drive specifier
--  followed by an absolute or relative path.  UNC paths follow the general
--  form \\servername\sharename\path\file.  In both forms, the file names
--  . and .. are special and refer to the current directory and the parent
--  of the current directory respectively.  The following examples illustrate
--  various forms of path names:
--
--  \\Host\share/file   Absolute UNC path to a file called file in the
--                        root directory of the export point share on
--                        the host Host.
--  c:foo               VolumeRelative path to a file foo in the current
--                        directory on drive c.
--  c:/foo              Absolute path to a file foo in the root directory
--                        of drive c.
--  foo\bar             Relative path to a file bar in the foo directory
--                        in the current directory on the current volume.
--  \foo                VolumeRelative path to a file foo in the root
--                        directory of the current volume.
--
--  TILDE SUBSTITUTION
--  -==================
--  In addition to the file name rules described above, Tcl also supports
--  csh-style tilde substitution.  If a file name starts with a tilde,
--  then the file name will be interpreted as if the first element is
--  replaced with the location of the home directory for the given user.
--  If the tilde is followed immediately by a separator, then the $HOME
--  environment variable is substituted.  Otherwise the characters between
--  the tilde and the next separator are taken as a user name, which is
--  used to retrieve the user's home directory for substitution.
--
--  The Macintosh (OS 9) and Windows platforms do not support tilde
--  substitution when a user name follows the tilde.  On these
--  platforms, attempts to use a tilde followed by a user name will
--  generate an error.  File names that have a tilde without a user
--  name will be substituted using the $HOME environment variable,
--  just like for Unix.
--
--  PORTABILITY ISSUES
--  -==================
--  Not all file systems are case sensitive, so programs should avoid code
--  that depends on the case of characters in a file name.  In addition,
--  the character sets allowed on different devices may differ, so programs
--  should choose file names that do not contain special characters like:
--  <>:"/\|.  The safest approach is to use names consisting of alphanumeric
--  characters only.  Also Windows 3.1 only supports file names with a root
--  of no more than 8 characters and an extension of no more than 3
--  characters.
--
----------------------------------------------------------------------------

with Ada.Calendar;
with Tash.Lists;

package Tash.File is

   No_Such_File : exception;
   Time_Format_Error : exception;
   File_Already_Exists : exception;
   Could_Not_Read_Link : exception;

   function Get_Access_Time (Name : in String) return Ada.Calendar.Time;
   --  Gets the time at which the file was last accessed.
   --  Exceptions:
   --    No_Such_File is raised if the file does not exist.
   --    Time_Format_Error is raised if the access time cannot
   --       be converted to Ada calendar time.
   --  Portability Note:
   --    On Windows, FAT file systems do not support access time.

   procedure Set_Access_Time
     (Name : in String;
      Date : in Ada.Calendar.Time);
   --  Sets the time at which the file was last accessed.
   --  Exceptions:
   --    No_Such_File is raised if the file does not exist.
   --    Time_Format_Error is raised if the access time cannot
   --       be converted from Ada calendar time.
   --  Portability Note:
   --    On Windows, FAT file systems do not support access time.

   type Attribute is (
      Group,        --  Unix:    group name of a file
      Owner,        --  Unix:    user name of the owner of a file
      Permissions,  --  Unix:    permissions as octal code used by chmod(1)
      Archive,      --  Win:     archive attribute of a file ("0" or "1")
      Hidden,       --  Win/Mac: hidden attribute of a file ("0" or "1")
      LongName,      --  Win:     expands each path element to its long version
                     --          (this attribute cannot be set)
      ReadOnly,     --  Win/Mac: readonly attribute of a file ("0" or "1")
      ShortName,     --  Win:     returns string where each path element
                     --          is replaced with its short (8.3) version
                     --          of the name (this attribute cannot be set)
      SystemAttr,   --  Win:     system attribute of a file ("0" or "1")
      Creator,      --  Mac:     Finder creator type of the file
      FType         --  Mac:     Finder file type of file
     );

   function Get_Attribute
     (Name : in String;
      Attr : in Attribute)
      return String;
   --  Gets the specified attribute of the file.
   --  Exceptions:
   --    No_Such_File is raised if the file does not exist.
   --  Portability Note:
   --    Getting an attribute not supported on the platform returns
   --    an empty string

   procedure Set_Attribute
     (Name  : in String;
      Attr  : in Attribute;
      Value : in String);
   --  Sets the specified attribute of the file.
   --  Exceptions:
   --    No_Such_File is raised if the file does not exist.
   --  Portability Note:
   --    Setting an attribute not supported on the platform
   --    has no effect (good or bad!).

   procedure Copy
     (Source : in String;
      Target : in String;
      Force  : in Boolean := False);
   --  Copies Source file or directory to Target file or directory.
   --  If Target is a directory name:
   --    If Source is a file, it is copied into Target directory.
   --    If Source is a directory, its contents are copied recursively
   --       into Target directory.
   --  If Target is a file name or does not exist,
   --    If Source is a file, it is copied to Target.
   --    If source is a directory, an exception is raised
   --  Existing files will be overwritten only if Force = True.
   --  Exceptions:
   --    No_Such_File is raised if Source does not exist.
   --    File_Already_Exists is raised if Target already exists and
   --       Force = False.

   procedure Delete (Name : in String; Force : in Boolean := False);
   --  Removes the specified file or directory.
   --  Non-empty directories will be removed only if Force = True.
   --  Trying to delete a non-existant file is not considered an error.
   --  Trying to delete a read-only file will cause the file to be deleted,
   --  even if Force = False.

   function Directory_Name (Name : in String) return String;
   function Dirname (Name : in String) return String renames Directory_Name;
   --  Returns a name comprised of all of the path components in Name
   --  excluding the last element.  If name is a relative file name and
   --  only contains one path element, then returns "." (or ":" on the
   --  Macintosh).  If name refers to a root directory, then the root
   --  directory is returned.  For example, Directory_Name ("c:/")
   --  returns "c:/".  Note that tilde substitution will only be performed
   --  if it is necessary to complete the command.  For example,
   --  Directory_Name ("~/src/foo.c") returns "~/src", whereas
   --  Directory_Name ("~") returns "/home" (or something similar).

   function Executable (Name : in String) return Boolean;
   --  Returns True if file Name is executable by the current user,
   --  False otherwise.

   function Exists (Name : in String) return Boolean;
   --  Returns True if file Name exists and the current user has search
   --  privileges for the directories leading to it, False otherwise.

   function Extension (Name : in String) return String;
   --  Returns all of the characters in Name after and including the
   --  last dot in the last element of Name.  If there is no dot in
   --  the last element of name then returns the empty string.

   function Is_Directory (Name : in String) return Boolean;
   --  Returns True if file Name is a directory, False otherwise.

   function Is_File (Name : in String) return Boolean;
   --  Returns True if file Name is a regular file, False otherwise.

   function Join
     (Name1 : in String;
      Name2 : in String := "";
      Name3 : in String := "";
      Name4 : in String := "";
      Name5 : in String := "";
      Name6 : in String := "";
      Name7 : in String := "";
      Name8 : in String := "";
      Name9 : in String := "")
      return  String;
   function Join (Name_List : in Tash.Lists.Tash_List) return String;
   --  Takes one or more file names and combines them, using the correct
   --  path separator for the current platform.  If a particular name is
   --  relative, then it will be joined to the previous file name argument.
   --  Otherwise, any earlier arguments will be discarded, and joining will
   --  proceed from the current argument.  For example,
   --  Join ("a", "b", "/foo", "bar") returns "/foo/bar".
   --  Note that any of the names can contain separators, and that the
   --  result is always canonical for the current platform: / for Unix
   --  and Windows, and : for Macintosh.

   procedure Make_Directory (Name : in String);
   procedure Mkdir (Name : in String) renames Make_Directory;
   --  Creates a directory with the specified Name, including all
   --  non-existing parent directories as well.  If an existing
   --  directory is specified, then no action is taken.
   --  Exceptions:
   --    File_Already_Exists is raised when trying to overwrite an
   --       existing file with a directory

   function Get_Modified_Time (Name : in String) return Ada.Calendar.Time;
   --  Gets the time at which the file was last modified.
   --  Exceptions:
   --    No_Such_File is raised if the file does not exist.
   --    Time_Format_Error is raised if the modified time cannot
   --       be converted to Ada calendar time.

   procedure Set_Modified_Time
     (Name : in String;
      Date : in Ada.Calendar.Time);
   --  Sets the time at which the file was last modified.
   --  Exceptions:
   --    No_Such_File is raised if the file does not exist.
   --    Time_Format_Error is raised if the modified time cannot
   --       be converted from Ada calendar time.

   function Native_Name (Name : in String) return String;
   --  Returns the platform-specific name of the file.  This is useful
   --  if the filename is needed to pass to a platform-specific call,
   --  such as exec under Windows or AppleScript on the Macintosh.

   function Owned (Name : in String) return Boolean;
   --  Returns True if file name is owned by the current user,
   --  False otherwise.

   --  File names are grouped into three general types based on the
   --  starting point for the path used to specify the file:
   type Path_Type is (
      Absolute,         --  Absolute names are completely qualified, giving a
                        --  path to the file relative to a particular volume
                        --  and the root directory on that volume.
      Relative,         --  Relative names are unqualified, giving a path to
                        --  the file relative  to the current working
                        --  directory.
      VolumeRelative    --  VolumeRelative names are partially qualified,
                        --  either giving the path relative to the root
                        --  directory on the current volume, or relative to
                        --  the current directory of the specified volume.
     );

   function Get_Path_Type (Name : in String) return Path_Type;
   --  Get Path_Type of specified file Name.

   function Readable (Name : in String) return Boolean;
   --  Returns True if file Name is readable by the current user,
   --  False otherwise.

   function Is_Link (Name : in String) return Boolean;
   --  Returns True if file Name is a symbolic link,
   --  False otherwise.
   --  Portability Notes:
   --    Will always return False on those platforms which
   --       do not support symbolic links.
   --    A Windows shortcut is not a symbolic link.

   function Read_Link (Name : in String) return String;
   --  Returns the value of the symbolic link given by Name
   --  (i.e. the name of the file it points to).  If file
   --  Name is not a symbolic link or this platform does
   --  not support symbolic link, returns Name.
   --  Exceptions:
   --    No_Such_File is raised if the file Name does not exist.
   --  Note for Tcl programmers:
   --    If the file is not a symbolic link or symbolic
   --    links are not supported on this platform, the
   --    Tcl command "file readlink" returns an error.
   --    In order to make cross-platform programming a
   --    little easier, we chose to provide the function
   --    Is_Link (see above) and to return Name rather
   --    than raising an exception in this case.

   procedure Rename
     (Source : in String;
      Target : in String;
      Force  : in Boolean := False);
   --  Renames (moves) Source file or directory to Target file or directory.
   --  If Target is a directory name:
   --    If Source is a file, it is moved into Target directory.
   --    If Source is a directory, its contents are moved recursively
   --       into Target directory.
   --  If Target is a file name or does not exist,
   --    If Source is a file, it is moved to Target.
   --    If source is a directory, an exception is raised
   --  Existing files will be overwritten only if Force = True.
   --  Exceptions:
   --    No_Such_File is raised if Source does not exist.
   --    File_Already_Exists is raised if Target already exists and
   --       Force = False.

   function Root_Name (Name : in String) return String;
   --  Returns all of the characters in Name up to but not including
   --  the last "." character in the last component of Name.  If the
   --  last component of Name doesn't contain a dot, then returns Name.

   function Size (Name : in String) return Natural;
   --  Returns the size of file Name in bytes.
   --  Exceptions:
   --    No_Such_File is raised if the file does not exist.

   function Split (Name : in String) return Tash.Lists.Tash_List;
   --  Returns a list whose elements are the path components in Name.
   --  The first element of the list will have the same path type as Name.
   --  All other elements will be relative.  Path separators will be
   --  discarded unless they are needed ensure that an element is
   --  unambiguously relative. For example, under Unix,
   --  Split ("/foo/~bar/baz") returns a list with the elements
   --  "/", "foo", "./~bar", "baz" to ensure that later commands
   --  that use the third component do not attempt to perform tilde
   --  substitution.

   function Tail (Name : in String) return String;
   --  Returns all of the characters in Name after the last directory
   --  separator.  If Name contains no separators then returns Name.

   type File_Type is (
      File,             --
      Directory,        --
      CharacterSpecial, --
      BlockSpecial,     --
      Fifo,             --  AKA named pipe
      Link,             --
      Socket            --
     );
   type File_Type_List is array (Positive range <>) of File_Type;
   Null_File_Type_List : File_Type_List (1 .. 0);

   function Get_File_Type (Name : in String) return File_Type;
   --  Returns a string giving the type of file Name.

   function Volume return Tash.Lists.Tash_List;
   --  Returns a list of the the absolute paths to the volumes mounted
   --  on the system.
   --  Portability Note:
   --    Macintosh: will be a list of the mounted drives, both
   --       local and network.  N.B. if two drives have the same name,
   --       they will both appear on the volume list, but there is
   --       currently no way, from Tcl, to access any but the first
   --       of these drives.
   --    Unix: will always return "/", since all filesystems are
   --       locally mounted.
   --    Windows: will return a list of the available local drives
   --       (e.g. "a:/", "c:/").

   function Writable (Name : in String) return Boolean;
   --  Returns True if file Name is writable by the current user,
   --  False otherwise.

   procedure Change_Directory (Name : in String := "");
   procedure Cd (Name : in String := "") renames Change_Directory;
   --  Change the current working directory to Name, or to the home
   --  directory (as specified in the HOME environment variable)
   --  if Name is an empty string.

   function Match
     (Pattern     : in String;
      Directory   : in String := "";
      Path_Prefix : in String := "";
      Type_List   : in String := "")
      return        Tash.Lists.Tash_List;
   function Glob
     (Pattern     : in String;
      Directory   : in String := "";
      Path_Prefix : in String := "";
      Type_List   : in String := "")
      return        Tash.Lists.Tash_List renames Match;
   --  Returns a list of the files whose names match Pattern.
   --    ?          Matches any single character.
   --    *          Matches any sequence of zero or more characters.
   --    [chars]    Matches any single character in chars.  If chars
   --                  contains a sequence of the form a-b then any
   --                  character between a and b (inclusive) will match.
   --    \x         Matches the character x.
   --    {a,b,...}  Matches any of the strings a, b, etc.
   --
   --  If Directory is not an empty string, Match searches for files
   --  which match the Pattern starting in the given directory.  This
   --  allows searching of directories whose name contains glob-sensitive
   --  characters without the need to quote such characters explicitly.
   --  This parameter may not be used in conjunction with Path.
   --
   --  If Path_Prefix is not an empty string, Match searches for files
   --  with the given Path_Prefix where the rest of the name matches the
   --  given patterns.  This allows searching for files with names similar
   --  to a given file even when the names contain glob-sensitive characters.
   --  This parameter may not be used in conjunction with Directory.
   --
   --  If Type_List is not an empty string, Match returns a list of files
   --  where all the Type_List strings given must match.  The string
   --  elements of Type_List are "r", "w", or "x" as file permissions,
   --  and "readonly" and "hidden" as special permission cases.  On
   --  MacOS 9, MacOS types and creators are also supported,
   --  where any item which is four characters long is assumed to be a
   --  MacOS type (e.g. TEXT).  Items which are of the form
   --  "{macintosh type XXXX}" or "{macintosh creator XXXX}" will match
   --  types or creators respectively.  Unrecognised types, or specifications
   --  of multiple MacOS types/creators will signal an error.
   --  Type_List will also take a second form of type which is a string
   --  version of the File_Type: "f" for File, "d" for Directory,
   --  "c" for CharacterSpecial, "b" for BlockSpecial, "p" for Fifo,
   --  "l" for Link, "s" for Socket.
   --  The two forms may be mixed, so a Type_List of "d f r w" will
   --  find all regular files OR directories that have both read
   --  AND write permissions.
   --
   --  Portability Notes:
   --    Unlike other Tcl commands that will accept both network and
   --    native style names (see the Tcl filename manual entry for
   --    details on how native and network names are specified),
   --    Match only accepts native names.
   --
   --    Windows:
   --       For Windows UNC names, the servername and sharename
   --       components of the path may not contain ?, *, or []
   --       constructs.  On Windows NT, if pattern is of the form
   --       "~username@domain" it refers to the home directory of
   --       the user whose account information resides on the specified
   --       NT domain server.  Otherwise, user account information is
   --       obtained from the local computer.  On Windows 95 and 98,
   --       Match accepts patterns like ".../" and "..../" for
   --       successively higher up parent directories.
   --
   --   MacOS 9
   --      When using the Directory or Path_Prefix parameters,
   --      Match assumes the directory separator for the entire pattern
   --      is the standard ":".  When not using these parameters
   --      (they are empty strings), Match examines each pattern
   --      argument and uses "/" unless the pattern contains a ":".

   function Current_Working_Directory return String;
   function Pwd return String renames Current_Working_Directory;
   --  Returns the full path name of the current working directory.

end Tash.File;
