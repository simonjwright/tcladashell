-------------------------------------------------------------------
--
--  Unit Name:    Tash.File_IO spec
--
--  File Name:    tash-file_io.ads
--
--  Purpose:      Provides file input/output and file configuration
--               routines.
--
--  Copyright (c) 2000 Terry J. Westley
--
--  Limitations:  Although the underlying Tcl file I/O system supports
--                nonblocking I/O, Tash does not currently support it.
--                The primary technical problems associated with
--                implementing nonblocking read/write is that Tash
--                must start the Tcl event loop and support the Tcl
--                fileevent command.  This has been postponed so we
--                can make this new implementation of a Tash thick
--                binding available sooner.
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
--    Tcl command               Tash Ada subprogram
--    -----------               -------------------
--    open                      Open
--    close                     Close
--    flush                     Flush
--    eof                       End_Of_File, EOF
--    read                      Get
--    gets                      Get_Line
--    puts                      Put, Put_Line
--    fconfigure -blocking      Set_Blocking_Mode
--                              Get_Blocking_Mode
--    fconfigure -buffering     Set_Buffering_Mode
--                              Get_Buffering_Mode
--    fconfigure -buffersize    Set_Buffer_Size
--                              Get_Buffer_Size
--    fconfigure -encoding      Get_Encoding
--    fconfigure -eofchar       Set_EOF_Char
--                              Get_EOF_Char
--                              EOF_Char_Is_Empty_String
--    fconfigure -translation   Set_Translation_Mode
--                              Get_Translation_Mode
--
----------------------------------------------------------------------------

with Tash.Lists;

package Tash.File_IO is

   type Tash_File is new Tash.Tash_Object with null record;

   function To_String (File : in Tash_File) return String;
   --  Returns the string value of the underlying Tcl channel.

   --  These have the standard Posix meanings
   --  for file open access flags.
   -----------------------------------------
   type File_Mode is (
      RDONLY,   --  Open the file for reading only.
      WRONLY,   --  Open the file for writing only.
      RDWR,     --  Open the file for both reading and writing.
      APPEND,     --  Set the file pointer to the end of the file
                  --  prior to each write.
      CREAT,      --  Create the file if it doesn't already exist
                  --  (without this flag it is an error for the file
                  --  not to exist).
      EXCL,       --  If CREAT is also specified, an error is returned
                  --  if the file already exists.
      NOCTTY,     --  If the file is a terminal device, this flag prevents
                  --  the file from becoming the controlling terminal of
                  --  the process.
   --  NONBLOCK, -- Prevents the process from blocking while opening the
   --  file, and possibly in subsequent I/O operations.  The
   --  exact behavior of this flag is system- and device-
   --  dependent;  its use is discouraged (it is better to
   --  use the Set_Blocking_Mode procedure to put a file in
   --  nonblocking mode).  For details refer to your system
   --  documentation on the open system call's O_NONBLOCK flag.
      TRUNC     --  If the file exists it is truncated to zero length.
     );

   type File_Mode_List is array (Positive range <>) of File_Mode;

   Read         : constant File_Mode_List := (1 => RDONLY);
   Write_Trunc  : constant File_Mode_List := (WRONLY, TRUNC, CREAT);
   Write_Append : constant File_Mode_List := (WRONLY, APPEND, CREAT);

   procedure Open
     (File : in out Tash_File;
      Name : in String;
      Mode : in File_Mode_List := Read;
      Perm : in Natural        := 8#0666#);
   --  Opens a file, serial port, or command pipeline.
   --
   --  Mode is a list of any of the File_Modes, one of which must be
   --  either RDONLY, WRONLY or RDWR.
   --
   --  If a new file is created, Perm is used to set the permissions
   --  for the new file in conjunction with the process's file mode
   --  creation mask.  Posix octal file permission numbers are used.
   --
   --  If the first character of file Name is '|' then
   --    The remaining characters of Name are treated as a list of
   --    arguments that describe a command pipeline to invoke, in the
   --    same style as the arguments for Tash.Exec.Exec.  In this case,
   --    the Tash_File identifier returned by Open may be used to write
   --    to the command's input pipe or read from its output pipe,
   --    depending on the value of Mode.  If write-only mode is used
   --    (i.e. Mode is WRONLY), then standard output for the pipeline is
   --    directed to the current standard output unless overridden by
   --    the command.  If read-only mode is used (i.e. Mode is RDONLY),
   --    standard input for the pipeline is taken from the current
   --    standard input unless overridden by the command.
   --  else
   --    The subprogram opens a file:  Name gives the name of the
   --    file to open, and it must conform to the file naming
   --    conventions described in Tash.File.
   --    If Name refers to a serial port, then the specified serial
   --    port is opened and initialized in a platform-dependent manner.
   --    Acceptable values for the file Name to use to open a serial
   --    port are described in the Portability Notes section below.
   --
   --  Portability Notes
   --------------------
   --
   --  Windows (all versions)
   --    Valid values for Name to open a serial port are of the form
   --    comX:, where X is a number, generally from 1 to 4. This notation
   --    only works for serial ports from 1 to 9, if the system happens
   --    to have more than four.  An attempt to open a serial port that
   --    does not exist or has a number greater than 9 will fail.  An
   --    alternate form of opening serial ports is to use the filename
   --    \\.\comX, where X is any number that corresponds to a serial port;
   --    please note that this method is considerably slower on Windows
   --    95 and Windows 98.
   --
   --  Windows NT
   --    When running Tash interactively, there may be some strange
   --    interactions between the real console, if one is present, and
   --    a command pipeline that uses standard input or output.  If a
   --    command pipeline is opened for reading, some of the lines
   --    entered at the console will be sent to the command pipeline
   --    and some will be sent to the Tcl evaluator.  If a command
   --    pipeline is opened for writing, keystrokes entered into the
   --    console are not visible until the the pipe is closed.  This
   --    behavior occurs whether the command pipeline is executing 16-bit
   --    or 32-bit applications.  These problems only occur because both
   --    Tcl and the child application are competing for the console at
   --    the same time.  If the command pipeline is started from a script,
   --    so that Tcl is not accessing the console, or if the command
   --    pipeline does not use standard input or output, but is redirected
   --    from or to a file, then the above problems do not occur.
   --
   --  Windows 95
   --    A command pipeline that executes a 16-bit DOS application
   --    cannot be opened for both reading and writing, since 16-bit
   --    DOS applications that receive standard input from a pipe and
   --    send standard output to a pipe run synchronously.  Command
   --    pipelines that do not execute 16-bit DOS applications run
   --    asynchronously and can be opened for both reading and writing.
   --    When running Tash interactively, there may be some strange
   --    interactions between the real console, if one is present, and
   --    a command pipeline that uses standard input or output.  If a
   --    command pipeline is opened for reading from a 32-bit application,
   --    some of the keystrokes entered at the console will be sent to
   --    the command pipeline and some will be sent to the Tcl evaluator.
   --    If a command pipeline is opened for writing to a 32-bit application,
   --    no output is visible on the console until the the pipe is closed.
   --    These problems only occur because both Tcl and the child
   --    application are competing for the console at the same time.
   --    If the command pipeline is started from a program, so that Tash
   --    is not accessing the console, or if the command pipeline does
   --    not use standard input or output, but is redirected from or
   --    to a file, then the above problems do not occur.
   --    Whether or not Tash is running interactively, if a command
   --    pipeline is opened for reading from a 16-bit DOS application,
   --    the call to open will not return until end-of-file has been
   --    received from the command pipeline's standard output.  If a
   --    command pipeline is opened for writing to a 16-bit DOS
   --    application, no data will be sent to the command pipeline's
   --    standard output until the pipe is actually closed.  This
   --    problem occurs because 16-bit DOS applications are run
   --    synchronously, as described above.
   --
   --  Macintosh
   --    Opening a serial port is not currently implemented under Macintosh.
   --    Opening a command pipeline is not supported under Macintosh, since
   --    applications do not support the concept of standard input or output.
   --
   --  Unix
   --    Valid values for file Name to open a serial port are generally
   --    of the form /dev/ttyX, where X is a or b, but the name of any
   --    pseudo-file that maps to a serial port may be used.
   --    When running Tash interactively, there may be some strange
   --    interactions between the console, if one is present, and a
   --    command pipeline that uses standard input.  If a command
   --    pipeline is opened for reading, some of the lines entered
   --    at the console will be sent to the command pipeline and some
   --    will be sent to the Tcl evaluator.  This problem only occurs
   --    because both Tcl and the child application are competing for
   --    the console at the same time.  If the command pipeline is
   --    started from a script, so that Tcl is not accessing the console,
   --    or if the command pipeline does not use standard input, but is
   --    redirected from a file, then the above problem does not occur.
   --
   --  See the Portability Notes section of Tash.Exec.Exec for
   --  additional information not specific to command pipelines
   --  about executing applications on the various platforms.

   procedure Close (File : in out Tash_File);
   --  Closes the channel specified by File.  All buffered output is
   --  flushed to the output device, any buffered input is discarded,
   --  the underlying file or device is closed, and Tash_File becomes
   --  unavailable for use.
   --
   --  If the Tash_File is blocking, the procedure does not return
   --  until all output is flushed.  If the Tash_File is nonblocking
   --  and there is unflushed output, the channel remains open and the
   --  procedure returns immediately; output will be flushed in the
   --  background and the Tash_File will be closed when all the flushing
   --  is complete.
   --
   --  If File is a blocking Tash_File for a command pipeline then close
   --  waits for the child processes to complete.
   --
   --  Tash_Files are automatically closed when the process exits.
   --  Tash_Files are switched to blocking mode, to ensure that all
   --  output is correctly flushed before the process exits.

   procedure Flush (File : in out Tash_File);
   --  Flushes any output that has been buffered for File.  File must be
   --  open for writing.  If the Tash_File is in blocking mode the procedure
   --  does not return until all the buffered output has been flushed to the
   --  Tash_File.  If the Tash_File is in nonblocking mode, the procedure
   --  may return before all buffered output has been flushed; the remainder
   --  will be flushed in the background as fast as the underlying file or
   --  device is able to absorb it.

   function End_Of_File (File : in Tash_File) return Boolean;
   function EOF (File : in Tash_File) return Boolean renames End_Of_File;
   --  Check for end of file on the specified Tash_File.

   function Pid (File : in Tash_File) return Tash.Lists.Tash_List;
   --  File refers to a command pipeline created with the Open procedure.
   --  Pid returns a list whose elements are the process identifiers of all
   --  the processes in the pipeline, in order.  The list will be empty if
   --  File refers to an open file that isn't a process pipeline.

   procedure New_Line (File : in Tash_File; Count : in Positive := 1);
   --  In Tash File I/O (really, in the underlying Tcl I/O system), the end
   --  of a line is always represented using a single line-feed character
   --  (ASCII.LF).  So, New_Line merely writes the specified Count number of
   --  line-feed characters to the specified File.  However, in actual files
   --  and devices, the end of a line may be represented differently on
   --  different platforms, or even for different devices on the same
   --  platform.  For example, under UNIX line-feeds are used in files,
   --  whereas carriage-return-linefeed sequences are normally used in
   --  network connections.  On input (i.e., with Get and Get_Line) the Tcl
   --  I/O system automatically translates the external end-of-line
   --  representation into line-feed characters.  Upon output (i.e., with
   --  Put and Put_Line), the I/O system translates line-feeds to the
   --  external end-of-line representation.  The default translation mode,
   --  auto, handles all the common cases automatically, but the
   --  Set_Translation_Mode procedure provides explicit control over the end
   --  of line translations.

   procedure Get (File : in Tash_File; Item : out String);
   --  Reads Item'length characters from File or to the end of the file,
   --  whichever is fewer.  If File is in nonblocking mode, the procedure
   --  may not read as many characters as requested: once all available
   --  input has been read, the procedure will return the data that is
   --  available rather than blocking for more input.

   procedure Get_Line
     (File : in Tash_File;
      Item : out String;
      Last : out Natural);
   --  Reads the next line from File, returns everything in the line up to
   --  (but not including) the end-of-line character(s), and discards the
   --  end-of-line character(s).  If end of file occurs while scanning for
   --  an end of line, the procedure returns whatever input is available up
   --  to the end of file.  If file is in nonblocking mode and there is not
   --  a full line of input available, the procedure returns an empty string
   --  and does not consume any input.

   procedure Put (File : in Tash_File; Item : in String);
   --  Writes Item to File.

   procedure Put_Line (File : in Tash_File; Item : in String);
   --  Writes Item to File, followd by the appropriate end-of-line
   --  character(s) for this platform based on the current Translation Mode.
   --
   --  Tcl buffers output internally, so characters written with Put_Line
   --  (and Put above) may not appear immediately on the output file or
   --  device; Tcl will normally delay output until the buffer is full or
   --  the Tash_File is closed.  You can force output to appear immediately
   --  with the Flush procedure.  When the output buffer fills up, the
   --  Put_Line procedure will normally block until all the buffered data
   --  has been accepted for output by the operating system.  If File is in
   --  nonblocking mode then the Put_Line procedure will not block even if
   --  the operating system cannot accept the data.  Instead, Tcl continues
   --  to buffer the data and writes it in the background as fast as the
   --  underlying file or device can accept it.  It is possible for an
   --  arbitrarily large amount of data to be buffered for a Tash_File in
   --  nonblocking mode, which could consume a large amount of memory. To
   --  avoid wasting memory, nonblocking I/O should normally be used in an
   --  event-driven fashion with the fileevent procedure (don't invoke
   --  Put_Line unless you have recently been notified via a file event that
   --  the Tash_File is ready for more output data).

   function Get_Blocking_Mode (File : in Tash_File) return Boolean;
   --  procedure Set_Blocking_Mode (
   --   File : in Tash_File;
   --   Mode : in Boolean);
   --  Get and set the blocking mode of the specified File.
   --  By default, the blocking mode of all Tash_Files is True.
   --
   --  Implementation Note:
   --    Non-blocking mode is not currently supported by Tash.
   --    In order to support non-blocking mode, Tash must start
   --    the Tcl event loop and provide a capability equivalent
   --    to the Tcl fileevent command.

   type Buffering_Mode is (
      Full, --  the I/O system will buffer output until its internal
            --  buffer is full or until the flush command is invoked.
      Line, --  the I/O system will automatically flush output whenever
            --  a line-feed character (ASCII.LF) is output.
      None  --  the I/O system will flush automatically after every
            --  output operation.
     );

   function Get_Buffering_Mode (File : in Tash_File) return Buffering_Mode;
   procedure Set_Buffering_Mode
     (File : in Tash_File;
      Mode : in Buffering_Mode);
   --  Get and set the buffering mode of the specified File.
   --  The default for a newly opened file is Full except for
   --  Tash_Files that connect to terminal-like devices; for these
   --  Tash_Files the initial setting is line.

   subtype Buffer_Size is Natural range 10 .. 10_000_000;

   function Get_Buffer_Size (File : in Tash_File) return Buffer_Size;
   procedure Set_Buffer_Size (File : in Tash_File; Size : in Buffer_Size);
   --  Get and set the size of the Tcl internal buffer.

   type Encoding_Type is (
      Normal,   --  Read/write ASCII file
      ShiftJIS, --  Read/write Japanese ShiftJIS file
      Binary    --  Read/write pure binary data (e.g, a JPEG)
     );

   --  Set_ and Get_Encoding is not currently implemented.
   --  We need to investigate what "Normal" encoding type
   --  means so we can switch to/from Normal.  Normal is
   --  not explicitly called out in Tcl and is platform-
   --  dependent.
   ------------------------------------------------------
   --  function Get_Encoding (
   --    File     : in Tash_File) return Encoding_Type;
   --  procedure Set_Encoding (
   --    File     : in Tash_File;
   --    Encoding : in Encoding_Type);
   --  Specify the encoding of the Tash_File, so that the data can be
   --  converted to and from Unicode for use in Tcl.  For instance, in order
   --  for Tcl to read characters from a Japanese file in ShiftJIS and
   --  properly process and display the contents, the encoding would be set
   --  to ShiftJIS.  Thereafter, when reading from the Tash_File, the bytes
   --  in the Japanese file would be converted to Unicode as they are
   --  read. Writing is also supported - as Tcl strings are written to the
   --  Tash_File they will automatically be converted to the specified
   --  encoding on output.
   --
   --  If a file contains pure binary data (for instance, a JPEG image), the
   --  encoding for the Tash_File should be configured to be binary.  Tcl
   --  will then assign no interpretation to the data in the file and simply
   --  read or write raw bytes.  The Tcl binary command can be used to
   --  manipulate this byte-oriented data.
   --
   --  The default encoding for newly opened Tash_Files is the same
   --  platform- and locale-dependent system encoding used for interfacing
   --  with the operating system.

   function Get_EOF_Char (File : in Tash_File) return Character;
   function Get_EOF_Char (File : in Tash_File) return String;
   function EOF_Char_Is_Empty_String (File : in Tash_File) return Boolean;

   procedure Set_EOF_Char (File : in Tash_File; Char : in Character);
   procedure Set_EOF_Char_To_Empty_String (File : in Tash_File);
   --  This option supports DOS file systems that use Control-z (16#1A#) as
   --  an end of file marker.
   --
   --  Set_EOF_Char sets Char to signal end-of-file when it is encountered
   --  during input.  For output, Char is output when the channel is closed.
   --
   --  Set_EOF_Char_to_Empty_String signifies that there is no special end
   --  of file character marker.
   --
   --  The default value for is the empty string in all cases except for
   --  files under Windows.  In that case, the default is Control-z (16#1A#)
   --  for reading and the empty string for writing.

   --  function Get_EOF_In_Char (
   --    File     : in Tash_File) return String;
   --  function Get_EOF_Out_Char (
   --    File     : in Tash_File) return String;
   --  procedure Set_EOF_Char (
   --    File     : in Tash_File;
   --    In_Char  : in String;
   --   Out_Char : in String);
   --  This option supports DOS file systems that use Control-z (16#1A#) as
   --  an end of file marker.
   --
   --  If In_Char is not an empty string, then
   --    the character In_Char(In_Char'first) signals end-of-file when
   --    it is encountered during input.
   --  If Out_Char is not an empty string, then
   --    the Out_Char(Out_Char'first) is output when the channel is closed.
   --  If In_Char is the empty string, then
   --    there is no special end of file character marker for input.
   --  If Out_Char is the empty string, then
   --    there is no special end of file character marker for output.
   --
   --  The default value for is the empty string in all cases except for
   --  files under Windows.  In that case, the default is Control-z (16#1A#)
   --  for reading and the empty string for writing.
   --
   --  For read-write channels, the end of file marker for input and output
   --  are In_Char and Out_Char, respectively.  As a convenience, when
   --  setting the end-of-file character for a read-write channel you can
   --  specify a single value that will apply to both reading and writing.

   type Translation_Mode is (
      Auto,    --  Input:  Treats any of line-feed (lf), carriage return (cr),
               --         or carriage return followed by a line-feed (crlf)
               --         as the end of line representation.  The end of line
               --         representation can even change from line-to-line,
               --         and all cases are translated to a line-feed.
               --  Output: Chooses a platform specific representation;
               --         for sockets on all platforms Tcl chooses crlf,
               --         for all Unix flavors, it chooses lf, for the
               --         Macintosh platform it chooses cr and for the
               --         various flavors of Windows it chooses crlf.
               --         The default setting for -translation is auto
               --         for both input and output.

      Binary,  --  No end-of-line translations are performed.  This is
               --  nearly identical to LF mode, except that in addition
               --  Binary mode also sets the end-of-file character to
               --  the empty string (which disables it) and sets the
               --  encoding to binary (which disables encoding filtering).
               --  See the description of Set_EOF_Char and Set_Encoding_Mode
               --  for more information.

      CR,      --  The end of a line in the underlying file or device is
               --  represented by a single carriage return character.
               --  This mode is typically used on Macintosh platforms.
               --  Input:  Converts carriage returns to line-feed characters.
               --  Output: Translates line-feed characters to carriage returns

      CRLF,    --  The end of a line in the underlying file or device is
               --  represented by a carriage return character followed by
               --  a linefeed character.
               --  This mode is typically used on Windows platforms and
               --  for network connections.
               --  Input:  Converts carriage-return-linefeed sequences to
               --         line-feed characters.
               --  Output: Translates line-feed characters to
               --         carriage-return-linefeed sequences.

      LF       --  The end of a line in the underlying file or device is
               --  represented by a single line-feed (linefeed) character.
               --  In this mode no translations occur during either input
               --  or output.  This mode is typically used on UNIX platforms.
     );

   function Get_Translation_Mode
     (File : in Tash_File)
      return Translation_Mode;
   procedure Set_Translation_Mode
     (File : in Tash_File;
      Mode : in Translation_Mode);
   --  procedure Get_Translation_Mode (
   --    File     : in  Tash_File;
   --    In_Mode  : out Translation_Mode;
   --    Out_Mode : out Translation_Mode);
   --  procedure Set_Translation_Mode (
   --    File     : in Tash_File;
   --    In_Mode  : in Translation_Mode;
   --    Out_Mode : in Translation_Mode);
   --  In Tash File I/O (really, in the underlying Tcl I/O system), the end
   --  of a line is always represented using a single line-feed character
   --  (ASCII.LF).  However, in actual files and devices, the end of a line
   --  may be represented differently on different platforms, or even for
   --  different devices on the same platform.  For example, under UNIX
   --  line-feeds are used in files, whereas carriage-return-linefeed
   --  sequences are normally used in network connections.  On input (i.e.,
   --  with Get and Get_Line) the Tcl I/O system automatically translates
   --  the external end-of-line representation into line-feed characters.
   --  Upon output (i.e., with Put and Put_Line), the I/O system translates
   --  line-feeds to the external end-of-line representation.  The default
   --  translation mode, auto, handles all the common cases automatically,
   --  but the Set_Translation_Mode procedure provides explicit control over
   --  the end of line translations.

end Tash.File_IO;
