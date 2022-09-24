# Tcl Ada SHell #

Tcl Ada SHell (Tash) is an Ada binding to Tcl/Tk.

Its purpose is to

* allow a Tcl program to use Ada in place of C to implement Tcl commands where additional execution speed, more complex data structures, or better name space management is needed, and

* support the rapid development of Platform-Independent Graphical User Interfaces via Tk.

## Installation and usage ##

This branch is designed to be used with [Alire](https://alire.ada.dev/docs/#introduction).
```
alr init --bin my_project
cd my_project
alr with tash
```

## Licensing ##

The software is released under the GPL Version 2, with the following additional permission:

>As a special exception, if other files instantiate generics from this unit, or you link this unit with other files to produce an executable, this unit does not by itself cause the resulting executable to be covered by the GNU General Public License. This exception does not however invalidate any other reasons why the executable file might be covered by the GNU Public License.

## Skinny Tcl Binding ##

TASH includes both skinny and medium bindings to Tcl. The skinny binding is a direct translation of the public Tcl interface, `tcl.h`. It is implemented in the Ada package `Tcl`. It includes all the definitions in `tcl.h`, both functions and data types. All data types are implemented with Ada equivalents from `Interfaces.C`. All functions take C data types and use return codes for status information.

## Medium Tcl Binding ##

In addition to the skinny binding, TASH provides a medium binding to Tcl. This binding replaces C data types with Ada types (e.g. `String` is used in place of `Interfaces.C.Strings.chars_ptr`), uses exceptions in place of return codes and uses generic packages to implement Tcl `clientdata` types.

## The TASHELL Interpreter ##

The Tcl distribution includes a Tcl shell interpreter, named `tclsh`. The TASH binding derives its name from the Ada version of the Tcl shell interpreter: Tcl Ada SHell. Just like `tclsh`, `tashell` reads and interprets a Tcl script. It is also a good starting point for building a custom Tcl interpreter in which new Tcl commands are implemented in Ada rather than C.

An early paper describing the rationale of the design of TASH is available in several different formats in the docs directory.

## Skinny Tk Binding ##

TASH includes both skinny and medium bindings to Tk. The skinny binding is a direct translation of the public Tk interface, `tk.h`, and is implemented in the Ada package, `Tcl.Tk`.

## Medium Tk Binding ##

An early, experimental medium binding to Tk is provided in the Ada package, `Tcl.Tk.Ada`. This binding does not yet support all Tk widgets.

## The TWASHELL Interpreter ##

`twashell` is the Tcl Windowing Ada SHell. It is the Ada version of the Tcl/Tk `wish` program. Just like `wish`, it reads and interprets a Tcl/Tk script. It is also a starting point for building a custom Tcl/Tk interpreter in which new Tcl/Tk commands are implemented in Ada rather than C.

An early paper describing how `twashell` is a Platform-Independent toolkit for development of Graphical User Interfaces is available in several different formats in the docs directory.

