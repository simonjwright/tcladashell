# Tcl Ada SHell #

Tcl Ada SHell (Tash) is an Ada binding to Tcl/Tk.

Its purpose is to

* allow a Tcl program to use Ada in place of C to implement Tcl commands where additional execution speed, more complex data structures, or better name space management is needed, and

* support the rapid development of Platform-Independent Graphical User Interfaces via Tk.

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

## Unsupported features ##

The original TASH included a 'new thick binding'. Unfortunately, this is not supportable in Tcl/Tk 8.5 and later, because it relies on what is now the private implementation of commands such as `file`, `list`.

The thick binding will be built if TASH is installed over Tcl/Tk 8.4 or earlier. However, it will be as well not to rely on its continued availability (for example, Mac OS X Leopard had Tcl/Tk 8.4, but Snow Leopard has 8.5).

The GUI developer RAPID does not depend on these unsupported features.

## Installation ##

### Prerequisites ###

You need an Ada compiler. The instructions here are based on GNAT, though other compilers have been used and there is nothing GNAT-specific about TASH (there are GNAT-isms in the build support files).

You also need Tcl/Tk with working `tclsh` and `wish`.

On Unix systems, this should be all, aside from the usual development environment (`bash`/`sh`, `make` etc).

On Windows systems, you can get the development environment using Cygwin. Cygwin includes a Tcl/Tk implementation, but unfortunately it may not be compatible with GNAT (in the early 2000's, there was a `.dll` clash). We recommend that you use the excellent Community edition from ActiveState, and these instructions are written assuming that. We also recommend that you install at `C:\Tcl` or `C:\ActiveTcl` rather than at `C:\Program Files\Tcl`, to avoid problems with path names with spaces later on.

### Extract TASH ###

Choose a place to build TASH. If you're not using GNAT GPL or FSF GCC, this should be the place you intend to use it from, perhaps your HOME directory.
Assuming this is your home directory, go there and extract TASH by
```
$ unzip tash-8.6-3-20170217.zip
```
or
```
$ tar zxvf tash-8.6-3-20170217.tgz
```
and then enter the source directory:
```
$ cd tash-8.6-3-20170217
```

### Configure ###

#### On Unix ####

If you're running a Unix system, make sure you have the version of GNAT you wish to use at the beginning of your PATH (this is in case you have a clever script like `gnatfe` in `/usr/local/bin`, which will confuse `make install`).

#### On Windows ####

If you're running on Windows, you'll need to set up Cygwin so that GNAT and ActiveState Tcl/Tk are first on your path in Cygwin (which puts `/usr/local/bin` and `/usr/bin` before your Windows path). One way of doing this is to add the following line at the end of `~/.bash_profile`:
```
PATH=/cygdrive/c/Tcl/bin:/cygdrive/c/GNAT/2019/bin:$PATH
```
(assuming that you've got ActiveState Tcl/Tk installed at C:\Tcl and GNAT at C:\GNAT\2019).

#### Common ####

It's important that the C compiler used to compile glue files is compatible with your Ada compiler; one way of achieving this (if you're using GNAT; note, these instructions assume that's the case) is to make sure that GNAT's gcc is first on your path.

If you're happy to accept default settings, there's nothing more; make creates default `makeconf` and `tash_options.gpr` using `setup.tcl --nogui`. If not, open up a shell window at the extracted TASH and say
```
$ ./setup.tcl
```
In either case, the script sets values for several makefile macros used to customize your TASH build.

In most cases, the default values will be OK. If not, either edit the generated files or run `setup.tcl` interactively and type in the correct values; press the Save button to create the `tash_options.gpr` and `makeconf` files.

### Build ###

That done, run
```
$ make all
```
This will compile the necessary C glue files and build the TASH library statically (`libtash.a`) or relocatably (`libtash.so`, `libtash.dll` or `libtash.dylib`) with `.ali` files in `lib-static/` or `lib-relocatable/` respectively, and the corresponding Ada source files in `src/` (still) and `include-relocatable/`.

### Run tests ###

Run
```
$ make test
```
This runs several test programs in the `test/`, `demos/` and, with Tcl/Tk 8.4, `apps/` directories.

## Using TASH ##

If you're using a GNAT-based compiler, it's strongly recommended that you use the GNAT Project feature.

### Using in-place ###

After the make all stage, you can use TASH where it was built with any GNAT-based compiler.

Make sure that the TASH distribution directory is on your `ADA_PROJECT_PATH`, by e.g.
```
$ export ADA_PROJECT_PATH=~/tash-8.6-3-20170217:$ADA_PROJECT_PATH
```
Your GPR should include TASH:
```
with "tash";
```

### Installing with the compiler ###

If you're using FSF GCC or GNAT GPL, the best way is to install TASH alongside your compiler.

The remaining instructions assume use on a Unix machine, or Cygwin if on Windows.

Having run `make all` as above,
```
$ make install
```
(you may need to do this as root on Unix systems).
