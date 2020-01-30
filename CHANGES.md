# Tcl Ada SHell #

## Changes in 20200230 ##

Corrected setup failures in macOS Mojave, Debian stretch, and Windows 10.

## Changes in 20191120 ##

Reorganized build and distribution.

## Changes in 20140210 ##

`Tcl` and `Tcl.Tk` no longer provide `Null_*` constants or `Is_Null (Ptr : in *) return Boolean;` functions (the types concerned are visibly access types, so the standard `null` is available).

`Tcl`, `Tcl.Ada` and `Tcl.Tk` use `not null` in subprogram parameters where applicable. Note that, although this is an Ada 2005 construct, your GNAT project can still specify Ada 95 if required, because of the use of the GNAT-specific `pragma Ada_2005`.

Fixed some confusion between `Tcl_UniChar` and strings of same.

## Changes in 20140118 ##

A new package `Tcl.Async` supports writing Tcl variables from Ada. This is especially important if the Ada code isn't running in the same thread as the Tcl interpreter.

You can use the 'trace' facility in Tcl to detect when such a write has taken place.

See `watching.adb`, `watching.tcl` in the `demos/` directory for an example.

Supports XQuartz in Mac OS X.

## Changes in 20110925 ##

The `ClientData` generics in `Tcl.Ada` had comments stating that the size of the `ClientData` formal type must be equal to the size of a C pointer. These have been replaced by assertions that the size of `ClientData` must not be greater than that of `System.Address`.

The top-level makefile now supports an `install` target which on GNAT-based systems other than Debian installs TASH alongside your compiler (so you don't need to set `ADA_PROJECT_PATH`).

The `setup.tcl` script recognises `gnatgcc`, if present, as the compiler to use for the C compilations required to build the library.

The setup.tcl script supports the flag `--nogui`, meaning "perform the setup immediately".

The GPR files have been improved; the result is that the Tcl and Tk libraries will be linked automatically.

## Changes in 20090207 ##

Updated the Tcl Ada API to native Tcl 8.[4-6]

Removed function `Tcl.Tcl_GetObjId` as it is not part of native Tcl and it gave trouble on 64-bit systems.

Many of the record types in package `Tcl` were declared private although their native Tcl counterpart is public. Certain functions could not even be used without public access to the record type's contents. These types are now public.

Removed type `Tcl.Tcl_Obj_Ptr` as it is not part of native Tcl, and its use was prone to introduce memory leaks.
