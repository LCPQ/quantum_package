# Compile

We need to create the file which contains all the tree dependencies for the
binaries.  It's not a Makefile, but a Ninja file (so don't type `make` is
hopeless, type `ninja` instead).

The script to create the dependency file (aka `build.ninja`) is
`qp_create_ninja.py`.

## What utilization of the code will you do?

* If you only want the binaries (for production workflow) use the flag
  `--production` in when calling this script. It's quicker 
* Else if you are a developer and you want to be able to compile specific
  modules use: `--development`. It will create for you the `build.ninja` in each
  module

## Compilation Flags

You need to specify all the flags useful for the compilation:  like the
optimization flags, the Lapack libary, etc.  ``$QP_ROOT/config`` contains
``ifort.cfg`` and ``gfortran.cfg`` containing the compiler flags that will be
used.  You can edit these files to modify the compiling options. 

## Example to create the Ninja file

`qp_create_ninja.py create --production $QP_ROOT/config/ifort.cfg`

## Compiling

Just type `ninja` if you are in `$QP_ROOT` (or `ninja -f $QP_ROOT/build.ninja`
elsewhere). The compilation will take approximately 3 min.

If you have set the `--developement` flag in a specific module you can go in
the corresponding module directory and run `ninja` to build only this module.
You can type `ninja all` in a module for compiling all the submodule

Finally, go in `$QP_ROOT/ocaml` and type `make`
