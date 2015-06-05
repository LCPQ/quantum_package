# Compile

We need to create the file who contain all the tree dependencies for the binaries. It's not a MakeFile, but a Ninja file. (So don't type `make` is hopeless, type `ninja` instead)

The script to create the dependencies file (aka `build.ninja`) is `create_ninja_build.py`.

## What utilization of the code will you do?

   * If you only want the binaries (for production workflow) use the flag `--production` in when calling this script. It's quicker 

   * Else if you are a developer and you want to be able to only compile one specific module use: `--development`

## Compilation Flag

You need to specify all the flag useful for the compilation:   like the optimization one, the mkl one .``$QPACKAGE_ROOT/config`` contains ``ifort.cfg`` and ``gfortran.cfg`` files which have the compiler flags that will be used to compile the code. You can edit these files to modify the compiling option. Put the file path when calling `create_ninja_build.py`

## Example to create the Ninja file

`create_ninja_build.py --production $QPACKAGE_ROOT/config/ifort.cfg`

# WARNING

For now reload this command if you add a `IRP.f90` or `EZFIO.cfg` file or modify the `NEED_CHILDREN_MODULE`!

## Compile

Just type `ninja` if you are in `$QPACKAGE_ROOT` (or `ninja -f $QPACKAGE_ROOT/build.ninja` elsewhere). The compilation will take approximately 3 min.

If you have set the `--developement` flag in a specific module you can go in the corresponding IRPF90_temp and run `ninja` to only make the module and submodule binaries. (You can use the `-f` option too)

Now go in `$QPACKAGE_ROOT/ocaml` and type `make`