``$QPACKAGE_ROOT/config`` contains ``ifort.cfg`` and ``gfortran.cfg`` files which
have the compiler flags that will be used to compile the code. You can edit these
files to modify the compiling options (This configuration file is parsed by
``read_compilation_cfg.py``).

To create the Ninja file ::

  create_ninja_build.py --production $QPACKAGE_ROOT/config/ifort.cfg


To compile, run ::

  $QPACKAGE_ROOT/ninja/ninja
