======================
Programming guidelines
======================

Each module (directory) contains the following:

* A ``README.rst`` file to document the current module.
* An ``ASSUMPTIONS.rst`` file. This file should document all the implicit
  assumptions used in the module. For example, if the atomic orbitals are
  assumed to be normalized, this should be mentioned in the
  ``AOs/ASSUMPTIONS.rst`` file.
* A ``NEEDED_MODULES`` file which contains the list of modules on which the
  current module depends
* A set of ``.irp.f`` files containing provider, subroutines and functions
* A ``Makefile`` that should compile
* Optionally some ``*.ezfio_config`` configuration files for the EZFIO
  library

A new module may be created by invoking the ``create_module.sh`` script.

Every subroutine, function or provider should be documented using the
BEGIN_DOC ... END_DOC block. The documentation should be written in
ReStructured Text format to enable the automatic generation of the Sphinx
documentation.

When the current module depends on other modules, the list of modules should
be added in the ``NEEDED_MODULES`` file.


Creating a new module
=====================

Every new module should be created using the interactive ``create_module.sh``
script located in the ``${QP_ROOT}/scripts`` directory. This will create
all the required files with correct templates.


Makefiles
=========

Use the structure of Makefiles provided by the ``create_module.sh`` script. If
you need to re-create the Makefile, you can use the ``create_Makefile.sh``
script in the current module directory.

If you need to add some Fortran or C files that should not be tracked by IRPF90,
you have to add them manually to the Makefile in the ``SRC`` variable.
You also need to add the corresponding ``*.o`` file prefixed by ``IRPF90_temp/``.
For example

.. code-block:: Makefile

  SRC=map_module.f90
  OBJ=IRPF90_temp/map_module.o



Input data
==========

Every program is supposed to use an EZFIO database containing all the
persistent data. This database can be modified in using the generated Fortran
functions or the generated Python module.

The definition of the data needed by the module should be put in the
``*.ezfio_config`` file.

Input data can also be read from the standard input to enable the use of
a program with a pipe, but the read statements should be present **only** in
the main program.


Output data
===========

Print to stdout statements should only appear in programs, not in providers,
subroutines or functions. This enables the possibility easily use the programs
with pipes.

To print, you should write in an output file provided by the ``Output``
module. Every module has its own output file. Before printing something,
a timestamp should be put in the output with the ``write_time`` function.
This allows an external script to read all the pieces of the output files
and put them in a sequential order. The format of the output should be
in ReStructured Text for easy transformation of the output data to pdf,
HTML, man, etc.


Creating a shell script
=======================

Shell scripts should be located in the ``${QP_ROOT}/scripts`` directory.
Relative paths should be avoided as much as possible, and the result of commands
should be always checked. For example, when creating a directory the existence
of the directory has to be checked.


Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.



Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

