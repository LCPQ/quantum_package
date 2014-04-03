======================
Programming guidelines
======================

Each module (directory) contains the following:

* A :file:`README.rst` file to document the current module.
* An :file:`ASSUMPTIONS.rst` file. This file should document all the implicit
  assumptions used in the module. For example, if the atomic orbitals are
  assumed to be normalized, this should be mentioned in the
  :file:`AOs/ASSUMPTIONS.rst` file.
* A set of :file:`.irp.f` files containing provider, subroutines and functions
* A :file:`tests` directory that should contain the test programs of the module
  (see testing section)
* A :file:`Makefile` that should compile
* Optionally some :file:`*.ezfio_config` configuration files for the EZFIO
  library

A new module may be created by invoking the :file:`create_module.sh` script.

Every subroutine, function or provider should be documented using the
BEGIN_DOC ... END_DOC block. The documentation should be written in
ReStructured Text format to enable the automatic generation of the Sphinx
documentation.


Testing
=======

Each module contains a :file:`tests` directory which contains the test script.
Each module should be tested by running::

  make test

Before merging into the master branch, the module should pass **all** the tests.
This enables the possibility tu use :command:`git bisect` to quickly find bugs.


Input data
==========

Every program is supposed to use an EZFIO database containing all the
persistent data. This database can be modified in using the generated Fortran
functions or the generated Python module.

The definition of the data needed by the module should be put in the
:file:`*.ezfio_config` file.

Input data can also be read from the standard input to enable the use of
a program with a pipe, but the read statements should be present **only** in
the main program.


Output data
===========

 Print to stdout statements should only appear in programs, not in providers,
subroutines or functions. This enables the possibility easily use the programs
with pipes.

To print, you should write in an output file provided by the :file:`Output`
module.



Modifying MOs
=============

When modifying MOs, the label of the current MOs should change accordingly by changing the mo_label. For example:

.. code-block::  fortran

  mo_coef = new_mo_coef
  mo_label = "MyNewLabel"
  TOUCH mo_coef mo_label


