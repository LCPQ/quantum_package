============
Installation
============

Requirements
------------

* `GNU make <http://www.gnu.org/software/make>`_ 
* `The Intel Fortran Compiler with MKL <http://software.intel.com/en-us/fortran-compilers>`_
* `IRPF90 <http://irpf90.ups-tlse.fr>`_
* `EZFIO <http://ezfio.sourceforge.net>`_

Optional Requirements
---------------------

* `Sphinx <http://sphinx-doc.org/>`_ is used to build the documentation

Setup
-----

#) Run the :file:`setup_environment.sh` script. This will create the :file:`sci.rc` file 
   that contains all the environment variables ::

   $ ./setup_environment.sh

#) Source this file into your shell ::

   $ source sci.rc

#) Go into the :file:`src` directory and create the :file:`Makefile.config` file using the
   :file:`Makefile.config.example` file as a template

#) Build the program ::

   $ make

#) Build the documentation ::

   $ make doc

