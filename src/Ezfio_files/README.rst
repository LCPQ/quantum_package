==================
Ezfio_files Module
==================

This modules essentially contains the name of the EZFIO directory in the
``ezfio_filename`` variable. This is read as the first argument of the
command-line, or as the ``QPACKAGE_INPUT`` environment variable.
Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`ezfio_filename <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/ezfio.irp.f#L1>`_
  Name of EZFIO file. It is obtained from the QPACKAGE_INPUT environment
  variable if it is set, or as the 1st argument of the command line.

`getunitandopen <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/get_unit_and_open.irp.f#L1>`_
  :f:
  file name
  .br
  :mode:
  'R' : READ, UNFORMATTED
  'W' : WRITE, UNFORMATTED
  'r' : READ, FORMATTED
  'w' : WRITE, FORMATTED
  'a' : APPEND, FORMATTED
  'x' : READ/WRITE, FORMATTED
  .br



