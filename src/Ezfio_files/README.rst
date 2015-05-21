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

`output_cpu_time_0 <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/output.irp.f#L2>`_
  Initial CPU and wall times when printing in the output files

`output_wall_time_0 <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/output.irp.f#L1>`_
  Initial CPU and wall times when printing in the output files

`write_bool <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/output.irp.f#L88>`_
  Write an logical value in output

`write_double <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/output.irp.f#L58>`_
  Write a double precision value in output

`write_int <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/output.irp.f#L73>`_
  Write an integer value in output

`write_time <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files/output.irp.f#L42>`_
  Write a time stamp in the output for chronological reconstruction




