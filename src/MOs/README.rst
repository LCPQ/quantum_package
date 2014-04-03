Modifying MOs
=============

When modifying MOs, the label of the current MOs should change accordingly by changing the mo_label. For example:

.. code-block::  fortran

  mo_coef = new_mo_coef
  mo_label = "MyNewLabel"
  TOUCH mo_coef mo_label


