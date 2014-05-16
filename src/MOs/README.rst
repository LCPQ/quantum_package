==========
MOs Module
==========

Molecular orbitals are expressed as 

.. math::

  \phi_k({\bf r}) = \sum_i C_{ik} \chi_k({\bf r})

The current set of molecular orbitals has a label ``mo_label``.
When the orbitals are modified, the label should also be updated to keep
everything consistent.

When saving the MOs, the ``mo_basis`` directory of the EZFIO file is copied
in the ``save`` directory, named by the current ``mo_label``. All this is
done with the script named ``save_current_mos.sh`` in the ``scripts`` directory.

Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`mo_coef <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L22>`_
  Molecular orbital coefficients on AO basis set
  mo_coef(i,j) = coefficient of the ith ao on the jth mo
  mo_label : Label characterizing the MOS (local, canonical, natural, etc)

`mo_coef_transp <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L61>`_
  Molecular orbital coefficients on AO basis set

`mo_label <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L23>`_
  Molecular orbital coefficients on AO basis set
  mo_coef(i,j) = coefficient of the ith ao on the jth mo
  mo_label : Label characterizing the MOS (local, canonical, natural, etc)

`mo_tot_num <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L1>`_
  Total number of molecular orbitals and the size of the keys corresponding

`mo_tot_num_align <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L12>`_
  Aligned variable for dimensioning of arrays

`mo_as_eigvectors_of_mo_matrix <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/utils.irp.f#L21>`_
  Undocumented

`save_mos <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/utils.irp.f#L1>`_
  Undocumented



