==========
MOs Module
==========

Molecular orbitals are expressed as 

.. math::

  \phi_k({\bf r}) = \sum_i C_{ik} \chi_k({\bf r})

where :math:`\chi_k` are *normalized* atomic basis set.

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
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`cholesky_mo <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/cholesky_mo.irp.f#L1>`_
  Cholesky decomposition of AO Density matrix to
  generate MOs

`mo_density_matrix <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/cholesky_mo.irp.f#L44>`_
  Density matrix in MO basis

`mo_density_matrix_virtual <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/cholesky_mo.irp.f#L64>`_
  Density matrix in MO basis (virtual MOs)

`mo_overlap <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mo_overlap.irp.f#L2>`_
  Undocumented

`ao_to_mo <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L126>`_
  Transform A from the AO basis to the MO basis

`mix_mo_jk <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L210>`_
  subroutine that rotates the jth MO with the kth MO
  to give two new MO's that are
  '+' = 1/sqrt(2) (|j> + |k>)
  '-' = 1/sqrt(2) (|j> - |k>)
  by convention, the '+' MO is in the lower index (min(j,k))
  by convention, the '-' MO is in the greater index (max(j,k))

`mo_coef <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L28>`_
  Molecular orbital coefficients on AO basis set
  mo_coef(i,j) = coefficient of the ith ao on the jth mo
  mo_label : Label characterizing the MOS (local, canonical, natural, etc)

`mo_coef_transp <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L71>`_
  Molecular orbital coefficients on AO basis set

`mo_label <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L29>`_
  Molecular orbital coefficients on AO basis set
  mo_coef(i,j) = coefficient of the ith ao on the jth mo
  mo_label : Label characterizing the MOS (local, canonical, natural, etc)

`mo_occ <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L102>`_
  MO occupation numbers

`mo_to_ao <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L152>`_
  Transform A from the MO basis to the AO basis

`mo_to_ao_no_overlap <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L184>`_
  Transform A from the MO basis to the S^-1 AO basis

`mo_tot_num <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L1>`_
  Total number of molecular orbitals and the size of the keys corresponding

`mo_tot_num_align <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L18>`_
  Aligned variable for dimensioning of arrays

`s_mo_coef <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/mos.irp.f#L89>`_
  Product S.C where S is the overlap matrix in the AO basis and C the mo_coef matrix.

`mo_as_eigvectors_of_mo_matrix <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/utils.irp.f#L24>`_
  Undocumented

`mo_as_eigvectors_of_mo_matrix_sort_by_observable <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/utils.irp.f#L62>`_
  Undocumented

`mo_sort_by_observable <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/utils.irp.f#L144>`_
  Undocumented

`save_mos <http://github.com/LCPQ/quantum_package/tree/master/src/MOs/utils.irp.f#L1>`_
  Undocumented



