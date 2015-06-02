===========
MRCC Module
===========

Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

.. image:: tree_dependancy.png

* `Perturbation <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation>`_
* `Selectors_full <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full>`_
* `Generators_full <http://github.com/LCPQ/quantum_package/tree/master/src/Generators_full>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`davidson_diag_hjj_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/davidson.irp.f#L51>`_
  Davidson diagonalization with specific diagonal elements of the H matrix
  .br
  H_jj : specific diagonal H matrix elements to diagonalize de Davidson
  .br
  dets_in : bitmasks corresponding to determinants
  .br
  u_in : guess coefficients on the various states. Overwritten
  on exit
  .br
  dim_in : leftmost dimension of u_in
  .br
  sze : Number of determinants
  .br
  N_st : Number of eigenstates
  .br
  iunit : Unit for the I/O
  .br
  Initial guess vectors are not necessarily orthonormal

`davidson_diag_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/davidson.irp.f#L1>`_
  Davidson diagonalization.
  .br
  dets_in : bitmasks corresponding to determinants
  .br
  u_in : guess coefficients on the various states. Overwritten
  on exit
  .br
  dim_in : leftmost dimension of u_in
  .br
  sze : Number of determinants
  .br
  N_st : Number of eigenstates
  .br
  iunit : Unit number for the I/O
  .br
  Initial guess vectors are not necessarily orthonormal

`h_u_0_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/davidson.irp.f#L355>`_
  Computes v_0 = H|u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>

`mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc.irp.f#L1>`_
  Undocumented

`run <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc.irp.f#L10>`_
  Undocumented

`run_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc.irp.f#L42>`_
  Undocumented

`run_mrcc_test <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc.irp.f#L29>`_
  Undocumented

`find_triples_and_quadruples <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_dress.irp.f#L202>`_
  Undocumented

`mrcc_dress <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_dress.irp.f#L15>`_
  Undocumented

`mrcc_dress_simple <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_dress.irp.f#L156>`_
  Undocumented

`psi_cas_lock <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_dress.irp.f#L3>`_
  Locks on CAS determinants to fill delta_ij

`ci_eigenvectors_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L79>`_
  Eigenvectors/values of the CI matrix

`ci_eigenvectors_s2_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L80>`_
  Eigenvectors/values of the CI matrix

`ci_electronic_energy_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L78>`_
  Eigenvectors/values of the CI matrix

`ci_energy_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L145>`_
  N_states lowest eigenvalues of the dressed CI matrix

`delta_ij <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L43>`_
  Dressing matrix in N_det basis

`delta_ij_non_cas <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L34>`_
  Dressing matrix in SD basis

`diagonalize_ci_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L160>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix

`dressing_type <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L26>`_
  [ Simple | MRCC ]

`h_matrix_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L63>`_
  Dressed H with Delta_ij

`lambda_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC/mrcc_utils.irp.f#L2>`_
  cm/<Psi_0|H|D_m>



