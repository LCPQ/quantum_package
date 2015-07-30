===========
MRCC Module
===========

Multi-Reference Coupled Cluster.

Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `Perturbation <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation>`_
* `Selectors_full <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full>`_
* `Generators_full <http://github.com/LCPQ/quantum_package/tree/master/src/Generators_full>`_
* `Psiref_Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Psiref_Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. by the `update_README.py` script.

`apply_excitation_operator <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_dress.irp.f#L132>`_
  Undocumented


`ci_eigenvectors_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L84>`_
  Eigenvectors/values of the CI matrix


`ci_eigenvectors_s2_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L85>`_
  Eigenvectors/values of the CI matrix


`ci_electronic_energy_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L83>`_
  Eigenvectors/values of the CI matrix


`ci_energy_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L150>`_
  N_states lowest eigenvalues of the dressed CI matrix


`davidson_diag_hjj_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/davidson.irp.f#L56>`_
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


`davidson_diag_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/davidson.irp.f#L1>`_
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


`delta_ii <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L45>`_
  Dressing matrix in N_det basis


`delta_ij <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L44>`_
  Dressing matrix in N_det basis


`diagonalize_ci_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L165>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix


`get_excitation_operators_for_one_ref <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_amplitudes.irp.f#L1>`_
  This subroutine provides all the amplitudes and excitation operators
  that one needs to go from the reference to the non reference wave function
  you enter with det_ref that is a reference determinant
  .br
  N_connect_ref is the number of determinants belonging to psi_non_ref
  that are connected to det_ref.
  .br
  amplitudes_phase_less(i) = amplitude phase less t_{I->i} = <I|H|i> * lambda_mrcc(i) * phase(I->i)
  .br
  excitation_operators(:,i) represents the holes and particles that
  link the ith connected determinant to det_ref
  if                           ::
  excitation_operators(5,i) =  2 :: double excitation alpha
  excitation_operators(5,i) = -2 :: double excitation beta
  !! excitation_operators(1,i)  :: hole 1
  !! excitation_operators(2,i)  :: particle 1
  !! excitation_operators(3,i)  :: hole 2
  !! excitation_operators(4,i)  :: particle 2
  else if                      ::
  excitation_operators(5,i) =  1 :: single excitation alpha
  !! excitation_operators(1,i)  :: hole 1
  !! excitation_operators(2,i)  :: particle 1
  else if                      ::
  excitation_operators(5,i) = -1 :: single excitation beta
  !! excitation_operators(3,i)  :: hole 1
  !! excitation_operators(4,i)  :: particle 1
  else if                      ::
  !! excitation_operators(5,i) =  0 :: double excitation alpha/beta
  !! excitation_operators(1,i)  :: hole 1 alpha
  !! excitation_operators(2,i)  :: particle 1 alpha
  !! excitation_operators(3,i)  :: hole 2 beta
  !! excitation_operators(4,i)  :: particle 2 beta


`h_matrix_dressed <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L58>`_
  Dressed H with Delta_ij


`h_u_0_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/davidson.irp.f#L360>`_
  Computes v_0 = H|u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>


`lambda_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L1>`_
  cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)


`lambda_pert <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_utils.irp.f#L2>`_
  cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)


`mrcc_dress <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_dress.irp.f#L1>`_
  Undocumented


`mrcc_iterations <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_general.irp.f#L7>`_
  Undocumented


`run_mrcc <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_general.irp.f#L1>`_
  Undocumented


`set_generators_bitmasks_as_holes_and_particles <http://github.com/LCPQ/quantum_package/tree/master/src/MRCC_Utils_new/mrcc_general.irp.f#L39>`_
  Undocumented

