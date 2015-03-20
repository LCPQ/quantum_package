===================
Hartree-Fock Module
===================

From the 140 molecules of the G2 set, only LiO, ONa don't converge well.

Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `BiInts <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `MonoInts <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`ao_bi_elec_integral_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L102>`_
  Alpha Fock matrix in AO basis set

`ao_bi_elec_integral_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L103>`_
  Alpha Fock matrix in AO basis set

`fock_matrix_alpha_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L83>`_
  Alpha Fock matrix in AO basis set

`fock_matrix_alpha_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L194>`_
  Fock matrix on the MO basis

`fock_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L242>`_
  Fock matrix in AO basis set

`fock_matrix_beta_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L84>`_
  Alpha Fock matrix in AO basis set

`fock_matrix_beta_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L214>`_
  Fock matrix on the MO basis

`fock_matrix_diag_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L2>`_
  Fock matrix on the MO basis.
  For open shells, the ROHF Fock Matrix is
  .br
  |   F-K    |  F + K/2  |    F     |
  |---------------------------------|
  | F + K/2  |     F     |  F - K/2 |
  |---------------------------------|
  |    F     |  F - K/2  |  F + K   |
  .br
  F = 1/2 (Fa + Fb)
  .br
  K = Fb - Fa
  .br

`fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L1>`_
  Fock matrix on the MO basis.
  For open shells, the ROHF Fock Matrix is
  .br
  |   F-K    |  F + K/2  |    F     |
  |---------------------------------|
  | F + K/2  |     F     |  F - K/2 |
  |---------------------------------|
  |    F     |  F - K/2  |  F + K   |
  .br
  F = 1/2 (Fa + Fb)
  .br
  K = Fb - Fa
  .br

`fock_mo_to_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L285>`_
  Undocumented

`hf_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L233>`_
  Hartree-Fock energy

`hf_density_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L27>`_
  Density matrix in the AO basis

`hf_density_matrix_ao_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L1>`_
  Alpha density matrix in the AO basis

`hf_density_matrix_ao_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L14>`_
  Beta density matrix in the AO basis

`run <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L7>`_
  Undocumented

`scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L2>`_
  Undocumented

`damping_scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/damping_SCF.irp.f#L1>`_
  Undocumented

`diagonal_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L1>`_
  Diagonal Fock matrix in the MO basis

`diagonal_fock_matrix_mo_sum <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L57>`_
  diagonal element of the fock matrix calculated as the sum over all the interactions
  with all the electrons in the RHF determinant
  diagonal_Fock_matrix_mo_sum(i) = sum_{j=1, N_elec} 2 J_ij -K_ij

`eigenvectors_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L2>`_
  Diagonal Fock matrix in the MO basis

`bi_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L5>`_
  Energy of the reference bitmask used in Slater rules

`kinetic_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L3>`_
  Energy of the reference bitmask used in Slater rules

`mono_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L2>`_
  Energy of the reference bitmask used in Slater rules

`nucl_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L4>`_
  Energy of the reference bitmask used in Slater rules

`ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L1>`_
  Energy of the reference bitmask used in Slater rules



