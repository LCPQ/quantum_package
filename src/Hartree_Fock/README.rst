===================
Hartree-Fock Module
===================

From the 140 molecules of the G2 set, only LiO, ONa don't converge well.

Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `Bielec_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/Bielec_integrals>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `MonoInts <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts>`_
* `MOGuess <http://github.com/LCPQ/quantum_package/tree/master/src/MOGuess>`_
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

`fock_matrix_alpha_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L231>`_
  Fock matrix on the MO basis

`fock_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L289>`_
  Fock matrix in AO basis set

`fock_matrix_beta_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L84>`_
  Alpha Fock matrix in AO basis set

`fock_matrix_beta_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L251>`_
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

`fock_mo_to_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L332>`_
  Undocumented

`hf_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L270>`_
  Hartree-Fock energy

`hf_density_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L27>`_
  S^-1 Density matrix in the AO basis S^-1

`hf_density_matrix_ao_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L1>`_
  S^-1 x Alpha density matrix in the AO basis x S^-1

`hf_density_matrix_ao_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L14>`_
  S^-1 Beta density matrix in the AO basis x S^-1

`guess <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Huckel_guess.irp.f#L1>`_
  Undocumented

`create_guess <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L8>`_
  Create an MO guess if no MOs are present in the EZFIO directory

`run <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L33>`_
  Run SCF calculation

`scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L2>`_
  Undocumented

`damping_scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/damping_SCF.irp.f#L1>`_
  Undocumented

`diagonal_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L1>`_
  Diagonal Fock matrix in the MO basis

`diagonal_fock_matrix_mo_sum <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L67>`_
  diagonal element of the fock matrix calculated as the sum over all the interactions
  with all the electrons in the RHF determinant
  diagonal_Fock_matrix_mo_sum(i) = sum_{j=1, N_elec} 2 J_ij -K_ij

`eigenvectors_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L2>`_
  Diagonal Fock matrix in the MO basis

`huckel_guess <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/huckel.irp.f#L1>`_
  Build the MOs using the extended Huckel model



