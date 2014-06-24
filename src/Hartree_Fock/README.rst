===================
Hartree-Fock Module
===================


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

`fock_matrix_alpha_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L83>`_
  Alpha Fock matrix in AO basis set

`fock_matrix_alpha_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L172>`_
  Fock matrix on the MO basis

`fock_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L220>`_
  Fock matrix in AO basis set

`fock_matrix_beta_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L84>`_
  Alpha Fock matrix in AO basis set

`fock_matrix_beta_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L192>`_
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

`hf_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L211>`_
  Hartree-Fock energy

`hf_density_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L27>`_
  Density matrix in the AO basis

`hf_density_matrix_ao_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L1>`_
  Alpha density matrix in the AO basis

`hf_density_matrix_ao_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L14>`_
  Beta density matrix in the AO basis

`fock_mo_to_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L31>`_
  Undocumented

`insert_new_scf_density_matrix <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L19>`_
  Undocumented

`it_scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L1>`_
  Number of the current SCF iteration

`scf_density_matrices <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L9>`_
  Density matrices at every SCF iteration

`scf_energies <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L10>`_
  Density matrices at every SCF iteration

`scf_interpolation_step <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L64>`_
  Undocumented

`scf_iterations <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/SCF.irp.f#L89>`_
  Undocumented

`diagonal_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L1>`_
  Diagonal Fock matrix in the MO basis

`eigenvectors_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L2>`_
  Diagonal Fock matrix in the MO basis

`run <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/mo_SCF_iterations.irp.f#L7>`_
  Undocumented

`scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/mo_SCF_iterations.irp.f#L2>`_
  Undocumented

`do_diis <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/options.irp.f#L39>`_
  If True, compute integrals on the fly

`n_it_scf_max <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/options.irp.f#L21>`_
  Maximum number of SCF iterations

`thresh_scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/options.irp.f#L1>`_
  Threshold on the convergence of the Hartree Fock energy

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



