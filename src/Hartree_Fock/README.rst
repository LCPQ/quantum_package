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

`fock_matrix_alpha_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L/BEGIN_PROVIDER [ double precision, Fock_matrix_alpha_ao, (ao_num_align, ao_num) ]/;">`_
  Alpha Fock matrix in AO basis set

`fock_matrix_alpha_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L/BEGIN_PROVIDER [ double precision, Fock_matrix_alpha_mo, (mo_tot_num_align,mo_tot_num) ]/;">`_
  Fock matrix on the MO basis

`fock_matrix_beta_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L/&BEGIN_PROVIDER [ double precision, Fock_matrix_beta_ao,  (ao_num_align, ao_num) ]/;">`_
  Alpha Fock matrix in AO basis set

`fock_matrix_beta_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L/BEGIN_PROVIDER [ double precision, Fock_matrix_beta_mo, (mo_tot_num_align,mo_tot_num) ]/;">`_
  Fock matrix on the MO basis

`fock_matrix_diag_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L/&BEGIN_PROVIDER [ double precision, Fock_matrix_diag_mo, (mo_tot_num)]/;">`_
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

`fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L/BEGIN_PROVIDER [ double precision, Fock_matrix_mo, (mo_tot_num_align,mo_tot_num) ]/;">`_
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

`hf_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/Fock_matrix.irp.f#L/BEGIN_PROVIDER [ double precision, HF_energy ]/;">`_
  Hartree-Fock energy

`hf_density_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L/BEGIN_PROVIDER [ double precision, HF_density_matrix_ao, (ao_num_align,ao_num) ]/;">`_
  Density matrix in the AO basis

`hf_density_matrix_ao_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L/BEGIN_PROVIDER [ double precision, HF_density_matrix_ao_alpha, (ao_num_align,ao_num) ]/;">`_
  Alpha and Beta density matrix in the AO basis

`hf_density_matrix_ao_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/HF_density_matrix_ao.irp.f#L/&BEGIN_PROVIDER [ double precision, HF_density_matrix_ao_beta,  (ao_num_align,ao_num) ]/;">`_
  Alpha and Beta density matrix in the AO basis

`diagonal_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L/BEGIN_PROVIDER [ double precision, diagonal_Fock_matrix_mo, (mo_tot_num) ]/;">`_
  Diagonal Fock matrix in the MO basis

`eigenvectors_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/diagonalize_fock.irp.f#L/&BEGIN_PROVIDER [ double precision, eigenvectors_Fock_matrix_mo, (ao_num_align,mo_tot_num) ]/;">`_
  Diagonal Fock matrix in the MO basis

`scf_iteration <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/mo_SCF_iterations.irp.f#L/subroutine scf_iteration/;">`_
  Undocumented

`do_diis <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/options.irp.f#L/BEGIN_PROVIDER [ logical, do_DIIS ]/;">`_
  If True, compute integrals on the fly

`n_it_scf_max <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/options.irp.f#L/BEGIN_PROVIDER [ integer, n_it_scf_max]/;">`_
  Maximum number of SCF iterations

`thresh_scf <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/options.irp.f#L/BEGIN_PROVIDER [ double precision,thresh_SCF ]/;">`_
  Threshold on the convergence of the Hartree Fock energy

`bi_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L/&BEGIN_PROVIDER [ double precision, bi_elec_ref_bitmask_energy ]/;">`_
  Energy of the reference bitmask used in Slater rules

`kinetic_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L/&BEGIN_PROVIDER [ double precision, kinetic_ref_bitmask_energy ]/;">`_
  Energy of the reference bitmask used in Slater rules

`mono_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L/&BEGIN_PROVIDER [ double precision, mono_elec_ref_bitmask_energy ]/;">`_
  Energy of the reference bitmask used in Slater rules

`nucl_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L/&BEGIN_PROVIDER [ double precision, nucl_elec_ref_bitmask_energy ]/;">`_
  Energy of the reference bitmask used in Slater rules

`ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock/ref_bitmask.irp.f#L/BEGIN_PROVIDER [ double precision, ref_bitmask_energy ]/;">`_
  Energy of the reference bitmask used in Slater rules



