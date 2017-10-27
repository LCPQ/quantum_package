===================
Hartree-Fock Module
===================

From the 140 molecules of the G2 set, only LiO, ONa don't converge well.

Needed Modules
==============

.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `Integrals_Bielec <http://github.com/LCPQ/quantum_package/tree/master/src/Integrals_Bielec>`_
* `MOGuess <http://github.com/LCPQ/quantum_package/tree/master/src/MOGuess>`_

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Integrals_Bielec <http://github.com/LCPQ/quantum_package/tree/master/src/Integrals_Bielec>`_
* `MOGuess <http://github.com/LCPQ/quantum_package/tree/master/src/MOGuess>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`ao_bi_elec_integral_alpha <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L102>`_
  Alpha Fock matrix in AO basis set


`ao_bi_elec_integral_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L103>`_
  Alpha Fock matrix in AO basis set


`create_guess <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/SCF.irp.f#L13>`_
  Create a MO guess if no MOs are present in the EZFIO directory


`damping_scf <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/damping_SCF.irp.f#L1>`_
  Undocumented


`diagonal_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/diagonalize_fock.irp.f#L1>`_
  Diagonal Fock matrix in the MO basis


`diagonal_fock_matrix_mo_sum <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/diagonalize_fock.irp.f#L105>`_
  diagonal element of the fock matrix calculated as the sum over all the interactions
  with all the electrons in the RHF determinant
  diagonal_Fock_matrix_mo_sum(i) = sum_{j=1, N_elec} 2 J_ij -K_ij


`eigenvalues_fock_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/DIIS.irp.f#L73>`_
  Eigenvalues and eigenvectors of the Fock matrix over the AO basis


`eigenvectors_fock_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/DIIS.irp.f#L74>`_
  Eigenvalues and eigenvectors of the Fock matrix over the AO basis


`eigenvectors_fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/diagonalize_fock.irp.f#L2>`_
  Diagonal Fock matrix in the MO basis


`extrapolate_fock_matrix <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Roothaan_Hall_SCF.irp.f#L146>`_
  Compute the extrapolated Fock matrix using the DIIS procedure


`fock_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L317>`_
  Fock matrix in AO basis set


`fock_matrix_ao_alpha <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L84>`_
  Alpha Fock matrix in AO basis set


`fock_matrix_ao_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L85>`_
  Alpha Fock matrix in AO basis set


`fock_matrix_diag_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L2>`_
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


`fock_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L1>`_
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


`fock_matrix_mo_alpha <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L261>`_
  Fock matrix on the MO basis


`fock_matrix_mo_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L280>`_
  Fock matrix on the MO basis


`fock_mo_to_ao <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L378>`_
  Undocumented


`fps_spf_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/DIIS.irp.f#L15>`_
  Commutator FPS - SPF


`fps_spf_matrix_mo <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/DIIS.irp.f#L63>`_
  Commutator FPS - SPF in MO basis


`guess <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Huckel_guess.irp.f#L1>`_
  Undocumented


`hf_density_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/HF_density_matrix_ao.irp.f#L27>`_
  S^-1 Density matrix in the AO basis S^-1


`hf_density_matrix_ao_alpha <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/HF_density_matrix_ao.irp.f#L1>`_
  S^-1 x Alpha density matrix in the AO basis x S^-1


`hf_density_matrix_ao_beta <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/HF_density_matrix_ao.irp.f#L14>`_
  S^-1 Beta density matrix in the AO basis x S^-1


`hf_energy <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Fock_matrix.irp.f#L298>`_
  Hartree-Fock energy


`huckel_guess <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/huckel.irp.f#L1>`_
  Build the MOs using the extended Huckel model


`level_shift <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L44>`_
  Energy shift on the virtual MOs to improve SCF convergence


`localize_mos <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/localize_mos.irp.f#L1>`_
  Undocumented


`max_dim_diis <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L6>`_
  Maximum size of the DIIS extrapolation procedure


`mo_guess_type <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L82>`_
  Initial MO guess. Can be [ Huckel | HCore ]


`n_it_scf_max <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L139>`_
  Maximum number of SCF iterations


`no_oa_or_av_opt <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L120>`_
  If true, skip the (inactive+core) --> (active) and the (active) --> (virtual) orbital rotations within the SCF procedure


`roothaan_hall_scf <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/Roothaan_Hall_SCF.irp.f#L1>`_
  Roothaan-Hall algorithm for SCF Hartree-Fock calculation


`run <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/SCF.irp.f#L37>`_
  Run SCF calculation


`scf <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/SCF.irp.f#L1>`_
  Produce `Hartree_Fock` MO orbital
  output: mo_basis.mo_tot_num mo_basis.mo_label mo_basis.ao_md5 mo_basis.mo_coef mo_basis.mo_occ
  output: hartree_fock.energy
  optional: mo_basis.mo_coef


`scf_algorithm <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L101>`_
  Type of SCF algorithm used. Possible choices are [ Simple | DIIS]


`thresh_scf <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L158>`_
  Threshold on the convergence of the Hartree Fock energy.


`threshold_diis <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L63>`_
  Threshold on the convergence of the DIIS error vector during a Hartree-Fock calculation. If 0. is chosen, the square root of thresh_scf will be used.


`threshold_diis_nonzero <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/DIIS.irp.f#L1>`_
  If threshold_DIIS is zero, choose sqrt(thresh_scf)


`threshold_overlap_ao_eigenvalues <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/ezfio_interface.irp.f#L25>`_
  Threshold on the magnitude of the smallest eigenvalues of the overlap matrix in the AO basis


`x_matrix_ao <http://github.com/LCPQ/quantum_package/tree/master/plugins/Hartree_Fock/DIIS.irp.f#L140>`_
  Matrix X = S^{-1/2} obtained by SVD

