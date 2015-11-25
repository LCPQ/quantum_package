Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Integrals_Monoelec <http://github.com/LCPQ/quantum_package/tree/master/src/Integrals_Monoelec>`_
* `Integrals_Bielec <http://github.com/LCPQ/quantum_package/tree/master/src/Integrals_Bielec>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`a_operator <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1398>`_
  Needed for diag_H_mat_elem


`abs_psi_coef_max <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L452>`_
  Max and min values of the coefficients


`abs_psi_coef_min <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L453>`_
  Max and min values of the coefficients


`ac_operator <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1444>`_
  Needed for diag_H_mat_elem


`apply_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/excitations_utils.irp.f#L1>`_
  Undocumented


`bi_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ref_bitmask.irp.f#L5>`_
  Energy of the reference bitmask used in Slater rules


`bitstring_to_list_ab <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L352>`_
  Gives the inidices(+1) of the bits set to 1 in the bit string
  For alpha/beta determinants


`bitstring_to_list_ab_old <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L390>`_
  Gives the inidices(+1) of the bits set to 1 in the bit string
  For alpha/beta determinants


`build_fock_tmp <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/Fock_diag.irp.f#L1>`_
  Build the diagonal of the Fock matrix corresponding to a generator
  determinant. F_00 is <i|H|i> = E0.


`ci_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI.irp.f#L37>`_
  Eigenvectors/values of the CI matrix


`ci_eigenvectors_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_mono.irp.f#L2>`_
  Eigenvectors/values of the CI matrix


`ci_eigenvectors_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI.irp.f#L38>`_
  Eigenvectors/values of the CI matrix


`ci_eigenvectors_s2_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_mono.irp.f#L3>`_
  Eigenvectors/values of the CI matrix


`ci_electronic_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI.irp.f#L36>`_
  Eigenvectors/values of the CI matrix


`ci_electronic_energy_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_mono.irp.f#L1>`_
  Eigenvectors/values of the CI matrix


`ci_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI.irp.f#L18>`_
  N_states lowest eigenvalues of the CI matrix


`ci_sc2_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_SC2.irp.f#L27>`_
  Eigenvectors/values of the CI matrix


`ci_sc2_electronic_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_SC2.irp.f#L26>`_
  Eigenvectors/values of the CI matrix


`ci_sc2_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_SC2.irp.f#L1>`_
  N_states_diag lowest eigenvalues of the CI matrix


`cisd <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/truncate_wf.irp.f#L1>`_
  Undocumented


`cisd_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/SC2.irp.f#L1>`_
  CISD+SC2 method              :: take off all the disconnected terms of a CISD (selected or not)
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
  Initial guess vectors are not necessarily orthonormal


`connected_to_ref <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/connected_to_ref.irp.f#L192>`_
  Undocumented


`connected_to_ref_by_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/connected_to_ref.irp.f#L290>`_
  Undocumented


`copy_h_apply_buffer_to_wf <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/H_apply.irp.f#L112>`_
  Copies the H_apply buffer to psi_coef.
  After calling this subroutine, N_det, psi_det and psi_coef need to be touched


`create_minilist <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L840>`_
  Undocumented


`create_minilist_find_previous <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L887>`_
  Undocumented


`create_wf_of_psi_bilinear_matrix <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L445>`_
  Generate a wave function containing all possible products
  of alpha and beta determinants


`davidson_converged <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L604>`_
  True if the Davidson algorithm is converged


`davidson_criterion <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L596>`_
  Can be : [  energy  | residual | both | wall_time | cpu_time | iterations ]


`davidson_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L18>`_
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


`davidson_diag_hjj <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L288>`_
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


`davidson_iter_max <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L1>`_
  Max number of Davidson iterations


`davidson_sze_max <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L9>`_
  Max number of Davidson sizes


`decode_exc <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L76>`_
  Decodes the exc arrays returned by get_excitation.
  h1,h2 : Holes
  p1,p2 : Particles
  s1,s2 : Spins (1:alpha, 2:beta)
  degree : Degree of excitation


`det_alpha_norm <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L353>`_
  Norm of the alpha and beta spin determinants in the wave function:
  .br
  ||Da||_i \sum_j C_{ij}**2


`det_beta_norm <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L354>`_
  Norm of the alpha and beta spin determinants in the wave function:
  .br
  ||Da||_i \sum_j C_{ij}**2


`det_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L138>`_
  det_coef


`det_inf <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L69>`_
  Undocumented


`det_occ <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L248>`_
  det_occ


`det_search_key <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/connected_to_ref.irp.f#L1>`_
  Return an integer*8 corresponding to a determinant index for searching


`det_to_occ_pattern <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/occ_pattern.irp.f#L2>`_
  Transform a determinant to an occupation pattern


`diag_algorithm <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI.irp.f#L1>`_
  Diagonalization algorithm (Davidson or Lapack)


`diag_h_mat_elem <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1336>`_
  Computes <i|H|i>


`diag_h_mat_elem_fock <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1267>`_
  Computes <i|H|i> when i is at most a double excitation from
  a reference.


`diagonalize_ci <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI.irp.f#L105>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix


`diagonalize_ci_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_mono.irp.f#L73>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix


`diagonalize_ci_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_SC2.irp.f#L45>`_
  Replace the coefficients of the CI states_diag by the coefficients of the
  eigenstates of the CI matrix


`do_mono_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/create_excitations.irp.f#L1>`_
  Apply the mono excitation operator : a^{dager}_(i_particle) a_(i_hole) of spin = ispin
  on key_in
  ispin = 1  == alpha
  ispin = 2  == beta
  i_ok = 1  == the excitation is possible
  i_ok = -1 == the excitation is not possible


`double_exc_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants_bitmasks.irp.f#L40>`_
  double_exc_bitmask(:,1,i) is the bitmask for holes of excitation 1
  double_exc_bitmask(:,2,i) is the bitmask for particles of excitation 1
  double_exc_bitmask(:,3,i) is the bitmask for holes of excitation 2
  double_exc_bitmask(:,4,i) is the bitmask for particles of excitation 2
  for a given couple of hole/particle excitations i.


`expected_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/s2.irp.f#L48>`_
  Expected value of S2 : S*(S+1)


`fill_h_apply_buffer_no_selection <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/H_apply.irp.f#L266>`_
  Fill the H_apply buffer with determiants for CISD


`filter_3_highest_electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L424>`_
  Returns a determinant with only the 3 highest electrons


`filter_connected <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/filter_connected.irp.f#L2>`_
  Filters out the determinants that are not connected by H
  .br
  returns the array idx which contains the index of the
  .br
  determinants in the array key1 that interact
  .br
  via the H operator with key2.
  .br
  idx(0) is the number of determinants that interact with key1


`filter_connected_i_h_psi0 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/filter_connected.irp.f#L101>`_
  returns the array idx which contains the index of the
  .br
  determinants in the array key1 that interact
  .br
  via the H operator with key2.
  .br
  idx(0) is the number of determinants that interact with key1


`filter_connected_i_h_psi0_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/filter_connected.irp.f#L205>`_
  standard filter_connected_i_H_psi but returns in addition
  .br
  the array of the index of the non connected determinants to key1
  .br
  in order to know what double excitation can be repeated on key1
  .br
  idx_repeat(0) is the number of determinants that can be used
  .br
  to repeat the excitations


`first_guess <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/guess_lowest_state.irp.f#L1>`_
  Select all the determinants with the lowest energy as a starting point.


`generate_all_alpha_beta_det_products <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L503>`_
  Create a wave function from all possible alpha x beta determinants


`get_double_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L142>`_
  Returns the two excitation operators between two doubly excited determinants and the phase


`get_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L30>`_
  Returns the excitation operators between two determinants and the phase


`get_excitation_degree <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1>`_
  Returns the excitation degree between two determinants


`get_excitation_degree_vector <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1172>`_
  Applies get_excitation_degree to an array of determinants


`get_index_in_psi_det_alpha_unique <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L141>`_
  Returns the index of the determinant in the ``psi_det_alpha_unique`` array


`get_index_in_psi_det_beta_unique <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L223>`_
  Returns the index of the determinant in the ``psi_det_beta_unique`` array


`get_index_in_psi_det_sorted_bit <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/connected_to_ref.irp.f#L50>`_
  Returns the index of the determinant in the ``psi_det_sorted_bit`` array


`get_mono_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L275>`_
  Returns the excitation operator between two singly excited determinants and the phase


`get_occ_from_key <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1490>`_
  Returns a list of occupation numbers from a bitstring


`get_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/s2.irp.f#L1>`_
  Returns <S^2>


`get_s2_u0 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/s2.irp.f#L109>`_
  Undocumented


`get_s2_u0_old <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/s2.irp.f#L82>`_
  Undocumented


`h_apply_buffer_allocated <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/H_apply.irp.f#L15>`_
  Buffer of determinants/coefficients/perturbative energy for H_apply.
  Uninitialized. Filled by H_apply subroutines.


`h_apply_buffer_lock <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/H_apply.irp.f#L16>`_
  Buffer of determinants/coefficients/perturbative energy for H_apply.
  Uninitialized. Filled by H_apply subroutines.


`h_matrix_all_dets <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/utils.irp.f#L1>`_
  H matrix on the basis of the slater determinants defined by psi_det


`h_matrix_cas <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L115>`_
  Undocumented


`h_u_0 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1506>`_
  Computes v_0 = H|u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>


`i_h_j <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L430>`_
  Returns <i|H|j> where i and j are determinants


`i_h_j_phase_out <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L566>`_
  Returns <i|H|j> where i and j are determinants


`i_h_j_verbose <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L702>`_
  Returns <i|H|j> where i and j are determinants


`i_h_psi <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L940>`_
  Computes <i|H|Psi> = \sum_J c_J <i|H|J>.
  .br
  Uses filter_connected_i_H_psi0 to get all the |J> to which |i>
  is connected.
  The i_H_psi_minilist is much faster but requires to build the
  minilists


`i_h_psi_minilist <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L982>`_
  Computes <i|H|Psi> = \sum_J c_J <i|H|J>.
  .br
  Uses filter_connected_i_H_psi0 to get all the |J> to which |i>
  is connected. The |J> are searched in short pre-computed lists.


`i_h_psi_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1069>`_
  <key|H|psi> for the various Nstate
  .br
  returns in addition
  .br
  the array of the index of the non connected determinants to key1
  .br
  in order to know what double excitation can be repeated on key1
  .br
  idx_repeat(0) is the number of determinants that can be used
  .br
  to repeat the excitations


`i_h_psi_sc2_verbose <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1116>`_
  <key|H|psi> for the various Nstate
  .br
  returns in addition
  .br
  the array of the index of the non connected determinants to key1
  .br
  in order to know what double excitation can be repeated on key1
  .br
  idx_repeat(0) is the number of determinants that can be used
  .br
  to repeat the excitations


`i_h_psi_sec_ord <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/slater_rules.irp.f#L1022>`_
  <key|H|psi> for the various Nstates


`idx_cas <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L5>`_
  CAS wave function, defined from the application of the CAS bitmask on the
  determinants. idx_cas gives the indice of the CAS determinant in psi_det.


`idx_non_cas <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L65>`_
  Set of determinants which are not part of the CAS, defined from the application
  of the CAS bitmask on the determinants.
  idx_non_cas gives the indice of the determinant in psi_det.


`int_of_3_highest_electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L389>`_
  Returns an integer*8 as :
  .br
  |_<--- 21 bits ---><--- 21 bits ---><--- 21 bits --->|
  .br
  |0<---   i1    ---><---   i2    ---><---   i3    --->|
  .br
  It encodes the value of the indices of the 3 highest MOs
  in descending order
  .br


`is_connected_to <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/connected_to_ref.irp.f#L158>`_
  Undocumented


`is_in_wavefunction <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/connected_to_ref.irp.f#L36>`_
  True if the determinant ``det`` is in the wave function


`kinetic_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ref_bitmask.irp.f#L3>`_
  Energy of the reference bitmask used in Slater rules


`make_s2_eigenfunction <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/occ_pattern.irp.f#L251>`_
  Undocumented


`max_degree_exc <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L33>`_
  Maximum degree of excitation in the wf


`mono_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ref_bitmask.irp.f#L2>`_
  Energy of the reference bitmask used in Slater rules


`n_det <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L3>`_
  Number of determinants in the wave function


`n_det_alpha_unique <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f_template_136#L4>`_
  Unique alpha determinants


`n_det_beta_unique <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f_template_136#L80>`_
  Unique beta determinants


`n_det_cas <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L6>`_
  CAS wave function, defined from the application of the CAS bitmask on the
  determinants. idx_cas gives the indice of the CAS determinant in psi_det.


`n_det_max <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L50>`_
  Max number of determinants in the wave function


`n_det_max_jacobi <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L94>`_
  Maximum number of determinants diagonalized by Jacobi


`n_det_max_property <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L314>`_
  Max number of determinants in the wave function when you select for a given property


`n_det_non_cas <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L66>`_
  Set of determinants which are not part of the CAS, defined from the application
  of the CAS bitmask on the determinants.
  idx_non_cas gives the indice of the determinant in psi_det.


`n_double_exc_bitmasks <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants_bitmasks.irp.f#L31>`_
  Number of double excitation bitmasks


`n_occ_pattern <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/occ_pattern.irp.f#L143>`_
  array of the occ_pattern present in the wf
  psi_occ_pattern(:,1,j) = jth occ_pattern of the wave function : represent all the single occupation
  psi_occ_pattern(:,2,j) = jth occ_pattern of the wave function : represent all the double occupation


`n_single_exc_bitmasks <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants_bitmasks.irp.f#L8>`_
  Number of single excitation bitmasks


`n_states <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L72>`_
  Number of states to consider


`n_states_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/options.irp.f#L1>`_
  Number of states to consider for the diagonalization


`nucl_elec_ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ref_bitmask.irp.f#L4>`_
  Energy of the reference bitmask used in Slater rules


`occ_pattern_search_key <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/connected_to_ref.irp.f#L18>`_
  Return an integer*8 corresponding to a determinant index for searching


`occ_pattern_to_dets <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/occ_pattern.irp.f#L42>`_
  Generate all possible determinants for a give occ_pattern


`occ_pattern_to_dets_size <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/occ_pattern.irp.f#L20>`_
  Number of possible determinants for a given occ_pattern


`one_body_dm_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L162>`_
  One-body density matrix


`one_body_dm_mo_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L1>`_
  Alpha and beta one-body density matrix for each state


`one_body_dm_mo_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L2>`_
  Alpha and beta one-body density matrix for each state


`one_body_single_double_dm_mo_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L79>`_
  Alpha and beta one-body density matrix for each state


`one_body_single_double_dm_mo_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L80>`_
  Alpha and beta one-body density matrix for each state


`one_body_spin_density_mo <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L170>`_
  rho(alpha) - rho(beta)


`only_single_double_dm <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L182>`_
  If true, The One body DM is calculated with ignoring the Double<->Doubles extra diag elements


`psi_average_norm_contrib <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L274>`_
  Contribution of determinants to the state-averaged density


`psi_average_norm_contrib_sorted <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L304>`_
  Wave function sorted by determinants contribution to the norm (state-averaged)


`psi_bilinear_matrix <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L428>`_
  Coefficient matrix if the wave function is expressed in a bilinear form :
  D_a^t C D_b


`psi_bilinear_matrix_columns <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L390>`_
  Sparse coefficient matrix if the wave function is expressed in a bilinear form :
  D_a^t C D_b


`psi_bilinear_matrix_rows <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L389>`_
  Sparse coefficient matrix if the wave function is expressed in a bilinear form :
  D_a^t C D_b


`psi_bilinear_matrix_values <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L388>`_
  Sparse coefficient matrix if the wave function is expressed in a bilinear form :
  D_a^t C D_b


`psi_cas <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L3>`_
  CAS wave function, defined from the application of the CAS bitmask on the
  determinants. idx_cas gives the indice of the CAS determinant in psi_det.


`psi_cas_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L4>`_
  CAS wave function, defined from the application of the CAS bitmask on the
  determinants. idx_cas gives the indice of the CAS determinant in psi_det.


`psi_cas_coef_sorted_bit <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L50>`_
  CAS determinants sorted to accelerate the search of a random determinant in the wave
  function.


`psi_cas_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L146>`_
  Undocumented


`psi_cas_energy_diagonalized <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L128>`_
  Undocumented


`psi_cas_sorted_bit <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L49>`_
  CAS determinants sorted to accelerate the search of a random determinant in the wave
  function.


`psi_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L228>`_
  The wave function coefficients. Initialized with Hartree-Fock if the EZFIO file
  is empty


`psi_coef_cas_diagonalized <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L127>`_
  Undocumented


`psi_coef_max <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L450>`_
  Max and min values of the coefficients


`psi_coef_min <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L451>`_
  Max and min values of the coefficients


`psi_coef_sorted <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L303>`_
  Wave function sorted by determinants contribution to the norm (state-averaged)


`psi_coef_sorted_ab <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L469>`_
  Determinants on which we apply <i|H|j>.
  They are sorted by the 3 highest electrons in the alpha part,
  then by the 3 highest electrons in the beta part to accelerate
  the research of connected determinants.


`psi_coef_sorted_bit <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L334>`_
  Determinants on which we apply <i|H|psi> for perturbation.
  They are sorted by determinants interpreted as integers. Useful
  to accelerate the search of a random determinant in the wave
  function.


`psi_det <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L66>`_
  The wave function determinants. Initialized with Hartree-Fock if the EZFIO file
  is empty


`psi_det_alpha <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L27>`_
  List of alpha determinants of psi_det


`psi_det_alpha_unique <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f_template_136#L3>`_
  Unique alpha determinants


`psi_det_beta <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L41>`_
  List of beta determinants of psi_det


`psi_det_beta_unique <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f_template_136#L79>`_
  Unique beta determinants


`psi_det_size <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L48>`_
  Size of the psi_det/psi_coef arrays


`psi_det_sorted <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L302>`_
  Wave function sorted by determinants contribution to the norm (state-averaged)


`psi_det_sorted_ab <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L468>`_
  Determinants on which we apply <i|H|j>.
  They are sorted by the 3 highest electrons in the alpha part,
  then by the 3 highest electrons in the beta part to accelerate
  the research of connected determinants.


`psi_det_sorted_bit <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L333>`_
  Determinants on which we apply <i|H|psi> for perturbation.
  They are sorted by determinants interpreted as integers. Useful
  to accelerate the search of a random determinant in the wave
  function.


`psi_det_sorted_next_ab <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L470>`_
  Determinants on which we apply <i|H|j>.
  They are sorted by the 3 highest electrons in the alpha part,
  then by the 3 highest electrons in the beta part to accelerate
  the research of connected determinants.


`psi_non_cas <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L63>`_
  Set of determinants which are not part of the CAS, defined from the application
  of the CAS bitmask on the determinants.
  idx_non_cas gives the indice of the determinant in psi_det.


`psi_non_cas_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L64>`_
  Set of determinants which are not part of the CAS, defined from the application
  of the CAS bitmask on the determinants.
  idx_non_cas gives the indice of the determinant in psi_det.


`psi_non_cas_coef_sorted_bit <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L103>`_
  CAS determinants sorted to accelerate the search of a random determinant in the wave
  function.


`psi_non_cas_sorted_bit <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/psi_cas.irp.f#L102>`_
  CAS determinants sorted to accelerate the search of a random determinant in the wave
  function.


`psi_occ_pattern <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/occ_pattern.irp.f#L142>`_
  array of the occ_pattern present in the wf
  psi_occ_pattern(:,1,j) = jth occ_pattern of the wave function : represent all the single occupation
  psi_occ_pattern(:,2,j) = jth occ_pattern of the wave function : represent all the double occupation


`put_gess <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/guess_triplet.irp.f#L1>`_
  Undocumented


`read_dets <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L599>`_
  Reads the determinants from the EZFIO file


`read_wf <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L116>`_
  If true, read the wave function from the EZFIO file


`rec_occ_pattern_to_dets <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/occ_pattern.irp.f#L102>`_
  Undocumented


`ref_bitmask_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ref_bitmask.irp.f#L1>`_
  Energy of the reference bitmask used in Slater rules


`remove_duplicates_in_psi_det <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/H_apply.irp.f#L198>`_
  Removes duplicate determinants in the wave function.


`resize_h_apply_buffer <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/H_apply.irp.f#L57>`_
  Resizes the H_apply buffer of proc iproc. The buffer lock should
  be set before calling this function.


`s2_eig <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L270>`_
  Force the wave function to be an eigenfunction of S^2


`s2_values <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/s2.irp.f#L67>`_
  array of the averaged values of the S^2 operator on the various states


`s_z <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/s2.irp.f#L36>`_
  z component of the Spin


`s_z2_sz <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/s2.irp.f#L37>`_
  z component of the Spin


`save_natorb <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/save_natorb.irp.f#L1>`_
  Undocumented


`save_natural_mos <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L194>`_
  Save natural orbitals, obtained by diagonalization of the one-body density matrix in the MO basis


`save_wavefunction <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L646>`_
  Save the wave function into the EZFIO file


`save_wavefunction_general <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L665>`_
  Save the wave function into the EZFIO file


`save_wavefunction_specified <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L753>`_
  Save the wave function into the EZFIO file


`save_wavefunction_unsorted <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L656>`_
  Save the wave function into the EZFIO file


`set_bit_to_integer <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/create_excitations.irp.f#L38>`_
  Undocumented


`set_natural_mos <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L178>`_
  Set natural orbitals, obtained by diagonalization of the one-body density matrix in the MO basis


`single_exc_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants_bitmasks.irp.f#L17>`_
  single_exc_bitmask(:,1,i) is the bitmask for holes
  single_exc_bitmask(:,2,i) is the bitmask for particles
  for a given couple of hole/particle excitations i.


`sort_dets_ab <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L234>`_
  Uncodumented : TODO


`sort_dets_ab_v <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L164>`_
  Uncodumented : TODO


`sort_dets_ba_v <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L135>`_
  Uncodumented : TODO


`sort_dets_by_3_highest_electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L490>`_
  Determinants on which we apply <i|H|j>.
  They are sorted by the 3 highest electrons in the alpha part,
  then by the 3 highest electrons in the beta part to accelerate
  the research of connected determinants.


`sort_dets_by_det_search_key <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/determinants.irp.f#L347>`_
  Determinants are sorted are sorted according to their det_search_key.
  Useful to accelerate the search of a random determinant in the wave
  function.


`spin_det_search_key <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L9>`_
  Return an integer*8 corresponding to a determinant index for searching


`state_average_weight <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/density_matrix.irp.f#L205>`_
  Weights in the state-average calculation of the density matrix


`tamiser <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/davidson.irp.f#L91>`_
  Uncodumented : TODO


`target_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L160>`_
  Energy that should be obtained when truncating the wave function (optional)


`threshold_convergence_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/diagonalize_CI_SC2.irp.f#L18>`_
  convergence of the correlation energy of SC2 iterations


`threshold_davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L204>`_
  Thresholds of Davidson's algorithm


`threshold_generators <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L292>`_
  Thresholds on generators (fraction of the norm)


`threshold_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/ezfio_interface.irp.f#L6>`_
  Thresholds on selectors (fraction of the norm)


`write_spindeterminants <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants/spindeterminants.irp.f#L305>`_
  Undocumented

