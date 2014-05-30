===========
Dets Module
===========

This module contains the determinants of the CI wave function.

H is applied on the list of generator determinants. Selected determinants
are added into the *H_apply buffer*. Then the new wave function is
constructred as the concatenation of the odl wave function and
some determinants of the H_apply buffer. Generator determinants are built
as a subset of the determinants of the wave function.


Assumptions
===========

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* The MOs are orthonormal
* All the determinants have the same number of electrons
* The determinants are orthonormal
* The number of generator determinants <= the number of determinants
* All the determinants in the H_apply buffer are supposed to be different from the 
  wave function determinants
* All the determinants in the H_apply buffer are supposed to be unique


Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `BiInts <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Hartree_Fock <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock>`_
* `MonoInts <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`copy_h_apply_buffer_to_wf <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L113>`_
  Copies the H_apply buffer to psi_coef. You need to touch psi_det, psi_coef and N_det
  after calling this function.

`fill_h_apply_buffer_no_selection <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L199>`_
  Fill the H_apply buffer with determiants for CISD

`h_apply_buffer_allocated <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L14>`_
  Buffer of determinants/coefficients/perturbative energy for H_apply.
  Uninitialized. Filled by H_apply subroutines.

`h_apply_threshold <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L44>`_
  Theshold on | <Di|H|Dj> |

`resize_h_apply_buffer <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L63>`_
  Undocumented

`connected_to_ref <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/connected_to_ref.irp.f#L1>`_
  Undocumented

`det_is_not_or_may_be_in_ref <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/connected_to_ref.irp.f#L188>`_
  If true, det is not in ref
  If false, det may be in ref

`key_pattern_not_in_ref <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/connected_to_ref.irp.f#L222>`_
  Min and max values of the integers of the keys of the reference

`det_connections <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/connections.irp.f#L10>`_
  .br

`n_con_int <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/connections.irp.f#L2>`_
  Number of integers to represent the connections between determinants

`davidson_converged <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/davidson.irp.f#L383>`_
  True if the Davidson algorithm is converged

`davidson_criterion <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/davidson.irp.f#L373>`_
  Can be : [  energy  | residual | both | wall_time | cpu_time | iterations ]

`davidson_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/davidson.irp.f#L18>`_
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

`davidson_diag_hjj <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/davidson.irp.f#L68>`_
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

`davidson_iter_max <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/davidson.irp.f#L1>`_
  Max number of Davidson iterations

`davidson_sze_max <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/davidson.irp.f#L9>`_
  Max number of Davidson sizes

`davidson_threshold <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/davidson.irp.f#L374>`_
  Can be : [  energy  | residual | both | wall_time | cpu_time | iterations ]

`n_det <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L20>`_
  Number of determinants in the wave function

`n_det_reference <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L75>`_
  Number of determinants in the reference wave function

`n_states <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L3>`_
  Number of states to consider

`psi_average_norm_contrib <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L84>`_
  Contribution of determinants to the state-averaged density

`psi_average_norm_contrib_sorted <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L105>`_
  Wave function sorted by determinants (state-averaged)

`psi_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L47>`_
  The wave function. Initialized with Hartree-Fock if the EZFIO file
  is empty

`psi_coef_sorted <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L104>`_
  Wave function sorted by determinants (state-averaged)

`psi_det <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L46>`_
  The wave function. Initialized with Hartree-Fock if the EZFIO file
  is empty

`psi_det_size <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L38>`_
  Size of the psi_det/psi_coef arrays

`psi_det_sorted <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L103>`_
  Wave function sorted by determinants (state-averaged)

`double_exc_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L40>`_
  double_exc_bitmask(:,1,i) is the bitmask for holes of excitation 1
  double_exc_bitmask(:,2,i) is the bitmask for particles of excitation 1
  double_exc_bitmask(:,3,i) is the bitmask for holes of excitation 2
  double_exc_bitmask(:,4,i) is the bitmask for particles of excitation 2
  for a given couple of hole/particle excitations i.

`n_double_exc_bitmasks <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L31>`_
  Number of double excitation bitmasks

`n_single_exc_bitmasks <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L8>`_
  Number of single excitation bitmasks

`single_exc_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L17>`_
  single_exc_bitmask(:,1,i) is the bitmask for holes
  single_exc_bitmask(:,2,i) is the bitmask for particles
  for a given couple of hole/particle excitations i.

`ci_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/diagonalize_CI.irp.f#L36>`_
  Eigenvectors/values of the CI matrix

`ci_electronic_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/diagonalize_CI.irp.f#L35>`_
  Eigenvectors/values of the CI matrix

`ci_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/diagonalize_CI.irp.f#L18>`_
  N_states lowest eigenvalues of the CI matrix

`diag_algorithm <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/diagonalize_CI.irp.f#L1>`_
  Diagonalization algorithm (Davidson or Lapack)

`diagonalize_ci <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/diagonalize_CI.irp.f#L73>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix

`filter_connected <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/filter_connected.irp.f#L2>`_
  Filters out the determinants that are not connected by H
  .br
  returns the array idx which contains the index of the
  .br
  determinants in the array key1 that interact
  .br
  via the H operator with key2.
  .br
  idx(0) is the number of determinants that interact with key1

`filter_connected_davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/filter_connected.irp.f#L95>`_
  Filters out the determinants that are not connected by H
  .br
  returns the array idx which contains the index of the
  .br
  determinants in the array key1 that interact
  .br
  via the H operator with key2.
  .br
  idx(0) is the number of determinants that interact with key1

`filter_connected_i_h_psi0 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/filter_connected.irp.f#L211>`_
  returns the array idx which contains the index of the
  .br
  determinants in the array key1 that interact
  .br
  via the H operator with key2.
  .br
  idx(0) is the number of determinants that interact with key1

`filter_connected_i_h_psi0_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/filter_connected.irp.f#L310>`_
  Undocumented

`get_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/s2.irp.f#L1>`_
  Returns <S^2>

`get_s2_u0 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/s2.irp.f#L46>`_
  Undocumented

`s_z <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/s2.irp.f#L36>`_
  Undocumented

`s_z2_sz <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/s2.irp.f#L37>`_
  Undocumented

`a_operator <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L721>`_
  Needed for diag_H_mat_elem

`ac_operator <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L766>`_
  Needed for diag_H_mat_elem

`decode_exc <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L76>`_
  Decodes the exc arrays returned by get_excitation.
  h1,h2 : Holes
  p1,p2 : Particles
  s1,s2 : Spins (1:alpha, 2:beta)
  degree : Degree of excitation

`diag_h_mat_elem <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L659>`_
  Computes <i|H|i>

`get_double_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L141>`_
  Returns the two excitation operators between two doubly excited determinants and the phase

`get_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L30>`_
  Returns the excitation operators between two determinants and the phase

`get_excitation_degree <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L1>`_
  Returns the excitation degree between two determinants

`get_excitation_degree_vector <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L575>`_
  Applies get_excitation_degree to an array of determinants

`get_mono_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L274>`_
  Returns the excitation operator between two singly excited determinants and the phase

`get_occ_from_key <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L814>`_
  Returns a list of occupation numbers from a bitstring

`h_u_0 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L830>`_
  Computes v_0 = H|u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>

`i_h_j <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L355>`_
  Returns <i|H|j> where i and j are determinants

`i_h_psi <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L491>`_
  <key|H|psi> for the various Nstate

`i_h_psi_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L527>`_
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

`h_matrix_all_dets <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/utils.irp.f#L1>`_
  H matrix on the basis of the slater deter;inants defined by psi_det



