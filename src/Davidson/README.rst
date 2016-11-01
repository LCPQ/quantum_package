Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Determinants <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`ci_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L23>`_
  Eigenvectors/values of the CI matrix


`ci_eigenvectors_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI_mono.irp.f#L2>`_
  Eigenvectors/values of the CI matrix


`ci_eigenvectors_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L24>`_
  Eigenvectors/values of the CI matrix


`ci_eigenvectors_s2_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI_mono.irp.f#L3>`_
  Eigenvectors/values of the CI matrix


`ci_electronic_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L22>`_
  Eigenvectors/values of the CI matrix


`ci_electronic_energy_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI_mono.irp.f#L1>`_
  Eigenvectors/values of the CI matrix


`ci_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L2>`_
  N_states lowest eigenvalues of the CI matrix


`dav_det <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L540>`_
  Temporary arrays for parallel davidson
  .br
  Touched in davidson_miniserver_get


`dav_size <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L554>`_
  Size of the arrays for Davidson
  .br
  Touched in davidson_miniserver_get


`dav_ut <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L541>`_
  Temporary arrays for parallel davidson
  .br
  Touched in davidson_miniserver_get


`davidson_add_task <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L175>`_
  Undocumented


`davidson_collect <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L118>`_
  Undocumented


`davidson_collector <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L365>`_
  Undocumented


`davidson_converged <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/parameters.irp.f#L27>`_
  True if the Davidson algorithm is converged


`davidson_criterion <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/parameters.irp.f#L19>`_
  Can be : [  energy  | residual | both | wall_time | cpu_time | iterations ]


`davidson_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L1>`_
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


`davidson_diag_hjj <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L273>`_
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
  N_st_diag : Number of states in which H is diagonalized
  .br
  iunit : Unit for the I/O
  .br
  Initial guess vectors are not necessarily orthonormal


`davidson_diag_hjj_sjj <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization_hs2.irp.f#L56>`_
  Davidson diagonalization with specific diagonal elements of the H matrix
  .br
  H_jj : specific diagonal H matrix elements to diagonalize de Davidson
  .br
  S2_jj : specific diagonal S^2 matrix elements
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
  N_st_diag : Number of states in which H is diagonalized. Assumed > sze
  .br
  iunit : Unit for the I/O
  .br
  Initial guess vectors are not necessarily orthonormal


`davidson_diag_hs2 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization_hs2.irp.f#L1>`_
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


`davidson_init <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L143>`_
  Undocumented


`davidson_iter_max <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/parameters.irp.f#L1>`_
  Max number of Davidson iterations


`davidson_miniserver_end <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L495>`_
  Undocumented


`davidson_miniserver_get <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L514>`_
  Undocumented


`davidson_miniserver_run <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L465>`_
  Undocumented


`davidson_process <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L7>`_
  Undocumented


`davidson_pull_results <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L327>`_
  Undocumented


`davidson_push_results <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L289>`_
  Undocumented


`davidson_run <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L420>`_
  Undocumented


`davidson_run_slave <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L207>`_
  Undocumented


`davidson_slave <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_slave.irp.f#L1>`_
  Undocumented


`davidson_slave_inproc <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L190>`_
  Undocumented


`davidson_slave_tcp <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L198>`_
  Undocumented


`davidson_slave_work <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L242>`_
  Undocumented


`davidson_sze_max <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/parameters.irp.f#L9>`_
  Max number of Davidson sizes


`det_inf <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L52>`_
  Ordering function for determinants


`diagonalize_ci <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L154>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix


`diagonalize_ci_mono <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI_mono.irp.f#L73>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix


`first_guess <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/guess_lowest_state.irp.f#L1>`_
  Select all the determinants with the lowest energy as a starting point.


`h_s2_u_0_nstates <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f#L180>`_
  Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>
  .br
  S2_jj : array of <j|S^2|j>


`h_u_0_nstates <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f#L31>`_
  Computes v_0 = H|u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>


`max_blocksize <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L569>`_
  Undocumented


`n_states_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L25>`_
  n_states_diag


`provide_everything <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_slave.irp.f#L36>`_
  Undocumented


`psi_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f#L171>`_
  Energy of the current wave function


`shortcut_ <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L565>`_
  Undocumented


`sort_dets_ab <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L219>`_
  Uncodumented : TODO


`sort_dets_ab_v <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L149>`_
  Uncodumented : TODO


`sort_dets_ba_v <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L120>`_
  Uncodumented : TODO


`sort_idx_ <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L568>`_
  Undocumented


`sorted_ <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L567>`_
  Undocumented


`tamiser <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L77>`_
  Uncodumented : TODO


`threshold_davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L6>`_
  Thresholds of Davidson's algorithm


`u_0_h_u_0 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f#L1>`_
  Computes e_0 = <u_0|H|u_0>/<u_0|u_0>
  .br
  n : number of determinants
  .br


`version_ <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L566>`_
  Undocumented

