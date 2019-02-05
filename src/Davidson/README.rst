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


`ci_eigenvectors_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L24>`_
  Eigenvectors/values of the CI matrix


`ci_electronic_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L22>`_
  Eigenvectors/values of the CI matrix


`ci_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L2>`_
  N_states lowest eigenvalues of the CI matrix


`davidson_collector <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L220>`_
  Undocumented


`davidson_converged <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/parameters.irp.f#L9>`_
  True if the Davidson algorithm is converged


`davidson_criterion <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/parameters.irp.f#L1>`_
  Can be : [  energy  | residual | both | wall_time | cpu_time | iterations ]


`davidson_diag_hjj_sjj <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization_hs2.irp.f#L52>`_
  Davidson diagonalization with specific diagonal elements of the H matrix
  .br
  H_jj : specific diagonal H matrix elements to diagonalize de Davidson
  .br
  S2_out : Output : s^2
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


`davidson_pull_results <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L177>`_
  Undocumented


`davidson_push_results <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L134>`_
  Undocumented


`davidson_run_slave <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L22>`_
  Undocumented


`davidson_slave <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_slave.irp.f#L1>`_
  Undocumented


`davidson_slave_inproc <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L5>`_
  Undocumented


`davidson_slave_tcp <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L13>`_
  Undocumented


`davidson_slave_work <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L58>`_
  Undocumented


`davidson_sze_max <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L6>`_
  Number of micro-iterations before re-contracting


`det_inf <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L1>`_
  Ordering function for determinants


`diagonalize_ci <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalize_CI.irp.f#L154>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix


`disk_based_davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L176>`_
  If true, disk space is used to store the vectors


`distributed_davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L74>`_
  If true, use the distributed algorithm


`find_reference <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/find_reference.irp.f#L1>`_
  Undocumented


`first_guess <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/guess_lowest_state.irp.f#L1>`_
  Select all the determinants with the lowest energy as a starting point.


`h_s2_u_0_nstates_openmp <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f#L15>`_
  Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  .br
  Assumes that the determinants are in psi_det
  .br
  istart, iend, ishift, istep are used in ZMQ parallelization.


`h_s2_u_0_nstates_openmp_work <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f#L69>`_
  Computes v_t = H|u_t> and s_t = S^2 |u_t>
  .br
  Default should be 1,N_det,0,1


`h_s2_u_0_nstates_openmp_work_1 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f_template_458#L3>`_
  Computes v_t = H|u_t> and s_t = S^2 |u_t>
  .br
  Default should be 1,N_det,0,1


`h_s2_u_0_nstates_openmp_work_2 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f_template_458#L357>`_
  Computes v_t = H|u_t> and s_t = S^2 |u_t>
  .br
  Default should be 1,N_det,0,1


`h_s2_u_0_nstates_openmp_work_3 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f_template_458#L711>`_
  Computes v_t = H|u_t> and s_t = S^2 |u_t>
  .br
  Default should be 1,N_det,0,1


`h_s2_u_0_nstates_openmp_work_4 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f_template_458#L1065>`_
  Computes v_t = H|u_t> and s_t = S^2 |u_t>
  .br
  Default should be 1,N_det,0,1


`h_s2_u_0_nstates_openmp_work_n_int <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f_template_458#L1419>`_
  Computes v_t = H|u_t> and s_t = S^2 |u_t>
  .br
  Default should be 1,N_det,0,1


`h_s2_u_0_nstates_zmq <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L260>`_
  Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>
  .br
  S2_jj : array of <j|S^2|j>


`n_states_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L142>`_
  Number of states to consider during the Davdison diagonalization


`nthreads_davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L396>`_
  Number of threads for Davdison


`print_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/print_energy.irp.f#L1>`_
  Undocumented


`print_h_matrix_restart <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/print_H_matrix_restart.irp.f#L1>`_
  Undocumented


`provide_everything <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_slave.irp.f#L29>`_
  Undocumented


`psi_energy <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/u0Hu0.irp.f#L1>`_
  Energy of the current wave function


`sort_dets_ab <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L168>`_
  Uncodumented : TODO


`sort_dets_ab_v <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L98>`_
  Uncodumented : TODO


`sort_dets_ba_v <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L69>`_
  Uncodumented : TODO


`state_following <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L40>`_
  If true, the states are re-ordered to match the input states


`tamiser <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization.irp.f#L26>`_
  Uncodumented : TODO


`threshold_davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/ezfio_interface.irp.f#L108>`_
  Thresholds of Davidson's algorithm


`u_0_h_u_0 <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/diagonalization_hs2.irp.f#L446>`_
  Computes e_0 = <u_0|H|u_0>/<u_0|u_0>
  .br
  n : number of determinants
  .br


`zmq_get_n_states_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L445>`_
  Get N_states_diag from the qp_run scheduler


`zmq_put_n_states_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson/davidson_parallel.irp.f#L411>`_
  Put N_states_diag on the qp_run scheduler

