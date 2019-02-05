==========
MRPT_Utils
==========

Needed Modules
==============
.. Do not edit this section It was auto-generated
   .. by the `update_README.py` script.
      Documentation
=============
.. Do not edit this section It was auto-generated
   .. by the `update_README.py` script.

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Determinants <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants>`_
* `Davidson <http://github.com/LCPQ/quantum_package/tree/master/src/Davidson>`_
* `Psiref_CAS <http://github.com/LCPQ/quantum_package/tree/master/plugins/Psiref_CAS>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`a_coef <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L251>`_
  Undocumented


`add_poly <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L302>`_
  Add two polynomials
  D(t) =! D(t) +( B(t)+C(t))


`add_poly_multiply <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L330>`_
  Add a polynomial multiplied by a constant
  D(t) =! D(t) +( cst * B(t))


`apply_exc_to_psi <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L1>`_
  apply a contracted excitation to psi_in_out whose coefficients
  are psi_in_out_coef
  hole_particle =  1  ===> creation     of an electron in psi_in_out
  = -1  ===> annhilation  of an electron in psi_in_out
  orb ===> is the index of orbital where you want wether to create or
  annhilate an electron
  spin_exc ===> is the spin of the electron (1 == alpha) (2 == beta)
  the wave function gets out normalized to unity
  .br
  norm_out is the sum of the squared of the coefficients
  on which the excitation has been possible


`apply_rotation <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L367>`_
  Apply the rotation found by find_rotation


`approx_dble <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L340>`_
  Undocumented


`b_coef <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L256>`_
  Undocumented


`binom <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L31>`_
  Binomial coefficients


`binom_func <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L1>`_
  .. math                       ::
  .br
  \frac{i!}{j!(i-j)!}
  .br


`binom_transp <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L32>`_
  Binomial coefficients


`ci_dressed_pt2_new_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L225>`_
  Eigenvectors/values of the CI matrix


`ci_dressed_pt2_new_eigenvectors_s2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L226>`_
  Eigenvectors/values of the CI matrix


`ci_dressed_pt2_new_energy <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L349>`_
  N_states lowest eigenvalues of the CI matrix


`ci_electronic_dressed_pt2_new_energy <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L224>`_
  Eigenvectors/values of the CI matrix


`contrib_1h2p_dm_based <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/density_matrix_based.irp.f#L1>`_
  Undocumented


`contrib_2h1p_dm_based <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/density_matrix_based.irp.f#L64>`_
  Undocumented


`corr_e_from_1h1p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L817>`_
  Undocumented


`coulomb_value_no_check <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L425>`_
  Computes <i|H|i>


`dble_fact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L122>`_
  Undocumented


`dble_fact_even <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L139>`_
  n!!


`dble_fact_odd <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L183>`_
  n!!


`dble_logfact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L217>`_
  n!!


`ddfact2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L242>`_
  Undocumented


`degree_max_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/angular_integration.irp.f#L1>`_
  integrate correctly a polynom of order "degree_max_integration_lebedev"
  needed for the angular integration according to LEBEDEV formulae


`delta_ij_mrpt <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L2>`_
  Dressing matrix in N_det basis


`diag_h_mat_elem_no_elec_check <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L132>`_
  Computes <i|H|i>


`diag_h_mat_elem_no_elec_check_no_exchange <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L608>`_
  Computes <i|H|i>


`do_third_order_1h1p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/ezfio_interface.irp.f#L6>`_
  If true, compute the third order contribution for the 1h1p


`dset_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_323#L27>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`dset_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L90>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_270#L30>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`dtranspose <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/transpose.irp.f#L41>`_
  Transpose input matrix A into output matrix B


`energy_cas_dyall <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L1>`_
  Undocumented


`energy_cas_dyall_no_exchange <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L13>`_
  Undocumented


`erf0 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L104>`_
  Undocumented


`extrapolate_data <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/extrapolation.irp.f#L1>`_
  Extrapolate the data to the FCI limit


`f_integral <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L404>`_
  function that calculates the following integral
  \int_{\-infty}^{+\infty} x^n \exp(-p x^2) dx


`fact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L49>`_
  n!


`fact_inv <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L109>`_
  1/n!


`find_connections_previous <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_dress.irp.f#L137>`_
  Undocumented


`find_rotation <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L348>`_
  Find A.C = B


`fock_core_inactive <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L1>`_
  inactive part of the fock operator with contributions only from the inactive


`fock_core_inactive_from_act <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L45>`_
  inactive part of the fock operator with contributions only from the active


`fock_core_inactive_total <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L138>`_
  inactive part of the fock operator


`fock_core_inactive_total_spin_trace <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L139>`_
  inactive part of the fock operator


`fock_operator_active_from_core_inact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L180>`_
  active part of the fock operator with contributions only from the inactive


`fock_virt_from_act <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L91>`_
  virtual part of the fock operator with contributions only from the active


`fock_virt_from_core_inact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L22>`_
  fock operator for the virtuals that comes from the doubly occupied orbitals


`fock_virt_total <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L157>`_
  inactive part of the fock operator


`fock_virt_total_spin_trace <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/fock_like_operators.irp.f#L158>`_
  inactive part of the fock operator


`gammln <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L270>`_
  Undocumented


`gammp <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L132>`_
  Undocumented


`gaussian_product <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L181>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}


`gaussian_product_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L223>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}


`gcf <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L210>`_
  Undocumented


`gen_det_ref_idx <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_dress.irp.f#L129>`_
  Undocumented


`gen_det_ref_shortcut <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_dress.irp.f#L127>`_
  Undocumented


`gen_det_ref_sorted <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_dress.irp.f#L126>`_
  Undocumented


`gen_det_ref_version <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_dress.irp.f#L128>`_
  Undocumented


`get_delta_e_dyall <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/psi_active_prov.irp.f#L155>`_
  routine that returns the delta_e with the Moller Plesset and Dyall operators
  .br
  with det_1 being a determinant from the cas, and det_2 being a perturber
  .br
  Delta_e(det_1,det_2) = sum (hole) epsilon(hole) + sum(part) espilon(part) + delta_e(act)
  .br
  where hole is necessary in the inactive, part necessary in the virtuals
  .br
  and delta_e(act) is obtained from the contracted application of the excitation
  .br
  operator in the active space that lead from det_1 to det_2


`get_delta_e_dyall_general_mp <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/psi_active_prov.irp.f#L426>`_
  routine that returns the delta_e with the Moller Plesset and Dyall operators
  .br
  with det_1 being a determinant from the cas, and det_2 being a perturber
  .br
  Delta_e(det_1,det_2) = sum (hole) epsilon(hole) + sum(part) espilon(part) + delta_e(act)
  .br
  where hole is necessary in the inactive, part necessary in the virtuals
  .br
  and delta_e(act) is obtained as the sum of energies of excitations a la MP
  .br


`get_inverse <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L266>`_
  Returns the inverse of the square matrix A


`get_pseudo_inverse <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L294>`_
  Find C = A^-1


`give_1h1p_contrib <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way.irp.f#L396>`_
  Undocumented


`give_1h1p_only_doubles_spin_cross <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way.irp.f#L822>`_
  Undocumented


`give_1h1p_sec_order_singles_contrib <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way.irp.f#L516>`_
  Undocumented


`give_1h2p_contrib <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way.irp.f#L195>`_
  Undocumented


`give_1h2p_contrib_sec_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way_second_order_coef.irp.f#L358>`_
  Undocumented


`give_1h2p_new <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/second_order_new.irp.f#L2>`_
  Undocumented


`give_1p_sec_order_singles_contrib <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way.irp.f#L676>`_
  Undocumented


`give_2h1p_contrib <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way.irp.f#L1>`_
  Undocumented


`give_2h1p_contrib_sec_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/new_way_second_order_coef.irp.f#L1>`_
  Undocumented


`give_2h1p_new <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/second_order_new.irp.f#L482>`_
  Undocumented


`give_2h2p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/give_2h2p.irp.f#L1>`_
  Undocumented


`give_2p_new <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/second_order_new_2p.irp.f#L2>`_
  Undocumented


`give_active_part_determinant <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/utils_bitmask.irp.f#L2>`_
  Undocumented


`give_core_inactive_part_determinant <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/utils_bitmask.irp.f#L14>`_
  Undocumented


`give_explicit_poly_and_gaussian <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L46>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )


`give_explicit_poly_and_gaussian_double <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L119>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3)
  exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta) exp(-(r-Nucl_center)^2 gama
  .br
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )


`give_explicit_poly_and_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L1>`_
  Transform the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k  (x-x_P)^iorder(1)  (y-y_P)^iorder(2)  (z-z_P)^iorder(3) exp(-p(r-P)^2)


`give_holes_and_particles_in_active_space <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/psi_active_prov.irp.f#L21>`_
  returns the holes and particles operators WITHIN THE ACTIVE SPACE
  that connect det_1 and det_2. By definition, the holes/particles
  are such that one starts from det_1 and goes to det_2
  .br
  n_holes is the total number of holes
  n_particles is the total number of particles
  n_holes_spin is the number of number of holes per spin (1=alpha, 2=beta)
  n_particles_spin is the number of number of particles per spin (1=alpha, 2=beta)
  holes_active_list is the index of the holes per spin, that ranges from 1 to n_act_orb
  particles_active_list is the index of the particles per spin, that ranges from 1 to n_act_orb


`give_holes_in_inactive_space <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/psi_active_prov.irp.f#L89>`_
  returns the holes operators WITHIN THE INACTIVE SPACE
  that has lead to det_1.
  .br
  n_holes is the total number of holes
  n_holes_spin is the number of number of holes per spin (1=alpha, 2=beta)
  holes_inactive_list is the index of the holes per spin, that ranges from 1 to mo_tot_num


`give_particles_in_virt_space <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/psi_active_prov.irp.f#L121>`_
  returns the holes operators WITHIN THE VIRTUAL SPACE
  that has lead to det_1.
  .br
  n_particles is the total number of particles
  n_particles_spin is the number of number of particles per spin (1=alpha, 2=beta)
  particles_inactive_list is the index of the particles per spin, that ranges from 1 to mo_tot_num


`give_singles_and_partial_doubles_1h1p_contrib <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L963>`_
  Undocumented


`give_virt_part_determinant <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/utils_bitmask.irp.f#L26>`_
  Undocumented


`gser <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L166>`_
  Undocumented


h_apply_mrpt
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_1h
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_1h1p
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_1h1p_diexc
  Undocumented


h_apply_mrpt_1h1p_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_1h1p_diexcp
  Undocumented


h_apply_mrpt_1h1p_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_1h2p
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_1h2p_diexc
  Undocumented


h_apply_mrpt_1h2p_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_1h2p_diexcp
  Undocumented


h_apply_mrpt_1h2p_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_1h_diexc
  Undocumented


h_apply_mrpt_1h_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_1h_diexcp
  Undocumented


h_apply_mrpt_1h_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_1p
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_1p_diexc
  Undocumented


h_apply_mrpt_1p_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_1p_diexcp
  Undocumented


h_apply_mrpt_1p_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2h
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_2h1p
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_2h1p_diexc
  Undocumented


h_apply_mrpt_2h1p_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2h1p_diexcp
  Undocumented


h_apply_mrpt_2h1p_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2h2p
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_2h2p_diexc
  Undocumented


h_apply_mrpt_2h2p_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2h2p_diexcp
  Undocumented


h_apply_mrpt_2h2p_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2h_diexc
  Undocumented


h_apply_mrpt_2h_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2h_diexcp
  Undocumented


h_apply_mrpt_2h_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2p
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrpt_2p_diexc
  Undocumented


h_apply_mrpt_2p_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_2p_diexcp
  Undocumented


h_apply_mrpt_2p_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_diexc
  Undocumented


h_apply_mrpt_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrpt_diexcp
  Undocumented


h_apply_mrpt_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


`heap_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L312>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_dsort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L375>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L1008>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_i2sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L1071>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L776>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_i8sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L839>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L544>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_isort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L607>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L80>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L143>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`hermite <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L536>`_
  Hermite polynomial


`hmatrix_dressed_pt2_new <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L196>`_
  Undocumented


`hmatrix_dressed_pt2_new_symmetrized <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L210>`_
  Undocumented


`i2radix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_605#L423>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i2set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_323#L102>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`i2set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L261>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_291#L34>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`i8radix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_605#L213>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i8radix_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_605#L843>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i8set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_323#L77>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`i8set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L204>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_291#L18>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`i_h_j_dyall <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L261>`_
  Returns <i|H|j> where i and j are determinants


`i_h_j_dyall_no_exchange <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L485>`_
  Returns <i|H|j> where i and j are determinants


`insertion_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L234>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_dsort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L59>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L930>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_i2sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L230>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L698>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_i8sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L173>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L466>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_isort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L116>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L2>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L2>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`inv_int <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L248>`_
  1/i


`iradix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_605#L3>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`iradix_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_605#L633>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`iset_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_323#L52>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`iset_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L147>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_291#L2>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`lapack_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L446>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_diag_s2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L514>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_diagd <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L379>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_partial_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L580>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`logfact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L77>`_
  n!


`lowercase <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L364>`_
  Transform to lower case


`map_load_from_disk <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/map_functions.irp.f#L66>`_
  Undocumented


`map_save_to_disk <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/map_functions.irp.f#L1>`_
  Undocumented


`matrix_vector_product <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L661>`_
  performs u1 =! performs u1 +( u0 * matrix)


`mrpt_dress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_dress.irp.f#L17>`_
  Undocumented


`multiply_poly <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L261>`_
  Multiply two polynomials
  D(t) =! D(t) +( B(t)*C(t))


`n_points_integration_angular_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/angular_integration.irp.f#L11>`_
  Number of points needed for the angular integral


`normalize <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L318>`_
  Normalizes vector u


`nproc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L274>`_
  Number of current OpenMP threads


`one_anhil <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L66>`_
  Undocumented


`one_anhil_inact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L614>`_
  Undocumented


`one_anhil_one_creat <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L205>`_
  Undocumented


`one_anhil_one_creat_inact_virt <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L507>`_
  Undocumented


`one_anhil_one_creat_inact_virt_bis <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L816>`_
  Undocumented


`one_anhil_one_creat_inact_virt_norm <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L508>`_
  Undocumented


`one_creat <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L26>`_
  Undocumented


`one_creat_virt <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L711>`_
  Undocumented


`ortho_canonical <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L45>`_
  Compute C_new=C_old.U.s^-1/2 canonical orthogonalization.
  .br
  overlap : overlap matrix
  .br
  LDA : leftmost dimension of overlap array
  .br
  N : Overlap matrix is NxN (array is (LDA,N) )
  .br
  C : Coefficients of the vectors to orthogonalize. On exit,
  orthogonal vectors
  .br
  LDC : leftmost dimension of C
  .br
  m : Coefficients matrix is MxN, ( array is (LDC,N) )
  .br


`ortho_lowdin <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L182>`_
  Compute C_new=C_old.S^-1/2 orthogonalization.
  .br
  overlap : overlap matrix
  .br
  LDA : leftmost dimension of overlap array
  .br
  N : Overlap matrix is NxN (array is (LDA,N) )
  .br
  C : Coefficients of the vectors to orthogonalize. On exit,
  orthogonal vectors
  .br
  LDC : leftmost dimension of C
  .br
  M : Coefficients matrix is MxN, ( array is (LDC,N) )
  .br


`ortho_qr <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L123>`_
  Orthogonalization using Q.R factorization
  .br
  A : matrix to orthogonalize
  .br
  LDA : leftmost dimension of A
  .br
  n : Number of rows of A
  .br
  m : Number of columns of A
  .br


`ortho_qr_unblocked <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L155>`_
  Orthogonalization using Q.R factorization
  .br
  A : matrix to orthogonalize
  .br
  LDA : leftmost dimension of A
  .br
  n : Number of rows of A
  .br
  m : Number of columns of A
  .br


`overlap_a_b_c <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/one_e_integration.irp.f#L35>`_
  Undocumented


`overlap_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/one_e_integration.irp.f#L1>`_
  .. math::
  .br
  \sum_{-infty}^{+infty} (x-A_x)^ax (x-B_x)^bx exp(-alpha(x-A_x)^2) exp(-beta(x-B_X)^2) dx
  .br


`overlap_gaussian_xyz <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/one_e_integration.irp.f#L113>`_
  .. math::
  .br
  S_x = \int (x-A_x)^{a_x} exp(-\alpha(x-A_x)^2)  (x-B_x)^{b_x} exp(-beta(x-B_x)^2) dx \\
  S = S_x S_y S_z
  .br


`overlap_x_abs <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/one_e_integration.irp.f#L175>`_
  .. math                      ::
  .br
  \int_{-infty}^{+infty} (x-A_center)^(power_A) * (x-B_center)^power_B * exp(-alpha(x-A_center)^2) * exp(-beta(x-B_center)^2) dx
  .br


`phi_angular_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/angular_integration.irp.f#L41>`_
  Theta phi values together with the weights values for the angular integration :
  integral [dphi,dtheta] f(x,y,z) = 4 * pi * sum (1<i<n_points_integration_angular_lebedev) f(xi,yi,zi)
  Note that theta and phi are in DEGREES !!


`progress_active <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L29>`_
  Current status for displaying progress bars. Global variable.


`progress_bar <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L27>`_
  Current status for displaying progress bars. Global variable.


`progress_timeout <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L28>`_
  Current status for displaying progress bars. Global variable.


`progress_title <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L31>`_
  Current status for displaying progress bars. Global variable.


`progress_value <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L30>`_
  Current status for displaying progress bars. Global variable.


`psi_active <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/psi_active_prov.irp.f#L3>`_
  active part of psi


`psi_ref_bis_lock <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_dress.irp.f#L4>`_
  Locks on ref determinants to fill delta_ij


`quick_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L262>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L958>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L726>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L494>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L30>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`rec__quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L43>`_
  Undocumented


`rec_d_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L275>`_
  Undocumented


`rec_i2_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L971>`_
  Undocumented


`rec_i8_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L739>`_
  Undocumented


`rec_i_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L507>`_
  Undocumented


`recentered_poly2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L359>`_
  Recenter two polynomials


`rint <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L432>`_
  .. math::
  .br
  \int_0^1 dx \exp(-p x^2) x^n
  .br


`rint1 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L592>`_
  Standard version of rint


`rint_large_n <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L561>`_
  Version of rint for large values of n


`rint_sum <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/integration.irp.f#L480>`_
  Needed for the calculation of two-electron integrals.


`rinteg <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L46>`_
  Undocumented


`rintgauss <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L30>`_
  Undocumented


`run_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L45>`_
  Display a progress bar with documentation of what is happening


`sabpartial <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/need.irp.f#L2>`_
  Undocumented


`second_order_pt_new <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L3>`_
  Dressing matrix in N_det basis


`second_order_pt_new_1h <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L4>`_
  Dressing matrix in N_det basis


`second_order_pt_new_1h1p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L6>`_
  Dressing matrix in N_det basis


`second_order_pt_new_1h2p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L9>`_
  Dressing matrix in N_det basis


`second_order_pt_new_1p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L5>`_
  Dressing matrix in N_det basis


`second_order_pt_new_2h <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L7>`_
  Dressing matrix in N_det basis


`second_order_pt_new_2h1p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L10>`_
  Dressing matrix in N_det basis


`second_order_pt_new_2h2p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L11>`_
  Dressing matrix in N_det basis


`second_order_pt_new_2p <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/mrpt_utils.irp.f#L8>`_
  Dressing matrix in N_det basis


`set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_323#L2>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_388#L33>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`set_zero_extra_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L637>`_
  Undocumented


`sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_270#L2>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`sorted_dnumber <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L441>`_
  Returns the number of sorted elements


`sorted_i2number <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L1137>`_
  Returns the number of sorted elements


`sorted_i8number <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L905>`_
  Returns the number of sorted elements


`sorted_inumber <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L673>`_
  Returns the number of sorted elements


`sorted_number <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/sort.irp.f_template_238#L209>`_
  Returns the number of sorted elements


`start_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L1>`_
  Starts the progress bar


`stop_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/progress.irp.f#L19>`_
  Stop the progress bar


`svd <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/LinearAlgebra.irp.f#L1>`_
  Compute A = U.D.Vt
  .br
  LDx : leftmost dimension of x
  .br
  Dimsneion of A is m x n
  .br


`theta_angular_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/angular_integration.irp.f#L40>`_
  Theta phi values together with the weights values for the angular integration :
  integral [dphi,dtheta] f(x,y,z) = 4 * pi * sum (1<i<n_points_integration_angular_lebedev) f(xi,yi,zi)
  Note that theta and phi are in DEGREES !!


`three_anhil <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L444>`_
  Undocumented


`three_creat <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L383>`_
  Undocumented


`transpose <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/transpose.irp.f#L2>`_
  Transpose input matrix A into output matrix B


`two_anhil <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L156>`_
  Undocumented


`two_anhil_one_creat <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L260>`_
  Undocumented


`two_creat <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L106>`_
  Undocumented


`two_creat_one_anhil <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/energies_cas.irp.f#L322>`_
  Undocumented


`u0_h_dyall_u0 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L392>`_
  Undocumented


`u0_h_dyall_u0_no_exchange <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/excitations_cas.irp.f#L678>`_
  Undocumented


`u_dot_u <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L304>`_
  Compute <u|u>


`u_dot_v <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L290>`_
  Compute <u|v>


`wall_time <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L259>`_
  The equivalent of cpu_time, but for the wall time.


`weights_angular_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/angular_integration.irp.f#L42>`_
  Theta phi values together with the weights values for the angular integration :
  integral [dphi,dtheta] f(x,y,z) = 4 * pi * sum (1<i<n_points_integration_angular_lebedev) f(xi,yi,zi)
  Note that theta and phi are in DEGREES !!


`write_git_log <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRPT_Utils/util.irp.f#L234>`_
  Write the last git commit in file iunit.

