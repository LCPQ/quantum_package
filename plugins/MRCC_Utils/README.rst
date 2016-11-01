Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Perturbation <http://github.com/LCPQ/quantum_package/tree/master/plugins/Perturbation>`_
* `Selectors_full <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_full>`_
* `Generators_full <http://github.com/LCPQ/quantum_package/tree/master/plugins/Generators_full>`_
* `Psiref_Utils <http://github.com/LCPQ/quantum_package/tree/master/plugins/Psiref_Utils>`_
* `Psiref_CAS <http://github.com/LCPQ/quantum_package/tree/master/plugins/Psiref_CAS>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`a_coef <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L252>`_
  Undocumented


`add_poly <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L306>`_
  Add two polynomials
  D(t) =! D(t) +( B(t)+C(t))


`add_poly_multiply <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L334>`_
  Add a polynomial multiplied by a constant
  D(t) =! D(t) +( cst * B(t))


`align_double <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L48>`_
  Compute 1st dimension such that it is aligned for vectorization.


`apply_hole_local <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1282>`_
  Undocumented


`apply_particle_local <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1319>`_
  Undocumented


`apply_rotation <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L320>`_
  Apply the rotation found by find_rotation


`approx_dble <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L371>`_
  Undocumented


`b_coef <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L257>`_
  Undocumented


`binom <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L31>`_
  Binomial coefficients


`binom_func <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L1>`_
  .. math                       ::
  .br
  \frac{i!}{j!(i-j)!}
  .br


`binom_transp <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L32>`_
  Binomial coefficients


`ci_eigenvectors_dressed <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L120>`_
  Eigenvectors/values of the dressed CI matrix


`ci_eigenvectors_s2_dressed <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L121>`_
  Eigenvectors/values of the dressed CI matrix


`ci_electronic_energy_dressed <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L119>`_
  Eigenvectors/values of the dressed CI matrix


`ci_energy_dressed <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L247>`_
  N_states lowest eigenvalues of the dressed CI matrix


`davidson_diag_hjj_mrcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/davidson.irp.f#L57>`_
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


`davidson_diag_hjj_sjj_mrcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/davidson.irp.f#L610>`_
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


`davidson_diag_mrcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/davidson.irp.f#L1>`_
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


`davidson_diag_mrcc_hs2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/davidson.irp.f#L552>`_
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


`dble_fact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L136>`_
  Undocumented


`dble_fact_even <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L153>`_
  n!!


`dble_fact_odd <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L197>`_
  n!!


`dble_logfact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L231>`_
  n!!


`ddfact2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L243>`_
  Undocumented


`dec_exc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L532>`_
  Undocumented


`diagonalize_ci_dressed <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L265>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix


`dij <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1092>`_
  Undocumented


`dij_unique <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L617>`_
  Undocumented


`dset_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_216#L27>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`dset_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L94>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L339>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`dtranspose <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/transpose.irp.f#L41>`_
  Transpose input matrix A into output matrix B


`erf0 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L105>`_
  Undocumented


`exc_inf <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L479>`_
  Undocumented


`exccmp <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1265>`_
  Undocumented


`exceq <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1253>`_
  Undocumented


`f_integral <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L408>`_
  function that calculates the following integral
  \int_{\-infty}^{+\infty} x^n \exp(-p x^2) dx


`fact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L63>`_
  n!


`fact_inv <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L123>`_
  1/n!


`find_rotation <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L301>`_
  Find A.C = B


`find_triples_and_quadruples <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_dress.irp.f#L286>`_
  Undocumented


`find_triples_and_quadruples_micro <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_dress.irp.f#L346>`_
  Undocumented


`gammln <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L271>`_
  Undocumented


`gammp <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L133>`_
  Undocumented


`gaussian_product <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L184>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}


`gaussian_product_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L226>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}


`gcf <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L211>`_
  Undocumented


`get_dij <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1129>`_
  Undocumented


`get_dij_index <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1113>`_
  Undocumented


`get_pseudo_inverse <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L247>`_
  Find C = A^-1


`give_explicit_poly_and_gaussian <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L46>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )


`give_explicit_poly_and_gaussian_double <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L122>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3)
  exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta) exp(-(r-Nucl_center)^2 gama
  .br
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )


`give_explicit_poly_and_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L1>`_
  Transform the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k  (x-x_P)^iorder(1)  (y-y_P)^iorder(2)  (z-z_P)^iorder(3) exp(-p(r-P)^2)


`gser <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L167>`_
  Undocumented


h_apply_mrcc
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrcc_diexc
  Undocumented


h_apply_mrcc_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrcc_diexcp
  Undocumented


h_apply_mrcc_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrcc_pt2
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrcc_pt2_diexc
  Undocumented


h_apply_mrcc_pt2_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrcc_pt2_diexcp
  Undocumented


h_apply_mrcc_pt2_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrcepa_pt2
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrcepa_pt2_collector
  Collects results from the selection in an array of generators


h_apply_mrcepa_pt2_diexc
  Undocumented


h_apply_mrcepa_pt2_diexcorg
  Generate all double excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrcepa_pt2_diexcp
  Undocumented


h_apply_mrcepa_pt2_monoexc
  Generate all single excitations of key_in using the bit masks of holes and
  particles.
  Assume N_int is already provided.


h_apply_mrcepa_pt2_slave
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry). Auto-generated by the ``generate_h_apply`` script.


h_apply_mrcepa_pt2_slave_inproc
  Computes a buffer using threads


h_apply_mrcepa_pt2_slave_tcp
  Computes a buffer over the network


`h_matrix_dressed <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L94>`_
  Dressed H with Delta_ij


`h_s2_u_0_mrcc_nstates <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/davidson.irp.f#L997>`_
  Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>
  .br
  S2_jj : array of <j|S^2|j>


`h_u_0_mrcc_nstates <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/davidson.irp.f#L409>`_
  Computes v_0 = H|u_0>
  .br
  n : number of determinants
  .br
  H_jj : array of <j|H|j>


`heap_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L210>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_dsort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L273>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L744>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_i2sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L807>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L566>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_i8sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L629>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L388>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_isort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L451>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L32>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L95>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`hermite <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L540>`_
  Hermite polynomial


`hh_exists <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1181>`_
  Undocumented


`hh_shortcut <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1182>`_
  Undocumented


`hij_mrcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L66>`_
  < ref | H | Non-ref > matrix


`i2radix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_452#L327>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i2set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_216#L102>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`i2set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L271>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L873>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`i8radix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_452#L165>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i8radix_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_452#L651>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i8set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_216#L77>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`i8set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L212>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L695>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L180>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_dsort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L61>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L714>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_i2sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L238>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L536>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_i8sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L179>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L358>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_isort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L120>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L2>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L2>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`inv_int <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L278>`_
  1/i


`iradix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_452#L3>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`iradix_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_452#L489>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`is_generable <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L284>`_
  Undocumented


`iset_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_216#L52>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`iset_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L153>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L517>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`lambda_mrcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L8>`_
  cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)


`lambda_mrcc_kept <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L10>`_
  cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)


`lambda_mrcc_pt2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L9>`_
  cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)


`lapack_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L399>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_diag_s2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L462>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_diagd <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L332>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_partial_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L528>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`logfact <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L91>`_
  n!


`lowercase <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L395>`_
  Transform to lower case


`map_load_from_disk <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/map_functions.irp.f#L70>`_
  Undocumented


`map_save_to_disk <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/map_functions.irp.f#L1>`_
  Undocumented


`mrcc_dress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_dress.irp.f#L17>`_
  Undocumented


`mrmode <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L3>`_
  Undocumented


`multiply_poly <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L264>`_
  Multiply two polynomials
  D(t) =! D(t) +( B(t)*C(t))


`n_ex_exists <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L575>`_
  Undocumented


`n_hh_exists <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L573>`_
  Undocumented


`n_pp_exists <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L574>`_
  Undocumented


`normalize <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L348>`_
  Normalizes vector u
  u is expected to be aligned in memory.


`nproc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L304>`_
  Number of current OpenMP threads


`ortho_canonical <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L45>`_
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


`ortho_lowdin <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L162>`_
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
  m : Coefficients matrix is MxN, ( array is (LDC,N) )
  .br


`ortho_qr <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L128>`_
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


`overlap_a_b_c <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/one_e_integration.irp.f#L35>`_
  Undocumented


`overlap_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/one_e_integration.irp.f#L1>`_
  .. math::
  .br
  \sum_{-infty}^{+infty} (x-A_x)^ax (x-B_x)^bx exp(-alpha(x-A_x)^2) exp(-beta(x-B_X)^2) dx
  .br


`overlap_gaussian_xyz <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/one_e_integration.irp.f#L113>`_
  .. math::
  .br
  S_x = \int (x-A_x)^{a_x} exp(-\alpha(x-A_x)^2)  (x-B_x)^{b_x} exp(-beta(x-B_x)^2) dx \\
  S = S_x S_y S_z
  .br


`overlap_x_abs <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/one_e_integration.irp.f#L175>`_
  .. math                      ::
  .br
  \int_{-infty}^{+infty} (x-A_center)^(power_A) * (x-B_center)^power_B * exp(-alpha(x-A_center)^2) * exp(-beta(x-B_center)^2) dx
  .br


`pouet <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_dummy.irp.f#L1>`_
  Undocumented


`pp_exists <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L1183>`_
  Undocumented


`progress_active <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L29>`_
  Current status for displaying progress bars. Global variable.


`progress_bar <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L27>`_
  Current status for displaying progress bars. Global variable.


`progress_timeout <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L28>`_
  Current status for displaying progress bars. Global variable.


`progress_title <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L31>`_
  Current status for displaying progress bars. Global variable.


`progress_value <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L30>`_
  Current status for displaying progress bars. Global variable.


`psi_non_ref_sorted <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L609>`_
  Undocumented


`psi_non_ref_sorted_idx <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L610>`_
  Undocumented


`psi_ref_lock <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_dress.irp.f#L4>`_
  Locks on ref determinants to fill delta_ij


`recentered_poly2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L363>`_
  Recenter two polynomials


`rho_mrcc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L618>`_
  Undocumented


`rint <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L436>`_
  .. math::
  .br
  \int_0^1 dx \exp(-p x^2) x^n
  .br


`rint1 <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L596>`_
  Standard version of rint


`rint_large_n <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L565>`_
  Version of rint for large values of n


`rint_sum <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/integration.irp.f#L484>`_
  Needed for the calculation of two-electron integrals.


`rinteg <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L47>`_
  Undocumented


`rintgauss <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L31>`_
  Undocumented


`run_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L45>`_
  Display a progress bar with documentation of what is happening


`sabpartial <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/need.irp.f#L2>`_
  Undocumented


`searchdet <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L337>`_
  Undocumented


`searchexc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L388>`_
  Undocumented


`set_generators_bitmasks_as_holes_and_particles <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_general.irp.f#L2>`_
  Undocumented


`set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_216#L2>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_283#L35>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`set_zero_extra_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L585>`_
  Undocumented


`sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/sort.irp.f_template_184#L161>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`sort_det <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L417>`_
  Undocumented


`sort_exc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L453>`_
  Undocumented


`start_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L1>`_
  Starts the progress bar


`stop_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/progress.irp.f#L19>`_
  Stop the progress bar


`svd <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/LinearAlgebra.irp.f#L1>`_
  Compute A = U.D.Vt
  .br
  LDx : leftmost dimension of x
  .br
  Dimsneion of A is m x n
  .br


`tamise_exc <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L495>`_
  Uncodumented : TODO


`transpose <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/transpose.irp.f#L2>`_
  Transpose input matrix A into output matrix B


`u_0_h_u_0_mrcc_nstates <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/davidson.irp.f#L374>`_
  Computes e_0 = <u_0|H|u_0>/<u_0|u_0>
  .br
  n : number of determinants
  .br


`u_dot_u <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L334>`_
  Compute <u|u>


`u_dot_v <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L320>`_
  Compute <u|v>


`unsortedsearchdet <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/mrcc_utils.irp.f#L368>`_
  Undocumented


`wall_time <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L289>`_
  The equivalent of cpu_time, but for the wall time.


`write_git_log <http://github.com/LCPQ/quantum_package/tree/master/plugins/MRCC_Utils/util.irp.f#L264>`_
  Write the last git commit in file iunit.

