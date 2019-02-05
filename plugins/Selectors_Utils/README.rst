=====================
Selectors_full Module
=====================

Needed Modules
==============

.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `Determinants <http://github.com/LCPQ/quantum_package/tree/master/src/Determinants>`_
* `Hartree_Fock <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock>`_

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


`a_coef <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L251>`_
  Undocumented


`add_poly <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L302>`_
  Add two polynomials
  D(t) =! D(t) +( B(t)+C(t))


`add_poly_multiply <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L330>`_
  Add a polynomial multiplied by a constant
  D(t) =! D(t) +( cst * B(t))


`apply_rotation <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L367>`_
  Apply the rotation found by find_rotation


`approx_dble <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L340>`_
  Undocumented


`b_coef <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L256>`_
  Undocumented


`binom <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L31>`_
  Binomial coefficients


`binom_func <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L1>`_
  .. math                       ::
  .br
  \frac{i!}{j!(i-j)!}
  .br


`binom_transp <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L32>`_
  Binomial coefficients


`coef_hf_selector <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L27>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`dble_fact <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L122>`_
  Undocumented


`dble_fact_even <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L139>`_
  n!!


`dble_fact_odd <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L183>`_
  n!!


`dble_logfact <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L217>`_
  n!!


`ddfact2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L242>`_
  Undocumented


`degree_max_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/angular_integration.irp.f#L1>`_
  integrate correctly a polynom of order "degree_max_integration_lebedev"
  needed for the angular integration according to LEBEDEV formulae


`delta_e_per_selector <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L32>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`double_index_selectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L3>`_
  degree of excitation respect to Hartree Fock for the wave function
  .br
  for the all the selectors determinants
  .br
  double_index_selectors = list of the index of the double excitations
  .br
  n_double_selectors = number of double excitations in the selectors determinants


`dset_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_323#L27>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`dset_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L90>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_270#L30>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`dtranspose <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/transpose.irp.f#L41>`_
  Transpose input matrix A into output matrix B


`e_corr_double_only <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L33>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`e_corr_per_selectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L30>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`e_corr_second_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L34>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`erf0 <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L104>`_
  Undocumented


`exc_degree_per_selectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L2>`_
  degree of excitation respect to Hartree Fock for the wave function
  .br
  for the all the selectors determinants
  .br
  double_index_selectors = list of the index of the double excitations
  .br
  n_double_selectors = number of double excitations in the selectors determinants


`extrapolate_data <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/extrapolation.irp.f#L1>`_
  Extrapolate the data to the FCI limit


`f_integral <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L404>`_
  function that calculates the following integral
  \int_{\-infty}^{+\infty} x^n \exp(-p x^2) dx


`fact <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L49>`_
  n!


`fact_inv <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L109>`_
  1/n!


`find_rotation <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L348>`_
  Find A.C = B


`gammln <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L270>`_
  Undocumented


`gammp <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L132>`_
  Undocumented


`gaussian_product <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L181>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}


`gaussian_product_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L223>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}


`gcf <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L210>`_
  Undocumented


`get_inverse <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L266>`_
  Returns the inverse of the square matrix A


`get_pseudo_inverse <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L294>`_
  Find C = A^-1


`give_explicit_poly_and_gaussian <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L46>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )


`give_explicit_poly_and_gaussian_double <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L119>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3)
  exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta) exp(-(r-Nucl_center)^2 gama
  .br
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )


`give_explicit_poly_and_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L1>`_
  Transform the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k  (x-x_P)^iorder(1)  (y-y_P)^iorder(2)  (z-z_P)^iorder(3) exp(-p(r-P)^2)


`gser <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L166>`_
  Undocumented


`heap_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L312>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_dsort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L375>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L1008>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_i2sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L1071>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L776>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_i8sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L839>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L544>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_isort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L607>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`heap_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L80>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`heap_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L143>`_
  Sort array x(isize) using the heap sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`hermite <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L536>`_
  Hermite polynomial


`i2radix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_605#L423>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i2set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_323#L102>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`i2set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L261>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_291#L34>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`i8radix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_605#L213>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i8radix_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_605#L843>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`i8set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_323#L77>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`i8set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L204>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_291#L18>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`i_h_hf_per_selectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L31>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`insertion_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L234>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_dsort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L59>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L930>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_i2sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L230>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L698>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_i8sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L173>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L466>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_isort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L116>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`insertion_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L2>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`insertion_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L2>`_
  Sort array x(isize) using the insertion sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`inv_int <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L248>`_
  1/i


`inv_selectors_coef_hf <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L28>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`inv_selectors_coef_hf_squared <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L29>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants


`iradix_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_605#L3>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`iradix_sort_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_605#L633>`_
  Sort integer array x(isize) using the radix sort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.
  iradix should be -1 in input.


`iset_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_323#L52>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`iset_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L147>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_291#L2>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`lapack_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L446>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_diag_s2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L514>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_diagd <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L379>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`lapack_partial_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L580>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br


`logfact <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L77>`_
  n!


`lowercase <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L364>`_
  Transform to lower case


`map_load_from_disk <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/map_functions.irp.f#L66>`_
  Undocumented


`map_save_to_disk <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/map_functions.irp.f#L1>`_
  Undocumented


`matrix_vector_product <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L661>`_
  performs u1 =! performs u1 +( u0 * matrix)


`multiply_poly <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L261>`_
  Multiply two polynomials
  D(t) =! D(t) +( B(t)*C(t))


`n_double_selectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/e_corr_selectors.irp.f#L4>`_
  degree of excitation respect to Hartree Fock for the wave function
  .br
  for the all the selectors determinants
  .br
  double_index_selectors = list of the index of the double excitations
  .br
  n_double_selectors = number of double excitations in the selectors determinants


`n_points_integration_angular_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/angular_integration.irp.f#L11>`_
  Number of points needed for the angular integral


`normalize <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L318>`_
  Normalizes vector u


`nproc <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L274>`_
  Number of current OpenMP threads


`ortho_canonical <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L45>`_
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


`ortho_lowdin <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L182>`_
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


`ortho_qr <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L123>`_
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


`ortho_qr_unblocked <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L155>`_
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


`overlap_a_b_c <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/one_e_integration.irp.f#L35>`_
  Undocumented


`overlap_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/one_e_integration.irp.f#L1>`_
  .. math::
  .br
  \sum_{-infty}^{+infty} (x-A_x)^ax (x-B_x)^bx exp(-alpha(x-A_x)^2) exp(-beta(x-B_X)^2) dx
  .br


`overlap_gaussian_xyz <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/one_e_integration.irp.f#L113>`_
  .. math::
  .br
  S_x = \int (x-A_x)^{a_x} exp(-\alpha(x-A_x)^2)  (x-B_x)^{b_x} exp(-beta(x-B_x)^2) dx \\
  S = S_x S_y S_z
  .br


`overlap_x_abs <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/one_e_integration.irp.f#L175>`_
  .. math                      ::
  .br
  \int_{-infty}^{+infty} (x-A_center)^(power_A) * (x-B_center)^power_B * exp(-alpha(x-A_center)^2) * exp(-beta(x-B_center)^2) dx
  .br


`phi_angular_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/angular_integration.irp.f#L41>`_
  Theta phi values together with the weights values for the angular integration :
  integral [dphi,dtheta] f(x,y,z) = 4 * pi * sum (1<i<n_points_integration_angular_lebedev) f(xi,yi,zi)
  Note that theta and phi are in DEGREES !!


`progress_active <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L29>`_
  Current status for displaying progress bars. Global variable.


`progress_bar <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L27>`_
  Current status for displaying progress bars. Global variable.


`progress_timeout <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L28>`_
  Current status for displaying progress bars. Global variable.


`progress_title <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L31>`_
  Current status for displaying progress bars. Global variable.


`progress_value <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L30>`_
  Current status for displaying progress bars. Global variable.


`psi_selectors_coef_transp <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/selectors.irp.f#L8>`_
  Transposed psi_selectors


`psi_selectors_diag_h_mat <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/selectors.irp.f#L22>`_
  Diagonal elements of the H matrix for each selectors


`psi_selectors_size <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/selectors.irp.f#L3>`_
  Undocumented


`quick_dsort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L262>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_i2sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L958>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_i8sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L726>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_isort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L494>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`quick_sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L30>`_
  Sort array x(isize) using the quicksort algorithm.
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`rec__quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L43>`_
  Undocumented


`rec_d_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L275>`_
  Undocumented


`rec_i2_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L971>`_
  Undocumented


`rec_i8_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L739>`_
  Undocumented


`rec_i_quicksort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L507>`_
  Undocumented


`recentered_poly2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L359>`_
  Recenter two polynomials


`rint <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L432>`_
  .. math::
  .br
  \int_0^1 dx \exp(-p x^2) x^n
  .br


`rint1 <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L592>`_
  Standard version of rint


`rint_large_n <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L561>`_
  Version of rint for large values of n


`rint_sum <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/integration.irp.f#L480>`_
  Needed for the calculation of two-electron integrals.


`rinteg <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L46>`_
  Undocumented


`rintgauss <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L30>`_
  Undocumented


`run_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L45>`_
  Display a progress bar with documentation of what is happening


`sabpartial <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/need.irp.f#L2>`_
  Undocumented


`set_order <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_323#L2>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.


`set_order_big <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_388#L33>`_
  array A has already been sorted, and iorder has contains the new order of
  elements of A. This subroutine changes the order of x to match the new order of A.
  This is a version for very large arrays where the indices need
  to be in integer*8 format


`set_zero_extra_diag <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L637>`_
  Undocumented


`sort <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_270#L2>`_
  Sort array x(isize).
  iorder in input should be (1,2,3,...,isize), and in output
  contains the new order of the elements.


`sorted_dnumber <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L441>`_
  Returns the number of sorted elements


`sorted_i2number <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L1137>`_
  Returns the number of sorted elements


`sorted_i8number <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L905>`_
  Returns the number of sorted elements


`sorted_inumber <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L673>`_
  Returns the number of sorted elements


`sorted_number <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/sort.irp.f_template_238#L209>`_
  Returns the number of sorted elements


`start_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L1>`_
  Starts the progress bar


`stop_progress <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/progress.irp.f#L19>`_
  Stop the progress bar


`svd <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/LinearAlgebra.irp.f#L1>`_
  Compute A = U.D.Vt
  .br
  LDx : leftmost dimension of x
  .br
  Dimsneion of A is m x n
  .br


`theta_angular_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/angular_integration.irp.f#L40>`_
  Theta phi values together with the weights values for the angular integration :
  integral [dphi,dtheta] f(x,y,z) = 4 * pi * sum (1<i<n_points_integration_angular_lebedev) f(xi,yi,zi)
  Note that theta and phi are in DEGREES !!


`transpose <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/transpose.irp.f#L2>`_
  Transpose input matrix A into output matrix B


`u_dot_u <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L304>`_
  Compute <u|u>


`u_dot_v <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L290>`_
  Compute <u|v>


`wall_time <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L259>`_
  The equivalent of cpu_time, but for the wall time.


`weights_angular_integration_lebedev <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/angular_integration.irp.f#L42>`_
  Theta phi values together with the weights values for the angular integration :
  integral [dphi,dtheta] f(x,y,z) = 4 * pi * sum (1<i<n_points_integration_angular_lebedev) f(xi,yi,zi)
  Note that theta and phi are in DEGREES !!


`write_git_log <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/util.irp.f#L234>`_
  Write the last git commit in file iunit.


`zmq_get_n_det_generators <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/zmq.irp.f_template_98#L37>`_
  Get N_det_generators from the qp_run scheduler


`zmq_get_n_det_selectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/zmq.irp.f_template_98#L130>`_
  Get N_det_selectors from the qp_run scheduler


`zmq_put_n_det_generators <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/zmq.irp.f_template_98#L3>`_
  Put N_det_generators on the qp_run scheduler


`zmq_put_n_det_selectors <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_Utils/zmq.irp.f_template_98#L96>`_
  Put N_det_selectors on the qp_run scheduler

