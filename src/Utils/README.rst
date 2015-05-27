============
Utils Module
============

Contains general purpose utilities.

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES_CHILDREN file by the `update_README.py` script.

`apply_rotation <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L168>`_
  Apply the rotation found by find_rotation

`find_rotation <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L149>`_
  Find A.C = B

`get_pseudo_inverse <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L95>`_
  Find C = A^-1

`lapack_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L247>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br

`lapack_diag_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L310>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br

`lapack_diagd <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L180>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br

`lapack_partial_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L376>`_
  Diagonalize matrix H
  .br
  H is untouched between input and ouptut
  .br
  eigevalues(i) = ith lowest eigenvalue of the H matrix
  .br
  eigvectors(i,j) = <i|psi_j> where i is the basis function and psi_j is the j th eigenvector
  .br

`ortho_lowdin <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L1>`_
  Compute C_new=C_old.S^-1/2 canonical orthogonalization.
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

`set_zero_extra_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L433>`_
  Undocumented

`abort_all <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/abort.irp.f#L1>`_
  If True, all the calculation is aborted

`abort_here <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/abort.irp.f#L11>`_
  If True, all the calculation is aborted

`catch_signal <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/abort.irp.f#L30>`_
  What to do on Ctrl-C. If two Ctrl-C are pressed within 1 sec, the calculation if aborted.

`trap_signals <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/abort.irp.f#L19>`_
  What to do when a signal is caught. Here, trap Ctrl-C and call the control_C subroutine.

`add_poly <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L306>`_
  Add two polynomials
  D(t) =! D(t) +( B(t)+C(t))

`add_poly_multiply <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L334>`_
  Add a polynomial multiplied by a constant
  D(t) =! D(t) +( cst * B(t))

`f_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L408>`_
  function that calculates the following integral
  \int_{\-infty}^{+\infty} x^n \exp(-p x^2) dx

`gaussian_product <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L184>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}

`gaussian_product_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L226>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}

`give_explicit_poly_and_gaussian <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L46>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )

`give_explicit_poly_and_gaussian_double <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L122>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3)
  exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta) exp(-(r-Nucl_center)^2 gama
  .br
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )

`give_explicit_poly_and_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L1>`_
  Transform the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k  (x-x_P)^iorder(1)  (y-y_P)^iorder(2)  (z-z_P)^iorder(3) exp(-p(r-P)^2)

`hermite <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L540>`_
  Hermite polynomial

`multiply_poly <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L264>`_
  Multiply two polynomials
  D(t) =! D(t) +( B(t)*C(t))

`recentered_poly2 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L363>`_
  Recenter two polynomials

`rint <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L436>`_
  .. math::
  .br
  \int_0^1 dx \exp(-p x^2) x^n
  .br

`rint1 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L596>`_
  Standard version of rint

`rint_large_n <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L565>`_
  Version of rint for large values of n

`rint_sum <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L484>`_
  Needed for the calculation of two-electron integrals.

`a_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L252>`_
  Undocumented

`b_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L257>`_
  Undocumented

`ddfact2 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L243>`_
  Undocumented

`erf0 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L105>`_
  Undocumented

`gammln <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L271>`_
  Undocumented

`gammp <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L133>`_
  Undocumented

`gcf <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L211>`_
  Undocumented

`gser <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L167>`_
  Undocumented

`rinteg <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L47>`_
  Undocumented

`rintgauss <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L31>`_
  Undocumented

`sabpartial <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/need.irp.f#L2>`_
  Undocumented

`overlap_a_b_c <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L35>`_
  Undocumented

`overlap_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L1>`_
  .. math::
  .br
  \sum_{-infty}^{+infty} (x-A_x)^ax (x-B_x)^bx exp(-alpha(x-A_x)^2) exp(-beta(x-B_X)^2) dx
  .br

`overlap_gaussian_xyz <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L113>`_
  .. math::
  .br
  S_x = \int (x-A_x)^{a_x} exp(-\alpha(x-A_x)^2)  (x-B_x)^{b_x} exp(-beta(x-B_x)^2) dx \\
  S = S_x S_y S_z
  .br

`overlap_x_abs <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L175>`_
  .. math                      ::
  .br
  \int_{-infty}^{+infty} (x-A_center)^(power_A) * (x-B_center)^power_B * exp(-alpha(x-A_center)^2) * exp(-beta(x-B_center)^2) dx
  .br

`progress_active <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L29>`_
  Current status for displaying progress bars. Global variable.

`progress_bar <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L27>`_
  Current status for displaying progress bars. Global variable.

`progress_timeout <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L28>`_
  Current status for displaying progress bars. Global variable.

`progress_title <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L31>`_
  Current status for displaying progress bars. Global variable.

`progress_value <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L30>`_
  Current status for displaying progress bars. Global variable.

`run_progress <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L45>`_
  Display a progress bar with documentation of what is happening

`start_progress <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L1>`_
  Starts the progress bar

`stop_progress <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/progress.irp.f#L19>`_
  Stop the progress bar

`align_double <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L48>`_
  Compute 1st dimension such that it is aligned for vectorization.

`approx_dble <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L380>`_
  Undocumented

`binom <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L31>`_
  Binomial coefficients

`binom_func <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L1>`_
  .. math                       ::
  .br
  \frac{i!}{j!(i-j)!}
  .br

`binom_transp <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L32>`_
  Binomial coefficients

`dble_fact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L138>`_
  Undocumented

`dble_fact_even <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L155>`_
  n!!

`dble_fact_odd <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L176>`_
  n!!

`dble_logfact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L210>`_
  n!!

`fact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L63>`_
  n!

`fact_inv <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L125>`_
  1/n!

`inv_int <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L257>`_
  1/i

`logfact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L93>`_
  n!

`normalize <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L356>`_
  Normalizes vector u
  u is expected to be aligned in memory.

`nproc <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L283>`_
  Number of current OpenMP threads

`u_dot_u <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L325>`_
  Compute <u|u>

`u_dot_v <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L299>`_
  Compute <u|v>

`wall_time <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L268>`_
  The equivalent of cpu_time, but for the wall time.

`write_git_log <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L243>`_
  Write the last git commit in file iunit.




