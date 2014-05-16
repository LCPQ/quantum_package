============
Utils Module
============

Contains general purpose utilities.

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`apply_rotation <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L146>`_
  Apply the rotation found by find_rotation

`find_rotation <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L127>`_
  Find A.C = B

`get_pseudo_inverse <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L73>`_
  Find C = A^-1

`lapack_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L158>`_
  Diagonalize matrix H

`ortho_lowdin <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L1>`_
  Compute U.S^-1/2 canonical orthogonalization

`add_poly <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L243>`_
  Add two polynomials
  D(t) =! D(t) +( B(t)+C(t))

`add_poly_multiply <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L271>`_
  Add a polynomial multiplied by a constant
  D(t) =! D(t) +( cst * B(t))

`f_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L345>`_
  function that calculates the following integral
  \int_{\-infty}^{+\infty} x^n \exp(-p x^2) dx

`gaussian_product <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L121>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}

`gaussian_product_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L163>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}

`give_explicit_poly_and_gaussian <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L46>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )

`give_explicit_poly_and_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L1>`_
  Transform the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k  (x-x_P)^iorder(1)  (y-y_P)^iorder(2)  (z-z_P)^iorder(3) exp(-p(r-P)^2)

`hermite <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L468>`_
  Hermite polynomial

`multiply_poly <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L201>`_
  Multiply two polynomials
  D(t) =! D(t) +( B(t)*C(t))

`recentered_poly2 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L300>`_
  Recenter two polynomials

`rint <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L373>`_
  .. math::
  .br
  \int_0^1 dx \exp(-p x^2) x^n
  .br

`rint1 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L524>`_
  Standard version of rint

`rint_large_n <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L493>`_
  Version of rint for large values of n

`rint_sum <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L417>`_
  Needed for the calculation of two-electron integrals.

`overlap_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L1>`_
  .. math::
  .br
  \sum_{-infty}^{+infty} (x-A_x)^ax (x-B_x)^bx exp(-alpha(x-A_x)^2) exp(-beta(x-B_X)^2) dx
  .br

`overlap_gaussian_xyz <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L37>`_
  .. math::
  .br
  S_x = \int (x-A_x)^{a_x} exp(-\alpha(x-A_x)^2)  (x-B_x)^{b_x} exp(-beta(x-B_x)^2) dx \\
  S = S_x S_y S_z
  .br

`overlap_x_abs <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L99>`_
  .. math                      ::
  .br
  \int_{-infty}^{+infty} (x-A_center)^(power_A) * (x-B_center)^power_B * exp(-alpha(x-A_center)^2) * exp(-beta(x-B_center)^2) dx
  .br

`align_double <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L65>`_
  Compute 1st dimension such that it is aligned for vectorization.

`all_utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L1>`_
  Dummy provider to provide all utils

`binom <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L47>`_
  Binomial coefficients

`binom_func <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L16>`_
  .. math                       ::
  .br
  \frac{i!}{j!(i-j)!}
  .br

`binom_transp <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L48>`_
  Binomial coefficients

`dble_fact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L124>`_
  n!!

`fact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L80>`_
  n!

`fact_inv <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L112>`_
  1/n!

`inv_int <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L171>`_
  1/i

`normalize <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L272>`_
  Normalizes vector u
  u is expected to be aligned in memory.

`nproc <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L197>`_
  Number of current OpenMP threads

`u_dot_u <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L244>`_
  Compute <u|u>
  u is expected to be aligned in memory.

`u_dot_v <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L213>`_
  Compute <u|v>
  u and v are expected to be aligned in memory.

`wall_time <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L182>`_
  The equivalent of cpu_time, but for the wall time.

`write_git_log <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L157>`_
  Write the last git commit in file iunit.



 
