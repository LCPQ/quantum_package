============
Utils Module
============

Contains general purpose utilities.

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`apply_rotation <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L/subroutine apply_rotation(A,LDA,R,LDR,B,LDB,m,n)/;"
>`_
  Apply the rotation found by find_rotation

`find_rotation <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L/subroutine find_rotation(A,LDA,B,m,C,n)/;"
>`_
  Find A.C = B

`get_pseudo_inverse <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L/subroutine get_pseudo_inverse(A,m,n,C,LDA)/;"
>`_
  Find C = A^-1

`lapack_diag <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L/subroutine lapack_diag(eigvalues,eigvectors,H,nmax,n)/;"
>`_
  Diagonalize matrix H

`ortho_lowdin <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/LinearAlgebra.irp.f#L/subroutine ortho_lowdin(overlap,lda,n,C,ldc,m)/;"
>`_
  Compute U.S^-1/2 canonical orthogonalization

`add_poly <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine add_poly(b,nb,c,nc,d,nd)/;"
>`_
  Add two polynomials
  D(t) =! D(t) +( B(t)+C(t))

`add_poly_multiply <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine add_poly_multiply(b,nb,cst,d,nd)/;"
>`_
  Add a polynomial multiplied by a constant
  D(t) =! D(t) +( cst * B(t))

`f_integral <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/double precision function F_integral(n,p)/;"
>`_
  function that calculates the following integral
  \int_{\-infty}^{+\infty} x^n \exp(-p x^2) dx

`gaussian_product <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine gaussian_product(a,xa,b,xb,k,p,xp)/;"
>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}

`gaussian_product_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine gaussian_product_x(a,xa,b,xb,k,p,xp)/;"
>`_
  Gaussian product in 1D.
  e^{-a (x-x_A)^2} e^{-b (x-x_B)^2} = K_{ab}^x e^{-p (x-x_P)^2}

`give_explicit_poly_and_gaussian <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine give_explicit_poly_and_gaussian(P_new,P_center,p,fact_k,iorder,alpha,beta,a,b,A_center,B_center,dim)/;"
>`_
  Transforms the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k * [ sum (l_x = 0,i_order(1)) P_new(l_x,1) * (x-P_center(1))^l_x ] exp (- p (x-P_center(1))^2 )
  * [ sum (l_y = 0,i_order(2)) P_new(l_y,2) * (y-P_center(2))^l_y ] exp (- p (y-P_center(2))^2 )
  * [ sum (l_z = 0,i_order(3)) P_new(l_z,3) * (z-P_center(3))^l_z ] exp (- p (z-P_center(3))^2 )

`give_explicit_poly_and_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine give_explicit_poly_and_gaussian_x(P_new,P_center,p,fact_k,iorder,alpha,beta,a,b,A_center,B_center,dim)/;"
>`_
  Transform the product of
  (x-x_A)^a(1) (x-x_B)^b(1) (x-x_A)^a(2) (y-y_B)^b(2) (z-z_A)^a(3) (z-z_B)^b(3) exp(-(r-A)^2 alpha) exp(-(r-B)^2 beta)
  into
  fact_k  (x-x_P)^iorder(1)  (y-y_P)^iorder(2)  (z-z_P)^iorder(3) exp(-p(r-P)^2)

`hermite <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/double precision function hermite(n,x)/;"
>`_
  Hermite polynomial

`multiply_poly <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine multiply_poly(b,nb,c,nc,d,nd)/;"
>`_
  Multiply two polynomials
  D(t) =! D(t) +( B(t)*C(t))

`recentered_poly2 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/subroutine recentered_poly2(P_new,x_A,x_P,a,P_new2,x_B,x_Q,b)/;"
>`_
  Recenter two polynomials

`rint <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/double precision function rint(n,rho)/;"
>`_
  .. math::
  .br
  \int_0^1 dx \exp(-p x^2) x^n
  .br

`rint1 <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/double precision function rint1(n,rho)/;"
>`_
  Standard version of rint

`rint_large_n <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/double precision function rint_large_n(n,rho)/;"
>`_
  Version of rint for large values of n

`rint_sum <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/integration.irp.f#L/double precision function rint_sum(n_pt_out,rho,d1)/;"
>`_
  Needed for the calculation of two-electron integrals.

`overlap_gaussian_x <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L/double precision function overlap_gaussian_x(A_center,B_center,alpha,beta,power_A,power_B,dim)/;"
>`_
  .. math::
  .br
  \sum_{-infty}^{+infty} (x-A_x)^ax (x-B_x)^bx exp(-alpha(x-A_x)^2) exp(-beta(x-B_X)^2) dx
  .br

`overlap_gaussian_xyz <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L/subroutine overlap_gaussian_xyz(A_center,B_center,alpha,beta,power_A,&
>`_
  .. math::
  .br
  S_x = \int (x-A_x)^{a_x} exp(-\alpha(x-A_x)^2)  (x-B_x)^{b_x} exp(-beta(x-B_x)^2) dx \\
  S = S_x S_y S_z
  .br

`overlap_x_abs <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/one_e_integration.irp.f#L/subroutine overlap_x_abs(A_center,B_center,alpha,beta,power_A,power_B,overlap_x,lower_exp_val,dx,nx)/;"
>`_
  .. math                      ::
  .br
  \int_{-infty}^{+infty} (x-A_center)^(power_A) * (x-B_center)^power_B * exp(-alpha(x-A_center)^2) * exp(-beta(x-B_center)^2) dx
  .br

`align_double <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/integer function align_double(n)/;"
>`_
  Compute 1st dimension such that it is aligned for vectorization.

`binom <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/BEGIN_PROVIDER [ double precision, binom, (0:20,0:20) ]/;"
>`_
  Binomial coefficients

`binom_func <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/double precision function binom_func(i,j)/;"
>`_
  .. math                       ::
  .br
  \frac{i!}{j!(i-j)!}
  .br

`binom_transp <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/&BEGIN_PROVIDER [ double precision, binom_transp, (0:20,0:20) ]/;"
>`_
  Binomial coefficients

`dble_fact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/double precision function dble_fact(n) result(fact2)/;"
>`_
  n!!

`fact <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/double precision function fact(n)/;"
>`_
  n!

`fact_inv <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/BEGIN_PROVIDER [ double precision, fact_inv, (128) ]/;"
>`_
  1/n!

`inv_int <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/BEGIN_PROVIDER [ double precision, inv_int, (128) ]/;"
>`_
  1/i

`nproc <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/BEGIN_PROVIDER [ integer, nproc ]/;"
>`_
  Number of current OpenMP threads

`wall_time <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/subroutine wall_time(t)/;"
>`_
  The equivalent of cpu_time, but for the wall time.

`write_git_log <http://github.com/LCPQ/quantum_package/tree/master/src/Utils/util.irp.f#L/subroutine write_git_log(iunit)/;"
>`_
  Write the last git commit in file iunit.



 
