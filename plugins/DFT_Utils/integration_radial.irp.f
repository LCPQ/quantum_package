 BEGIN_PROVIDER [ double precision, integral_density_alpha_knowles_becke_per_atom, (nucl_num)]
&BEGIN_PROVIDER [ double precision, integral_density_beta_knowles_becke_per_atom, (nucl_num)]
 implicit none
 double precision :: accu
 integer :: i,j,k,l
 double precision :: x
 double precision :: integrand(n_points_angular_grid), weights(n_points_angular_grid)
 double precision :: f_average_angular_alpha,f_average_angular_beta
 double precision :: derivative_knowles_function,knowles_function

 ! Run over all nuclei in order to perform the Voronoi partition 
 ! according ot equation (6) of the paper of Becke (JCP, (88), 1988)
 ! Here the m index is referred to the w_m(r) weight functions of equation (22)
   ! Run over all points of integrations : there are  
   ! n_points_radial_grid (i) * n_points_angular_grid (k) 
   do j = 1, nucl_num 
    integral_density_alpha_knowles_becke_per_atom(j) = 0.d0
    integral_density_beta_knowles_becke_per_atom(j) = 0.d0
    do i = 1, n_points_radial_grid-1
     ! Angular integration over the solid angle Omega for a FIXED angular coordinate "r"
     f_average_angular_alpha = 0.d0
     f_average_angular_beta = 0.d0
     do k = 1, n_points_angular_grid
      f_average_angular_alpha += weights_angular_points(k) * one_body_dm_mo_alpha_at_grid_points(k,i,j) * weight_functions_at_grid_points(k,i,j)
      f_average_angular_beta  += weights_angular_points(k) * one_body_dm_mo_beta_at_grid_points(k,i,j)  * weight_functions_at_grid_points(k,i,j)
     enddo
     ! 
     x = grid_points_radial(i) ! x value for the mapping of the [0, +\infty] to [0,1]
     double precision ::  contrib_integration
!    print*,m_knowles
     contrib_integration = derivative_knowles_function(alpha_knowles(int(nucl_charge(j))),m_knowles,x) & 
                          *knowles_function(alpha_knowles(int(nucl_charge(j))),m_knowles,x)**2          
     integral_density_alpha_knowles_becke_per_atom(j) += contrib_integration *f_average_angular_alpha
     integral_density_beta_knowles_becke_per_atom(j) += contrib_integration *f_average_angular_beta
    enddo
    integral_density_alpha_knowles_becke_per_atom(j) *= dr_radial_integral
    integral_density_beta_knowles_becke_per_atom(j) *= dr_radial_integral
   enddo
 
END_PROVIDER 

 double precision function knowles_function(alpha,m,x)
 implicit none
 BEGIN_DOC
! function proposed by Knowles (JCP, 104, 1996) for distributing the radial points : 
! the Log "m" function ( equation (7) in the paper )
 END_DOC
 double precision, intent(in) :: alpha,x
 integer, intent(in) :: m
 knowles_function = -alpha * dlog(1.d0-x**m)
 end

 double precision function derivative_knowles_function(alpha,m,x)
 implicit none
 BEGIN_DOC
! derivative of the function proposed by Knowles (JCP, 104, 1996) for distributing the radial points
 END_DOC
 double precision, intent(in) :: alpha,x
 integer, intent(in) :: m
 derivative_knowles_function = alpha * dble(m) * x**(m-1) / (1.d0 - x**m)
 end

 BEGIN_PROVIDER [double precision, alpha_knowles, (100)]
 implicit none
 integer :: i
 BEGIN_DOC
! recommended values for the alpha parameters according to the paper of Knowles (JCP, 104, 1996)
! as a function of the nuclear charge
 END_DOC

 ! H-He
 alpha_knowles(1) = 5.d0 
 alpha_knowles(2) = 5.d0 
 
 ! Li-Be
 alpha_knowles(3) = 7.d0 
 alpha_knowles(4) = 7.d0 

 ! B-Ne
 do i = 5, 10
  alpha_knowles(i) = 5.d0 
 enddo

 ! Na-Mg
 do i = 11, 12
  alpha_knowles(i) = 7.d0 
 enddo

 ! Al-Ar
 do i = 13, 18
  alpha_knowles(i) = 5.d0 
 enddo

 ! K-Ca 
 do i = 19, 20
  alpha_knowles(i) = 7.d0 
 enddo

 ! Sc-Zn
 do i = 21, 30
  alpha_knowles(i) = 5.d0 
 enddo

 ! Ga-Kr
 do i = 31, 36
  alpha_knowles(i) = 7.d0 
 enddo

 END_PROVIDER 
