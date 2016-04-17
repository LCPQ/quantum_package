 BEGIN_PROVIDER [ double precision, integral_density_alpha_knowles_becke_per_atom, (nucl_num)]
&BEGIN_PROVIDER [ double precision, integral_density_beta_knowles_becke_per_atom, (nucl_num)]
 implicit none
 double precision :: accu
 integer :: i,j,k,l
 integer :: m_param_knowles
 double precision :: dx,x
 integer :: n_pt_int_radial
 double precision :: integrand(n_points_angular_grid), weights(n_points_angular_grid)
 double precision :: f_average_angular_alpha,f_average_angular_beta
 double precision :: derivative_knowles_function,knowles_function
 n_pt_int_radial = 10
 dx = 1.d0/dble(n_pt_int_radial-1)
 x = 0.d0
 m_param_knowles = 3
 do j = 1, nucl_num
  integral_density_alpha_knowles_becke_per_atom(j) = 0.d0
  do i = 1, n_points_radial_grid
   ! Angular integration 
   f_average_angular_alpha = 0.d0
   f_average_angular_beta = 0.d0
   do k = 1, n_points_angular_grid
    f_average_angular_alpha += weights_angular_points(k) * one_body_dm_mo_alpha_at_grid_points(k,i,j) * weight_functions_at_grid_points(k,i,j)
    f_average_angular_beta += weights_angular_points(k) * one_body_dm_mo_beta_at_grid_points(k,i,j) * weight_functions_at_grid_points(k,i,j)
   enddo
   integral_density_alpha_knowles_becke_per_atom(j) += derivative_knowles_function(alpha,m_param_knowles,x) & 
                                                      *knowles_function(alpha,m_param_knowles,x)**2         &
                                                      *f_average_angular_alpha
   x += dx
  enddo
  integral_density_alpha_knowles_becke_per_atom(j) *= dx
 enddo
 
END_PROVIDER 

 double precision function knowles_function(alpha,m,x)
 implicit none
 double precision, intent(in) :: alpha,x
 integer, intent(in) :: m
 knowles_function = -alpha * dlog(1.d0-x**m)
 end

 double precision function derivative_knowles_function(alpha,m,x)
 implicit none
 double precision, intent(in) :: alpha,x
 integer, intent(in) :: m
 derivative_knowles_function = m x**(m-1) / (alpha * dlog(1.d0-x**m))
 end
