BEGIN_PROVIDER [integer, n_points_angular_grid]
 implicit none
 n_points_angular_grid = 18
END_PROVIDER 

BEGIN_PROVIDER [integer, n_points_radial_grid]
 implicit none
 n_points_radial_grid = 10
END_PROVIDER 


 BEGIN_PROVIDER [double precision, angular_quadrature_points, (n_points_angular_grid,3) ]
&BEGIN_PROVIDER [double precision, weights_angular_points, (n_points_angular_grid)]
 implicit none
 BEGIN_DOC
! weights and grid points for the integration on the angular variables on 
! the unit sphere centered on (0,0,0)
 END_DOC
 call cal_quad(n_points_aangular_grid, angular_quadrature_points,weights_angular_points)

END_PROVIDER 

BEGIN_PROVIDER [double precision, grid_points_per_atom, (3,n_points_angular_grid,n_points_radial_grid,nucl_num)]
 BEGIN_DOC
! points for integration over space
 END_DOC
 implicit none
 integer :: i,j,k
 double precision :: dr,x_ref,y_ref,z_ref
 dr = 1.d0/dble(n_points_radial_grid-1)
 do i = 1, nucl_num
  x_ref = nucl_coord(i,1)
  y_ref = nucl_coord(i,2)
  z_ref = nucl_coord(i,3)
  do j = 1, n_points_radial_grid
   do k = 1, n_points_angular_grid
    grid_points_per_atom(1,k,j,i) = x_ref + angular_quadrature_points(k,1) * dr
    grid_points_per_atom(2,k,j,i) = y_ref + angular_quadrature_points(k,2) * dr
    grid_points_per_atom(3,k,j,i) = z_ref + angular_quadrature_points(k,3) * dr
   enddo
  enddo
 enddo
END_PROVIDER 

BEGIN_PROVIDER [double precision, weight_functions_at_grid_points, (nucl_num,n_points_angular_grid,n_points_radial_grid) ]
 BEGIN_DOC 
! Weight function at grid points : w_n(r) according to the equation (22) of Becke original paper (JCP, 88, 1988)
! the "n" discrete variable represents the nucleis (j=1,nucl_num) 
 END_DOC
 implicit none
 integer :: i,j,k,l,m
 double precision :: r(3)
 double precision :: accu,cell_function_becke
 double precision :: tmp_array(nucl_num)
  do j = 1, nucl_num
   do k = 1, n_points_radial_grid
    do l = 1, n_points_angular_grid
     r(1) = grid_points_per_atom(1,j,k,l)
     r(2) = grid_points_per_atom(2,j,k,l)
     r(3) = grid_points_per_atom(3,j,k,l)
     accu = 0.d0
     do i = 1, nucl_num
      tmp_array(i) = cell_function_becke(r,i)
      accu += tmp_array(i)
     enddo
     accu = 1.d0/accu
     do i = 1, nucl_num
      weight_functions_at_grid_points(i,j,k,l) = tmp_array(i)*accu
     enddo
    enddo
   enddo
  enddo


END_PROVIDER 

 BEGIN_PROVIDER [double precision, one_body_dm_mo_alpha_at_grid_points, (n_points_angular_grid,n_points_radial_grid,nucl_num) ]
&BEGIN_PROVIDER [double precision, one_body_dm_mo_beta_at_grid_points, (n_points_angular_grid,n_points_radial_grid,nucl_num) ]
 implicit none
 integer :: i,j,k,l,m
 double precision :: contrib
 double precision :: r(3)
 double precision :: aos_array(ao_num)
  do j = 1, nucl_num
   do k = 1, n_points_radial_grid
    do l = 1, n_points_angular_grid
     r(1) = grid_points_per_atom(1,j,k,l)
     r(2) = grid_points_per_atom(2,j,k,l)
     r(3) = grid_points_per_atom(3,j,k,l)
     call give_all_aos_at_r(r,aos_array)
     one_body_dm_mo_alpha_at_grid_points(j,k,l) = 0.d0
     do i = 1, ao_num
       do m = 1, ao_num
        contrib = aos_array(i) * aos_array(m)
        one_body_dm_mo_alpha_at_grid_points(j,k,l) +=  one_body_dm_ao_alpha(i,m) * contrib
        one_body_dm_mo_beta_at_grid_points(j,k,l) +=  one_body_dm_ao_beta(i,m)  * contrib
       enddo
     enddo
    enddo
   enddo
  enddo

END_PROVIDER 

