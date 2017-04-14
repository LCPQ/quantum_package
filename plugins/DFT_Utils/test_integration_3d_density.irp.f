program pouet
 print*,'coucou'
 read_wf = .True.
 touch read_wf
 print*,'m_knowles = ',m_knowles
 call routine
 call routine3

end



subroutine routine3
 implicit none
 integer :: i,j,k,l
 double precision :: accu
 accu = 0.d0
  do j = 1, nucl_num  ! that are referred to each atom 
   do i = 1, n_points_radial_grid -1  !for each radial grid attached to the "jth" atom
    do k = 1, n_points_integration_angular ! for each angular point attached to the "jth" atom
     accu += final_weight_functions_at_grid_points(k,i,j) * one_body_dm_mo_alpha_at_grid_points(k,i,j,1)
    enddo
   enddo
  enddo
  print*, accu
 print*, 'lda_exchange',lda_exchange

end
subroutine routine2
 implicit none
 integer :: i,j,k,l
 double precision :: x,y,z
 double precision :: r
 double precision :: accu 
 accu = 0.d0
 r = 1.d0
 do k = 1, n_points_integration_angular 
  x  =  angular_quadrature_points(k,1) * r
  y  =  angular_quadrature_points(k,2) * r
  z  =  angular_quadrature_points(k,3) * r
  accu += weights_angular_points(k) * (x**2 + y**2 + z**2)
 enddo
 print*, accu

end


subroutine routine
 implicit none
 integer :: i
 double precision :: accu(2)
 accu = 0.d0
 do i = 1, N_det
  call debug_det(psi_det(1,1,i),N_int)
 enddo
 do i = 1, nucl_num
  accu(1) += integral_density_alpha_knowles_becke_per_atom(i)
  accu(2) += integral_density_beta_knowles_becke_per_atom(i)
 enddo
 print*,'accu(1) = ',accu(1)
 print*,'Nalpha  = ',elec_alpha_num
 print*,'accu(2) = ',accu(2)
 print*,'Nalpha  = ',elec_beta_num
 
 accu = 0.d0
 do i = 1, mo_tot_num
  accu(1) += one_body_dm_mo_alpha_average(i,i)
  accu(2) += one_body_dm_mo_beta_average(i,i)
 enddo

 
 print*,'          '
 print*,'          '
 print*,'accu(1) = ',accu(1)
 print*,'accu(2) = ',accu(2)


end
