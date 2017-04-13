program pouet
 print*,'coucou'
 read_wf = .True.
 touch read_wf
 print*,'m_knowles = ',m_knowles
 call routine

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
