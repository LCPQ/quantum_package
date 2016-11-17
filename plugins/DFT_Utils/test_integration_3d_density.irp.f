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
 do i = 1, nucl_num
  accu(1) += integral_density_alpha_knowles_becke_per_atom(i)
  accu(2) += integral_density_beta_knowles_becke_per_atom(i)
 enddo
 print*,'accu(1) = ',accu(1)
 print*,'Nalpha  = ',elec_alpha_num
 print*,'accu(2) = ',accu(2)
 print*,'Nalpha  = ',elec_beta_num


end
