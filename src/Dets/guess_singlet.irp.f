program put_gess
 use bitmasks
 implicit none
 integer :: i,j,N_det_tmp,N_states_tmp
 integer :: list(N_int*bit_kind_size,2)
 integer(bit_kind) :: string(N_int,2)
 integer(bit_kind) :: psi_det_tmp(N_int,2,2)
 double precision :: psi_coef_tmp(2,1)

 integer :: iorb,jorb
 print*,'which open shells ?'
 read(5,*)iorb,jorb
 N_states= 1
 N_det= 2


 list = 0
 list(1,1) = iorb 
 list(1,2) = jorb
 call list_to_bitstring( string(1,1), list(1,1), elec_alpha_num, N_int)
 call list_to_bitstring( string(1,2), list(1,2), elec_beta_num, N_int)
 call print_det(string,N_int)
 do j = 1,2
  do i = 1,  N_int 
   psi_det(i,j,1) = string(i,j)
  enddo
 enddo
 psi_coef(1,1) = 1.d0/dsqrt(2.d0)

 list = 0
 list(1,1) = jorb 
 list(1,2) = iorb
 call list_to_bitstring( string(1,1), list(1,1), elec_alpha_num, N_int)
 call list_to_bitstring( string(1,2), list(1,2), elec_beta_num, N_int)
 call print_det(string,N_int)
 do j = 1,2
  do i = 1,  N_int 
   psi_det(i,j,2) = string(i,j)
  enddo
 enddo
 psi_coef(2,1) = 1.d0/dsqrt(2.d0)

 call save_wavefunction
end
