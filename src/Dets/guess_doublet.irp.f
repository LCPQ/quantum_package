program put_gess
 use bitmasks
 implicit none
 integer :: i,j,N_det_tmp,N_states_tmp
 integer :: list(N_int*bit_kind_size,2)
 integer(bit_kind) :: string(N_int,2)
 integer(bit_kind) :: psi_det_tmp(N_int,2,3)
 double precision :: psi_coef_tmp(3,1)

 integer :: iorb,jorb,korb
 print*,'which open shells ?'
 read(5,*)iorb,jorb,korb
 print*,iorb,jorb,korb
 N_states= 1
 N_det= 3


 list = 0
 list(1,1) = 1
 list(1,2) = 1
 list(2,1) = 2
 list(2,2) = 2
 list(3,1) = iorb 
 list(4,1) = jorb
 list(3,2) = korb
 print*,'passed'
 call list_to_bitstring( string(1,1), list(1,1), elec_alpha_num, N_int)
 print*,'passed'
 call list_to_bitstring( string(1,2), list(1,2), elec_beta_num, N_int)
 print*,'passed'
 call print_det(string,N_int)
 do j = 1,2
  do i = 1,  N_int 
   psi_det(i,j,1) = string(i,j)
  enddo
 enddo
 psi_coef(1,1) = 1.d0/dsqrt(3.d0)

 print*,'passed 1'
 list = 0
 list(1,1) = 1
 list(1,2) = 1
 list(2,1) = 2
 list(2,2) = 2
 list(3,1) = iorb 
 list(4,1) = korb
 list(3,2) = jorb
 call list_to_bitstring( string(1,1), list(1,1), elec_alpha_num, N_int)
 call list_to_bitstring( string(1,2), list(1,2), elec_beta_num, N_int)
 call print_det(string,N_int)
 do j = 1,2
  do i = 1,  N_int 
   psi_det(i,j,2) = string(i,j)
  enddo
 enddo
 psi_coef(2,1) = 1.d0/dsqrt(3.d0)

 print*,'passed 2'
 list = 0
 list(1,1) = 1
 list(1,2) = 1
 list(2,1) = 2
 list(2,2) = 2
 list(3,1) = korb 
 list(4,1) = jorb
 list(3,2) = iorb
 call list_to_bitstring( string(1,1), list(1,1), elec_alpha_num, N_int)
 call list_to_bitstring( string(1,2), list(1,2), elec_beta_num, N_int)
 call print_det(string,N_int)
 do j = 1,2
  do i = 1,  N_int 
   psi_det(i,j,3) = string(i,j)
  enddo
 enddo
 psi_coef(3,1) = 1.d0/dsqrt(3.d0)
 print*,'passed 3'

 call save_wavefunction
end
