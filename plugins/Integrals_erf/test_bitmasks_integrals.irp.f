program pouet
 implicit none

 call routine

end

subroutine routine 
 implicit none
  integer(bit_kind)  :: mask_ijkl(N_int,4)
  integer, allocatable           :: list_ijkl(:,:)
  integer :: i,j
  integer :: n_i,n_j,n_k,n_l
  do i = 1,N_int 
   mask_ijkl(i,1) = inact_bitmask(i,1)
   mask_ijkl(i,2) = inact_bitmask(i,1)
   mask_ijkl(i,3) = inact_bitmask(i,1)
   mask_ijkl(i,4) = inact_bitmask(i,1)
  enddo
  allocate(list_ijkl(mo_tot_num,4))
  call bitstring_to_list( mask_ijkl(1,1), list_ijkl(1,1), n_i, N_int )
  call bitstring_to_list( mask_ijkl(1,2), list_ijkl(1,2), n_j, N_int )
  call bitstring_to_list( mask_ijkl(1,3), list_ijkl(1,3), n_k, N_int )
  call bitstring_to_list( mask_ijkl(1,4), list_ijkl(1,4), n_l, N_int )
  print*,'n_i,n_j = ',n_i,n_j
  print*,'n_k,n_l = ',n_k,n_l
  do i =1, n_i
   print*,list_ijkl(i,1), list_ijkl(i,2)
  enddo
  deallocate(list_ijkl)
 

end
