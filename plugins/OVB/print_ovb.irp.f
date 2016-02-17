program print_OVB
 implicit none
 read_wf = .True.
 call provide_all

end

subroutine provide_all
 implicit none
 integer :: i,j,k,l,istate
 do istate= 1, N_states
 print*,'-------------------'
 print*,'ISTATE = ',istate
 print*,'-------------------'
 print*,'CAS MATRIX         '
 print*,''
  do i = min_number_ionic,max_number_ionic
   write(*,'(I4,X,10(F8.5 ,4X))')i, H_OVB_naked(i,:,istate)
  enddo
  print*,''
  print*,'-------------------'
  print*,'-------------------'
 enddo


end

