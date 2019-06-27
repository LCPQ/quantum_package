program print_hcc_main
 implicit none
 read_wf = .True.
 touch read_wf
! call print_hcc
  call routine
end


subroutine routine
 implicit none
 integer :: i
 do i = 1, mo_tot_num
  write(*,'(I3,X,1000(F16.10,X))')i,one_body_dm_mo_beta(i,i,1)
 enddo
end

