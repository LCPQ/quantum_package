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
  write(*,'(1000(F16.10,X))')one_body_dm_mo_beta(i,:,1)
 enddo
end

