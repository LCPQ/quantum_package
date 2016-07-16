program test
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 integer :: i,j,k,l
 do i = 1, n_act_orb
  do j = 1, n_act_orb
   do k = 1, n_act_orb
    
   enddo
  enddo
 enddo
end
