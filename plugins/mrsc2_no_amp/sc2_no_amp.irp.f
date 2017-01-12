program pouet
 implicit none
 integer :: i
 do i = 1, 10
  call diagonalize_CI_sc2_no_amp
  TOUCH psi_coef
 enddo

end
