program pouet
 provide ao_bielec_integrals_in_map
 call bla
end
subroutine bla
 implicit none
 integer :: i
 do i = 1, 10
  call diagonalize_CI_sc2_no_amp
  TOUCH psi_coef
 enddo
 print *,  "E+PT2      = ", ci_energy_sc2_no_amp(:)

end
