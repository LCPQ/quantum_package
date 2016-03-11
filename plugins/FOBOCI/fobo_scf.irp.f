program foboscf
 implicit none
 no_oa_or_av_opt = .True.
 touch no_oa_or_av_opt
 call run_prepare
 call routine_fobo_scf
 call save_mos

end

subroutine run_prepare
 implicit none
  call damping_SCF
  call diag_inactive_virt_and_update_mos
end

subroutine routine_fobo_scf
 implicit none
 integer :: i,j
 print*,''
 print*,''
 character*(64) :: label
 label = "Natural"
 do i = 1, 5
  call FOBOCI_lmct_mlct_old_thr
  call save_osoci_natural_mos
  call damping_SCF
  call diag_inactive_virt_and_update_mos
  call clear_mo_map
  call provide_properties
 enddo



end
