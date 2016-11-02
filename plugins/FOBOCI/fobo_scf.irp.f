program foboscf
 implicit none
!if(disk_access_ao_integrals == "None" .or. disk_access_ao_integrals == "Read" )then
! disk_access_ao_integrals = "Write"
! touch disk_access_ao_integrals
!endif
!print*, 'disk_access_ao_integrals',disk_access_ao_integrals
 no_oa_or_av_opt = .True.
 touch no_oa_or_av_opt
 call run_prepare
 call routine_fobo_scf
 call save_mos

end

subroutine run_prepare
 implicit none
! no_oa_or_av_opt = .False.
! touch no_oa_or_av_opt
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
  print*,'*******************************************************************************'
  print*,'*******************************************************************************'
  print*,'FOBO-SCF Iteration ',i
  print*, 'ao_bielec_integrals_in_map = ',ao_bielec_integrals_in_map
  print*,'*******************************************************************************'
  print*,'*******************************************************************************'
  if(speed_up_convergence_foboscf)then
   if(i==3)then
    threshold_lmct = max(threshold_lmct,0.001)
    threshold_mlct = max(threshold_mlct,0.05)
    soft_touch threshold_lmct threshold_mlct
   endif
   if(i==4)then
    threshold_lmct = max(threshold_lmct,0.005)
    threshold_mlct = max(threshold_mlct,0.07)
    soft_touch threshold_lmct threshold_mlct
   endif
   if(i==5)then
    threshold_lmct = max(threshold_lmct,0.01)
    threshold_mlct = max(threshold_mlct,0.1)
    soft_touch threshold_lmct threshold_mlct
   endif
  endif
  call FOBOCI_lmct_mlct_old_thr(i)
  call save_osoci_natural_mos
  call damping_SCF
  call diag_inactive_virt_and_update_mos
  call clear_mo_map
  call provide_properties
 enddo



end
