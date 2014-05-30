program cisd_sc2_selected
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:),E_old(:)
  integer                        :: N_st, iter
  character*(64)                 :: perturbation
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st), H_pert_diag(N_st),E_old(N_st))
  
  pt2 = 1.d0
  perturbation = "epstein_nesbet_sc2_projected"
  E_old(1) = HF_energy
  do while (maxval(abs(pt2(1:N_st))) > 1.d-9)
    print*,'----'
    print*,''
    call H_apply_cisd_selection(perturbation,pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI_SC2
    print *,  'N_det                        = ', N_det
    print *,  'PT2(SC2)                     = ', pt2
    print *,  'E(SC2)                       = ', CI_SC2_energy(1) 
    print *,  'E_before(SC2)+PT2(SC2)       = ', (E_old(1)+pt2(1))
    print *,  'E(SC2)+PT2(projctd)SC2       = ', (E_old(1)+H_pert_diag(1)) 
!   print *,  'E corr           = ', (E_old(1)) - HF_energy
    E_old(1) = CI_SC2_energy(1)
    if (abort_all) then
      exit
    endif
  enddo
  deallocate(pt2,norm_pert,H_pert_diag)
end
