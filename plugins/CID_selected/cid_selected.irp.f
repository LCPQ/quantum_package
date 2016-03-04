program cisd
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:),E_old(:)
  integer                        :: N_st, iter
  character*(64)                 :: perturbation
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st), H_pert_diag(N_st),E_old(N_st))
  
  pt2 = 1.d0
  perturbation = "epstein_nesbet"
  E_old(1) = HF_energy
  do while (maxval(abs(pt2(1:N_st))) > 1.d-6)
    print*,'----'
    print*,''
    call H_apply_cisd_selection(perturbation,pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    print*,'N_det = ',N_det
    do i = 1, N_st
     print*,'state ',i
     print *,  'PT2                          = ', pt2(i)
     print *,  'E                            = ', CI_energy(i) 
     print *,  'E_before     +PT2            = ', (E_old(i)+pt2(i)) 
!   print *,  'E+PT2_new= ', (E_old(1)+1.d0*pt2(1)+H_pert_diag(1))/(1.d0 +norm_pert(1))
    enddo
    E_old = CI_energy
  enddo
  deallocate(pt2,norm_pert,H_pert_diag)
end
