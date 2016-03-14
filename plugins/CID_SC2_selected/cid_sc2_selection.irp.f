program cisd_sc2_selected
  implicit none
  integer                        :: i,k
  use bitmasks

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:),E_old(:)
  integer                        :: N_st, iter,degree
  character*(64)                 :: perturbation
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st), H_pert_diag(N_st),E_old(N_st))
  
  pt2 = 1.d0
  perturbation = "epstein_nesbet_sc2_projected"
  E_old(1) = HF_energy
  threshold_davidson = 1.d-6 

  do while (maxval(abs(pt2(1:N_st))) > 1.d-4)
    print*,'----'
    print*,''
    call H_apply_cisd_selection(perturbation,pt2, norm_pert, H_pert_diag,  N_st)
!    soft_touch det_connections
    call diagonalize_CI_SC2
    print *,  'N_det                        = ', N_det
    do i = 1, N_st
     print*,'state ',i
     print *,  'PT2(SC2)                     = ', pt2(i)
     print *,  'E(SC2)                       = ', CI_SC2_energy(i) 
     print *,  'E_before(SC2)+PT2(SC2)       = ', (E_old(i)+pt2(i)) 
     if(i==1)then
      print *,  'E(SC2)+PT2(projctd)SC2       = ', (E_old(i)+H_pert_diag(i)) 
     endif
     E_old(i) = CI_SC2_energy(i)
    enddo
!   print *,  'E corr           = ', (E_old(1)) - HF_energy
  enddo
  pt2 = 0.d0
  call H_apply_PT2(pt2, norm_pert, H_pert_diag,  N_st)
  threshold_davidson = 1.d-10
  touch threshold_davidson davidson_criterion
  do i = 1, N_st
   max = 0.d0

    print*,''
    print*,'-------------'
    print*,'for state ',i
    print*,''
   do k = 1, N_det
    if(dabs(psi_coef(k,i)).gt.max)then
     max = dabs(psi_coef(k,i))
     imax = k
    endif
   enddo
   double precision :: max
   integer :: imax
   print *,  'PT2(SC2)                     = ', pt2(i)
   print *,  'E(SC2)                       = ', CI_SC2_energy(i)
   print *,  'E_before(SC2)+PT2(SC2)       = ', (CI_SC2_energy(i)+pt2(i))
   if(i==1)then
    print *,  'E(SC2)+PT2(projctd)SC2       = ', (CI_SC2_energy(i)+H_pert_diag(i))
   endif

   print*,'greater coeficient of the state : ',dabs(psi_coef(imax,i))
   call get_excitation_degree(ref_bitmask,psi_det(1,1,imax),degree,N_int)
   print*,'degree of excitation of such determinant : ',degree
   
  enddo
  deallocate(pt2,norm_pert,H_pert_diag)
end
