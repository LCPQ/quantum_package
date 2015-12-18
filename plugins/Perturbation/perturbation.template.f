BEGIN_SHELL [ /usr/bin/env python ]
import perturbation
END_SHELL

subroutine perturb_buffer_$PERT(i_generator,buffer,buffer_size,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert,sum_norm_pert,sum_H_pert_diag,N_st,Nint,key_mask,fock_diag_tmp)
  implicit none
  BEGIN_DOC
  !  Applly pertubration ``$PERT`` to the buffer of determinants generated in the H_apply
  ! routine.
  END_DOC
  
  integer, intent(in)            :: Nint, N_st, buffer_size, i_generator
  integer(bit_kind), intent(in)  :: buffer(Nint,2,buffer_size)
  integer(bit_kind),intent(in)    :: key_mask(Nint,2)
  double precision, intent(in)    :: fock_diag_tmp(2,0:mo_tot_num)
  double precision, intent(inout) :: sum_norm_pert(N_st),sum_e_2_pert(N_st)
  double precision, intent(inout) :: coef_pert_buffer(N_st,buffer_size),e_2_pert_buffer(N_st,buffer_size),sum_H_pert_diag(N_st)
  double precision               :: c_pert(N_st), e_2_pert(N_st),  H_pert_diag(N_st)
  integer                        :: i,k, c_ref, ni, ex
  integer, external              :: connected_to_ref
  logical, external              :: is_in_wavefunction
  
  integer(bit_kind), allocatable :: minilist(:,:,:)
  integer, allocatable           :: idx_minilist(:)
  integer                        :: N_minilist
  
  integer(bit_kind), allocatable :: minilist_gen(:,:,:)
  integer :: N_minilist_gen
  logical :: fullMatch
  logical, external :: is_connected_to

  allocate( minilist(Nint,2,N_det_selectors),                        &
      minilist_gen(Nint,2,N_det_generators),                         &
      idx_minilist(N_det_selectors) )
  

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (buffer_size >= 0)
  ASSERT (minval(sum_norm_pert) >= 0.d0)
  ASSERT (N_st > 0)
  
  call create_minilist_find_previous(key_mask, psi_det_generators, miniList_gen, i_generator-1, N_minilist_gen, fullMatch, Nint)
  
  if(fullMatch) then
    deallocate( minilist, minilist_gen, idx_minilist )
    return
  end if
  
  call create_minilist(key_mask, psi_selectors, miniList, idx_miniList, N_det_selectors, N_minilist, Nint)
  
  do i=1,buffer_size
    
    if (is_in_wavefunction(buffer(1,1,i),Nint)) then
      cycle
    endif
    
    if(is_connected_to(buffer(1,1,i), miniList_gen, Nint, N_minilist_gen)) then
      cycle
    end if
    

    
    call pt2_$PERT(psi_det_generators(1,1,i_generator),buffer(1,1,i), fock_diag_tmp,        &
         c_pert,e_2_pert,H_pert_diag,Nint,N_minilist,n_st,minilist,idx_minilist,N_minilist) 

    do k = 1,N_st
      e_2_pert_buffer(k,i)   = e_2_pert(k)
      coef_pert_buffer(k,i)  = c_pert(k)
      sum_norm_pert(k)       = sum_norm_pert(k)   + c_pert(k) * c_pert(k)
      sum_e_2_pert(k)        = sum_e_2_pert(k)    + e_2_pert(k)
      sum_H_pert_diag(k)     = sum_H_pert_diag(k) + H_pert_diag(k)
    enddo
    
  enddo 
  deallocate( minilist, minilist_gen, idx_minilist )
  
end



subroutine perturb_buffer_by_mono_$PERT(i_generator,buffer,buffer_size,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert,sum_norm_pert,sum_H_pert_diag,N_st,Nint,key_mask,fock_diag_tmp)
  implicit none
  BEGIN_DOC
  !  Applly pertubration ``$PERT`` to the buffer of determinants generated in the H_apply
  ! routine.
  END_DOC
  
  integer, intent(in)            :: Nint, N_st, buffer_size, i_generator
  integer(bit_kind), intent(in)  :: buffer(Nint,2,buffer_size)
  integer(bit_kind),intent(in)    :: key_mask(Nint,2)
  double precision, intent(in)    :: fock_diag_tmp(2,0:mo_tot_num)
  double precision, intent(inout) :: sum_norm_pert(N_st),sum_e_2_pert(N_st)
  double precision, intent(inout) :: coef_pert_buffer(N_st,buffer_size),e_2_pert_buffer(N_st,buffer_size),sum_H_pert_diag(N_st)
  double precision               :: c_pert(N_st), e_2_pert(N_st),  H_pert_diag(N_st)
  integer                        :: i,k, c_ref, ni, ex
  integer, external              :: connected_to_ref_by_mono
  logical, external              :: is_in_wavefunction
  
  integer(bit_kind), allocatable :: minilist(:,:,:)
  integer, allocatable           :: idx_minilist(:)
  integer                        :: N_minilist
  
  integer(bit_kind), allocatable :: minilist_gen(:,:,:)
  integer :: N_minilist_gen
  logical :: fullMatch
  logical, external :: is_connected_to

  allocate( minilist(Nint,2,N_det_selectors),                        &
      minilist_gen(Nint,2,N_det_generators),                         &
      idx_minilist(N_det_selectors) )
  

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (buffer_size >= 0)
  ASSERT (minval(sum_norm_pert) >= 0.d0)
  ASSERT (N_st > 0)
  
  call create_minilist(key_mask, psi_selectors, miniList, idx_miniList, N_det_selectors, N_minilist, Nint)
  call create_minilist_find_previous(key_mask, psi_det_generators, miniList_gen, i_generator-1, N_minilist_gen, fullMatch, Nint)
  
  if(fullMatch) then
    deallocate( minilist, minilist_gen, idx_minilist )
    return
  end if
  
  
  do i=1,buffer_size
    
    c_ref = connected_to_ref_by_mono(buffer(1,1,i),psi_det_generators,Nint,i_generator,N_det)

    if (c_ref /= 0) then
      cycle
    endif
    
    if (is_in_wavefunction(buffer(1,1,i),Nint)) then
      cycle
    endif
    
    call pt2_$PERT(psi_det_generators(1,1,i_generator),buffer(1,1,i), fock_diag_tmp,        &
         c_pert,e_2_pert,H_pert_diag,Nint,N_minilist,n_st,minilist,idx_minilist,N_minilist) 

    do k = 1,N_st
      e_2_pert_buffer(k,i)   = e_2_pert(k)
      coef_pert_buffer(k,i)  = c_pert(k)
      sum_norm_pert(k)       = sum_norm_pert(k)   + c_pert(k) * c_pert(k)
      sum_e_2_pert(k)        = sum_e_2_pert(k)    + e_2_pert(k)
      sum_H_pert_diag(k)     = sum_H_pert_diag(k) + H_pert_diag(k)
    enddo
    
  enddo 
  deallocate( minilist, minilist_gen, idx_minilist )
  
end

