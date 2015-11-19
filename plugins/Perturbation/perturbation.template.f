BEGIN_SHELL [ /usr/bin/env python ]
import perturbation
END_SHELL

subroutine perturb_buffer_$PERT(i_generator,buffer,buffer_size,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert,sum_norm_pert,sum_H_pert_diag,N_st,Nint,key_mask)
  implicit none
  BEGIN_DOC
  !  Applly pertubration ``$PERT`` to the buffer of determinants generated in the H_apply
  ! routine.
  END_DOC
  
  integer, intent(in)            :: Nint, N_st, buffer_size, i_generator
  integer(bit_kind), intent(in)  :: buffer(Nint,2,buffer_size)
  integer(bit_kind),intent(in)    :: key_mask(Nint,2)
  double precision, intent(inout) :: sum_norm_pert(N_st),sum_e_2_pert(N_st)
  double precision, intent(inout) :: coef_pert_buffer(N_st,buffer_size),e_2_pert_buffer(N_st,buffer_size),sum_H_pert_diag(N_st)
  double precision               :: c_pert(N_st), e_2_pert(N_st),  H_pert_diag(N_st)
  integer                        :: i,k, c_ref, ni, ex
  integer, external              :: connected_to_ref
  logical, external              :: is_in_wavefunction
  
  integer(bit_kind) :: minilist(Nint,2,N_det_selectors)
  integer :: idx_minilist(N_det_selectors), N_minilist
  
  integer(bit_kind) :: minilist_gen(Nint,2,N_det_generators)
  integer :: idx_minilist_gen(N_det_generators), N_minilist_gen
  

  call create_minilist(key_mask, psi_selectors, miniList, idx_miniList, N_det_selectors, N_minilist, Nint)
  call create_minilist(key_mask, psi_det_generators, miniList_gen, idx_miniList_gen, N_det_generators, N_minilist_gen, Nint)

  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (buffer_size >= 0)
  ASSERT (minval(sum_norm_pert) >= 0.d0)
  ASSERT (N_st > 0)
  
  buffer_loop : do i = 1,buffer_size
    
    do k=1,N_minilist_gen
      if(idx_minilist_gen(k) >= i_generator) then
        exit
      end if
      ex = 0
      do ni=1,Nint
        ex += popcnt(xor(minilist_gen(ni,1,k), buffer(ni,1,i))) + popcnt(xor(minilist_gen(ni,2,k), buffer(ni,2,i)))
      end do
      if(ex <= 4) then
        cycle buffer_loop
      end if
    end do
    
!     c_ref = connected_to_ref(buffer(1,1,i),psi_det_generators,Nint,i_generator,N_det_generators)
! 
!     if (c_ref /= 0) then
!       cycle
!     endif
    
    if (is_in_wavefunction(buffer(1,1,i),Nint)) then
      cycle
    endif
    
    integer :: degree
    call get_excitation_degree(HF_bitmask,buffer(1,1,i),degree,N_int)
!     call pt2_$PERT(buffer(1,1,i),         &
!         c_pert,e_2_pert,H_pert_diag,Nint,N_det_selectors,n_st,minilist,idx_minilist)
    call pt2_$PERT(buffer(1,1,i),         &
         c_pert,e_2_pert,H_pert_diag,Nint,N_minilist,n_st,minilist,idx_minilist,N_minilist) !!!!!!!!!!!!!!!!! MAUVAISE SIGNATURE PR LES AUTRES PT2_* !!!!!

    do k = 1,N_st
      e_2_pert_buffer(k,i) = e_2_pert(k)
      coef_pert_buffer(k,i) = c_pert(k)
      sum_norm_pert(k) += c_pert(k) * c_pert(k)
      sum_e_2_pert(k) += e_2_pert(k)
      sum_H_pert_diag(k) +=  H_pert_diag(k)
    enddo
    
  enddo buffer_loop
  
end


subroutine perturb_buffer_by_mono_$PERT(i_generator,buffer,buffer_size,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert,sum_norm_pert,sum_H_pert_diag,N_st,Nint)
  implicit none
  BEGIN_DOC
  !  Applly pertubration ``$PERT`` to the buffer of determinants generated in the H_apply
  ! routine.
  END_DOC
  
  integer, intent(in)            :: Nint, N_st, buffer_size, i_generator
  integer(bit_kind), intent(in)  :: buffer(Nint,2,buffer_size)
  double precision, intent(inout) :: sum_norm_pert(N_st),sum_e_2_pert(N_st)
  double precision, intent(inout) :: coef_pert_buffer(N_st,buffer_size),e_2_pert_buffer(N_st,buffer_size),sum_H_pert_diag(N_st)
  double precision               :: c_pert(N_st), e_2_pert(N_st),  H_pert_diag(N_st)
  integer                        :: i,k, c_ref
  integer, external              :: connected_to_ref_by_mono
  logical, external              :: is_in_wavefunction
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (buffer_size >= 0)
  ASSERT (minval(sum_norm_pert) >= 0.d0)
  ASSERT (N_st > 0)
  do i = 1,buffer_size

    c_ref = connected_to_ref_by_mono(buffer(1,1,i),psi_det_generators,Nint,i_generator,N_det)

    if (c_ref /= 0) then
      cycle
    endif
    
    if (is_in_wavefunction(buffer(1,1,i),Nint)) then
      cycle
    endif
    
   integer :: degree
    call pt2_$PERT(buffer(1,1,i),         &
        c_pert,e_2_pert,H_pert_diag,Nint,N_det_selectors,n_st)

    do k = 1,N_st
      e_2_pert_buffer(k,i) = e_2_pert(k)
      coef_pert_buffer(k,i) = c_pert(k)
      sum_norm_pert(k) += c_pert(k) * c_pert(k)
      sum_e_2_pert(k) += e_2_pert(k)
      sum_H_pert_diag(k) +=  H_pert_diag(k)
    enddo
    
  enddo
  
end

