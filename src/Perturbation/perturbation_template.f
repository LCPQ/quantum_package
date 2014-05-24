BEGIN_SHELL [ /usr/bin/env python ]
import perturbation
END_SHELL

subroutine perturb_buffer_$PERT(buffer,buffer_size,e_2_pert_buffer,coef_pert_buffer,sum_e_2_pert,sum_norm_pert,sum_H_pert_diag,N_st,Nint)
  implicit none
  BEGIN_DOC
  !  Applly pertubration ``$PERT`` to the buffer of determinants generated in the H_apply
  ! routine.
  END_DOC
  
  integer, intent(in)            :: Nint, N_st, buffer_size
  integer(bit_kind), intent(in)  :: buffer(Nint,2,buffer_size)
  double precision, intent(inout) :: sum_norm_pert(N_st),sum_e_2_pert(N_st)
  double precision, intent(inout) :: coef_pert_buffer(N_st,buffer_size),e_2_pert_buffer(N_st,buffer_size),sum_H_pert_diag(N_st)
  double precision               :: c_pert(N_st), e_2_pert(N_st), H_pert_diag
  integer                        :: i,k, c_ref
  integer                        :: connected_to_ref
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (buffer_size >= 0)
  ASSERT (minval(sum_norm_pert) >= 0.d0)
  ASSERT (N_st > 0)
  do i = 1,buffer_size

    c_ref = connected_to_ref(buffer(1,1,i),psi_det,Nint,N_det_generators,N_det,h_apply_threshold)

    if (c_ref /= 0) then
      cycle
    endif
    
    call pt2_$PERT(buffer(1,1,i),         &
        c_pert,e_2_pert,H_pert_diag,Nint,n_det_ref,n_st)

    do k = 1,N_st
      e_2_pert_buffer(k,i) = e_2_pert(k)
      coef_pert_buffer(k,i) = c_pert(k)
      sum_norm_pert(k) += c_pert(k) * c_pert(k)
      sum_e_2_pert(k) += e_2_pert(k)
      sum_H_pert_diag(k) += c_pert(k) * c_pert(k) * H_pert_diag
    enddo
    
  enddo
  
end

