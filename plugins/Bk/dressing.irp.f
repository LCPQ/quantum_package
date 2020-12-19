 BEGIN_PROVIDER [ double precision, fock_diag_tmp_, (2,mo_tot_num+1,Nproc) ]
&BEGIN_PROVIDER [ integer, current_generator_, (Nproc) ]
  implicit none
  BEGIN_DOC
! Temporary arrays for speedup
  END_DOC
  current_generator_(:) = 0
 END_PROVIDER


subroutine dress_with_alpha_buffer(Nstates,Ndet,Nint,delta_ij_loc, i_gen, minilist, det_minilist, n_minilist, alpha, iproc)
 use bitmasks
 implicit none
  BEGIN_DOC
  !delta_ij_loc(:,:,1) : dressing column for H
  !delta_ij_loc(:,:,2) : dressing column for S2
  !minilist : indices of determinants connected to alpha ( in psi_det_sorted )
  !n_minilist : size of minilist
  !alpha : alpha determinant
  END_DOC
  integer, intent(in)             :: Nint, Ndet, Nstates, n_minilist, iproc, i_gen
  integer(bit_kind), intent(in)   :: alpha(Nint,2), det_minilist(Nint, 2, n_minilist)
  integer,intent(in)              :: minilist(n_minilist)
  double precision, intent(inout) :: delta_ij_loc(Nstates,Ndet,2)

  integer :: j, j_mini, i_state
  double precision :: c_alpha(N_states), h_alpha_alpha, hdress, sdress
  double precision :: i_h_alpha, i_s_alpha, alpha_h_psi(N_states)

  double precision, external :: diag_H_mat_elem_fock

  if(current_generator_(iproc) /= i_gen) then
    current_generator_(iproc) = i_gen
    call build_fock_tmp(fock_diag_tmp_(1,1,iproc),psi_det_generators(1,1,i_gen),N_int)
  end if

  h_alpha_alpha = diag_H_mat_elem_fock(psi_det_generators(1,1,i_gen),alpha,fock_diag_tmp_(1,1,iproc),N_int)
  call i_H_psi_minilist(alpha,det_minilist,minilist,n_minilist,psi_coef,N_int,n_minilist,size(psi_coef,1),N_states,alpha_h_psi)

  do i_state=1,N_states
    if (h_alpha_alpha - dress_e0_denominator(i_state) > 0.1d0 ) then
      c_alpha(i_state) = alpha_h_psi(i_state) / &
        (dress_e0_denominator(i_state) - h_alpha_alpha) 
    else
      c_alpha(i_state) = 0.d0
    endif
  enddo

  do j_mini=1,n_minilist
    j = minilist(j_mini)
    call i_H_j (det_minilist(1,1,j_mini),alpha,N_int,i_h_alpha)
    call get_s2(det_minilist(1,1,j_mini),alpha,N_int,i_s_alpha)
    do i_state=1,N_states
      hdress = c_alpha(i_state) * i_h_alpha
      sdress = c_alpha(i_state) * i_s_alpha
      delta_ij_loc(i_state,j,1) = delta_ij_loc(i_state,j,1) + hdress
      delta_ij_loc(i_state,j,2) = delta_ij_loc(i_state,j,2) + sdress
    enddo
  enddo


end subroutine



