subroutine fill_H_apply_buffer_selection(n_selected,det_buffer,e_2_pert_buffer,coef_pert_buffer,N_st,Nint,iproc)
  use bitmasks
  implicit none
  BEGIN_DOC
  !  Fill the H_apply buffer with determiants for the selection
  END_DOC

  integer, intent(in)            :: n_selected, Nint, N_st, iproc
  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  double precision, intent(in)   :: e_2_pert_buffer(N_st,n_selected)
  double precision, intent(in)   :: coef_pert_buffer(N_st,n_selected)
  integer                        :: i,j,k,l
  integer                        :: new_size
  double precision               :: s, smin, smax
  logical                        :: is_selected
  PROVIDE H_apply_buffer_allocated
  ASSERT (Nint > 0)
  ASSERT (N_int == N_int)
  ASSERT (N_selected >= 0)
  smax = selection_criterion
  smin = selection_criterion_min
  new_size = H_apply_buffer(iproc)%N_det + n_selected
  
  if (new_size > h_apply_buffer(iproc)%sze) then
    call resize_h_apply_buffer(max(h_apply_buffer(iproc)%sze*2,new_size),iproc)
  endif
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(h_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(h_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
  l=H_apply_buffer(iproc)%N_det
  do i=1,n_selected

    s = 0.d0
    do j=1,N_st
      s -= e_2_pert_buffer(j,i)
    enddo
    ASSERT (s>=-1.d-8)
    
    is_selected = s > selection_criterion * selection_criterion_factor
     
    if (is_selected) then
      l = l+1
      do j=1,N_int
        h_apply_buffer(iproc)%det(j,1,l) = det_buffer(j,1,i)
        h_apply_buffer(iproc)%det(j,2,l) = det_buffer(j,2,i)
      enddo
      do j=1,N_st
        H_apply_buffer(iproc)%e2(l,j) = e_2_pert_buffer(j,i)
        H_apply_buffer(iproc)%coef(l,j) = coef_pert_buffer(j,i)
      enddo
      ASSERT (sum(popcnt(h_apply_buffer(iproc)%det(:,1,l)) )== elec_alpha_num)
      ASSERT (sum(popcnt(h_apply_buffer(iproc)%det(:,2,l))) == elec_beta_num)
      smax = max(s,smax)
      smin = min(selection_criterion_min,smin)
    endif
  enddo
  H_apply_buffer(iproc)%N_det = l
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(h_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(h_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
  selection_criterion = smax
  selection_criterion_min = smin
end

 BEGIN_PROVIDER [ double precision, selection_criterion ]
&BEGIN_PROVIDER [ double precision, selection_criterion_min ]
&BEGIN_PROVIDER [ double precision, selection_criterion_factor ]
 implicit none
 BEGIN_DOC
 ! Threshold to select determinants. Set by selection routines.
 END_DOC
 selection_criterion = .1d0 
 selection_criterion_factor = 0.01d0
 selection_criterion_min = selection_criterion

END_PROVIDER

subroutine remove_small_contributions
  implicit none
  BEGIN_DOC
!  Remove determinants with small contributions
  END_DOC
  integer :: i,j,k, N_removed
  logical keep
  N_removed = 0
  do i=N_det,1,-1
    keep = .False.
    do j=1,N_states
      keep = keep .or. (dabs(psi_coef(i,j)) > selection_criterion_min)
    enddo
    if (.not.keep) then
      do k=i+1,N_det
        do j=1,N_int
           psi_det(j,1,k-1) = psi_det(j,1,k)
           psi_det(j,2,k-1) = psi_det(j,2,k)
        enddo
      enddo
      do j=1,N_states
        do k=i+1,N_det
           psi_coef(k-1,j) = psi_coef(k,j)
        enddo
      enddo
      N_removed += 1
    endif
  enddo
  if (N_removed > 0) then
    N_det -= N_removed
    call write_int(output_dets,N_removed, 'Removed determinants')
  endif
end
