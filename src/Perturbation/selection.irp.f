subroutine fill_H_apply_buffer_selection(n_selected,det_buffer,e_2_pert_buffer,coef_pert_buffer, &
   N_st,Nint,iproc,select_max_out)
  use bitmasks
  implicit none
  BEGIN_DOC
  !  Fill the H_apply buffer with determiants for the selection
  END_DOC

  integer, intent(in)            :: n_selected, Nint, N_st, iproc
  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  double precision, intent(in)   :: e_2_pert_buffer(N_st,n_selected)
  double precision, intent(in)   :: coef_pert_buffer(N_st,n_selected)
  double precision, intent(inout):: select_max_out
  integer                        :: i,j,k,l
  integer                        :: new_size
  double precision               :: s, smin, smax
  logical                        :: is_selected
  PROVIDE H_apply_buffer_allocated N_int
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

    is_selected = .False.
    do j=1,N_st
      s = -e_2_pert_buffer(j,i)
      is_selected = s > selection_criterion*selection_criterion_factor .or. is_selected
      select_max_out = max(select_max_out,s)
    enddo
    
     

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
  !$OMP CRITICAL
  selection_criterion = max(selection_criterion,smax)
  selection_criterion_min = min(selection_criterion_min,smin)
  !$OMP END CRITICAL
end

 BEGIN_PROVIDER [ double precision, selection_criterion ]
&BEGIN_PROVIDER [ double precision, selection_criterion_min ]
&BEGIN_PROVIDER [ double precision, selection_criterion_factor ]
 implicit none
 BEGIN_DOC
 ! Threshold to select determinants. Set by selection routines.
 END_DOC
 selection_criterion =  .1d0 
 selection_criterion_factor = 0.01d0
 selection_criterion_min = selection_criterion

END_PROVIDER

subroutine remove_small_contributions
  implicit none
  BEGIN_DOC
!  Remove determinants with small contributions. N_states is assumed to be 
!  provided.
  END_DOC
  integer :: i,j,k, N_removed
  logical, allocatable :: keep(:)
  double precision :: i_H_psi_array(N_states)

  allocate (keep(N_det))
  call diagonalize_CI
  do i=1,N_det
    keep(i) = .True.
  enddo
  !$OMP PARALLEL DEFAULT(NONE) &
  !$OMP PRIVATE(i,j,i_H_psi_array) &
  !$OMP SHARED(k,psi_det_sorted,psi_coef_sorted,N_int,N_det,psi_det_size,N_states, &
  !$OMP   selection_criterion_min,keep,N_det_generators) &
  !$OMP REDUCTION(+:N_removed)
  !$OMP DO
  do i=2*N_det_generators+1, N_det
    call i_H_psi(psi_det_sorted(1,1,i),psi_det_sorted,psi_coef_sorted,N_int,min(N_det,2*N_det_generators),psi_det_size,N_states,i_H_psi_array)
    keep(i) = .False.
    do j=1,N_states
      keep(i) = keep(i) .or. (-(psi_coef_sorted(i,j)*i_H_psi_array(j)) > selection_criterion_min)
    enddo
  enddo
  !$OMP END DO
  !$OMP END PARALLEL
  N_removed = 0
  k = 0
  do i=1, N_det
    PROVIDE  psi_coef psi_det psi_det_sorted psi_coef_sorted
    if (keep(i)) then
      k += 1
      do j=1,N_int
         psi_det(j,1,k) = psi_det_sorted(j,1,i)
         psi_det(j,2,k) = psi_det_sorted(j,2,i)
      enddo
      do j=1,N_states
         psi_coef(k,j) = psi_coef_sorted(i,j)
      enddo
    else
      N_removed += 1
    endif
  enddo
  deallocate(keep)
  if (N_removed > 0) then
    N_det = N_det - N_removed
    SOFT_TOUCH N_det psi_det psi_coef 
    call write_int(output_dets,N_removed, 'Removed determinants')
  endif
end

subroutine make_s2_eigenfunction
  implicit none
  integer                        :: i,j,k
  integer                        :: smax, s
  integer(bit_kind), allocatable :: d(:,:,:), det_buffer(:,:,:)
  integer                        :: N_det_new
  integer, parameter             :: bufsze = 1000
  logical, external              :: is_in_wavefunction

!  !TODO DEBUG
!  do i=1,N_det
!   do j=i+1,N_det
!    s = 0
!    do k=1,N_int
!      if((psi_det(k,1,j) /= psi_det(k,1,i)).or. &
!         (psi_det(k,2,j) /= psi_det(k,2,i))) then
!         s=1
!         exit
!      endif
!    enddo
!    if ( s == 0 ) then
!      print *,  'Error0: det ', j, 'already in wf'
!      call debug_det(psi_det(1,1,j),N_int)
!      stop
!    endif
!   enddo
!  enddo
!  !TODO DEBUG

  allocate (d(N_int,2,1), det_buffer(N_int,2,bufsze) )
  smax = 1
  N_det_new = 0

  do i=1,N_occ_pattern
    call occ_pattern_to_dets_size(psi_occ_pattern(1,1,i),s,elec_alpha_num,N_int)
    if (s > smax) then
      deallocate(d)
      allocate ( d(N_int,2,s) )
    endif
    call occ_pattern_to_dets(psi_occ_pattern(1,1,i),d,s,elec_alpha_num,N_int)
    do j=1,s
      if (.not. is_in_wavefunction( d(1,1,j), N_int, N_det)) then
        N_det_new += 1
        do k=1,N_int
          det_buffer(k,1,N_det_new) = d(k,1,j)
          det_buffer(k,2,N_det_new) = d(k,2,j)
        enddo
        if (N_det_new == bufsze) then
          call fill_H_apply_buffer_no_selection(bufsze,det_buffer,N_int,0)
          N_det_new = 0
        endif
      endif
    enddo
  enddo

  if (N_det_new > 0) then
    call fill_H_apply_buffer_no_selection(N_det_new,det_buffer,N_int,0)
  endif

  deallocate(d,det_buffer)

  double precision :: c(N_states), e_2_pert(N_states), H_pert_diag(N_states)

  call copy_H_apply_buffer_to_wf
! do i=1,N_det
!   if (psi_coef(i,1) == 0.d0) then
!     call pt2_epstein_nesbet_2x2(psi_det(1,1,i),c,e_2_pert,H_pert_diag,N_int,N_det,N_states)
!     do k=1,N_states
!       psi_coef(i,k) = c(k)
!     enddo
!   endif
! enddo
  SOFT_TOUCH N_det psi_coef psi_det
!  !TODO DEBUG
!  do i=1,N_det
!   do j=i+1,N_det
!    s = 0
!    do k=1,N_int
!      if((psi_det(k,1,j) /= psi_det(k,1,i)).or. &
!         (psi_det(k,2,j) /= psi_det(k,2,i))) then
!         s=1
!         exit
!      endif
!    enddo
!    if ( s == 0 ) then
!      print *,  'Error : det ', j, 'already in wf at ', i
!      call debug_det(psi_det(1,1,j),N_int)
!      stop
!    endif
!   enddo
!  enddo
!  !TODO DEBUG
   call write_int(output_dets,N_det_new, 'Added deteminants for S^2')

end

