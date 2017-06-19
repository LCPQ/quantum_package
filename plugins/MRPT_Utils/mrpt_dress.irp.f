use omp_lib
use bitmasks

BEGIN_PROVIDER [ integer(omp_lock_kind), psi_ref_bis_lock, (psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Locks on ref determinants to fill delta_ij
 END_DOC
 integer :: i
 do i=1,psi_det_size
   call omp_init_lock( psi_ref_bis_lock(i) )
 enddo

END_PROVIDER


subroutine mrpt_dress(delta_ij_,  Ndet,i_generator,n_selected,det_buffer,Nint,iproc,key_mask)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in)            :: Ndet
  integer(bit_kind),intent(in)   :: key_mask(Nint, 2)
  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  double precision, intent(inout) :: delta_ij_(Ndet,Ndet,*)


  integer                        :: i,j,k,l
  integer                        :: idx_alpha(0:psi_det_size)
  integer                        :: degree_alpha(psi_det_size)
  logical                        :: fullMatch
 
  double precision               :: delta_e_inv_array(psi_det_size,N_states)
  double precision               :: hij_array(psi_det_size)

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq

  double precision               :: hialpha,hij
  integer                        :: i_state, i_alpha
  
  integer(bit_kind),allocatable  :: miniList(:,:,:)
  integer,allocatable            :: idx_miniList(:)
  integer                        :: N_miniList, leng
  double precision               :: delta_e(N_states),hij_tmp
  integer                        :: index_i,index_j
  double precision :: phase_array(N_det),phase
  integer                        :: exc(0:2,2,2),degree
  
  
  leng = max(N_det_generators, N_det)
  allocate(miniList(Nint, 2, leng), idx_miniList(leng))
  
  !create_minilist_find_previous(key_mask, fullList, miniList, N_fullList, N_miniList, fullMatch, Nint)
  call create_minilist_find_previous(key_mask, psi_det_generators, miniList, i_generator-1, N_miniList, fullMatch, Nint)
  
  if(fullMatch) then
    return
  end if
  
  
  call find_connections_previous(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_minilist)

  if(N_tq > 0) then
    call create_minilist(key_mask, psi_ref, miniList, idx_miniList, N_det, N_minilist, Nint)
  end if
  
  
  do i_alpha=1,N_tq
    call get_excitation_degree_vector(miniList,tq(1,1,i_alpha),degree_alpha,Nint,N_minilist,idx_alpha)
    
    do j=1,idx_alpha(0)
      idx_alpha(j) = idx_miniList(idx_alpha(j))
    enddo
     
!   double precision :: ihpsi0,coef_pert
!   ihpsi0 = 0.d0
!   coef_pert = 0.d0
    phase_array  =0.d0
    do i = 1,idx_alpha(0)
      index_i = idx_alpha(i)
      call i_h_j(tq(1,1,i_alpha),psi_ref(1,1,index_i),Nint,hialpha)
      double  precision :: coef_array(N_states)
      do i_state = 1, N_states
       coef_array(i_state) = psi_coef(index_i,i_state)
      enddo
      call get_delta_e_dyall(psi_ref(1,1,index_i),tq(1,1,i_alpha),delta_e)
!     call get_delta_e_dyall_general_mp(psi_ref(1,1,index_i),tq(1,1,i_alpha),delta_e)
      hij_array(index_i) = hialpha
      call get_excitation(psi_ref(1,1,index_i),tq(1,1,i_alpha),exc,degree,phase,N_int)
!     phase_array(index_i) = phase
      do i_state = 1,N_states
       delta_e_inv_array(index_i,i_state) = 1.d0/delta_e(i_state)
      enddo
    enddo
    
    do i=1,idx_alpha(0)
      index_i = idx_alpha(i)
      hij_tmp = hij_array(index_i)
      call omp_set_lock( psi_ref_bis_lock(index_i) )
      do j = 1, idx_alpha(0)
       index_j = idx_alpha(j)
!      call get_excitation(psi_ref(1,1,index_i),psi_ref(1,1,index_i),exc,degree,phase,N_int)
!      if(index_j.ne.index_i)then
!       if(phase_array(index_j) * phase_array(index_i) .ne. phase)then
!        print*, phase_array(index_j) , phase_array(index_i) ,phase
!        call debug_det(psi_ref(1,1,index_i),N_int)
!        call debug_det(psi_ref(1,1,index_j),N_int)
!        call debug_det(tq(1,1,i_alpha),N_int)
!        stop
!       endif
!      endif
       do i_state=1,N_states
! standard dressing first order
         delta_ij_(index_i,index_j,i_state) += hij_array(index_j) * hij_tmp * delta_e_inv_array(index_j,i_state)
       enddo
      enddo
      call omp_unset_lock( psi_ref_bis_lock(index_i))
    enddo
  enddo
  deallocate(miniList, idx_miniList)
end



 BEGIN_PROVIDER [ integer(bit_kind), gen_det_ref_sorted,  (N_int,2,N_det_generators,2) ]
&BEGIN_PROVIDER [ integer, gen_det_ref_shortcut, (0:N_det_generators,2) ]
&BEGIN_PROVIDER [ integer, gen_det_ref_version, (N_int, N_det_generators,2) ]
&BEGIN_PROVIDER [ integer, gen_det_ref_idx, (N_det_generators,2) ]
  gen_det_ref_sorted(:,:,:,1) = psi_det_generators(:,:,:N_det_generators)
  gen_det_ref_sorted(:,:,:,2) = psi_det_generators(:,:,:N_det_generators)
  call sort_dets_ab_v(gen_det_ref_sorted(:,:,:,1), gen_det_ref_idx(:,1), gen_det_ref_shortcut(0:,1), gen_det_ref_version(:,:,1), N_det_generators, N_int)
  call sort_dets_ba_v(gen_det_ref_sorted(:,:,:,2), gen_det_ref_idx(:,2), gen_det_ref_shortcut(0:,2), gen_det_ref_version(:,:,2), N_det_generators, N_int)
END_PROVIDER


subroutine find_connections_previous(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_miniList)

 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,m
  logical                        :: is_in_wavefunction
  integer                        :: degree(psi_det_size)
  integer                        :: idx(0:psi_det_size)
  logical                        :: good

  integer(bit_kind), intent(out) :: tq(Nint,2,n_selected)
  integer, intent(out)           :: N_tq
  
  
  integer                        :: nt,ni
  logical, external              :: is_connected_to
  
  
  integer(bit_kind),intent(in)  :: miniList(Nint,2,N_det_generators)
  integer,intent(in)            :: N_miniList

  
  
  N_tq = 0
  
  
  i_loop : do i=1,N_selected
    if(is_connected_to(det_buffer(1,1,i), miniList, Nint, N_miniList)) then
      cycle
    end if

    if (.not. is_in_wavefunction(det_buffer(1,1,i),Nint,N_det)) then
      N_tq += 1
      do k=1,N_int
        tq(k,1,N_tq) = det_buffer(k,1,i)
        tq(k,2,N_tq) = det_buffer(k,2,i)
      enddo
    endif
  enddo i_loop
end







