


subroutine select_connected(i_generator,thr,E0,zmq_socket_push)
  use f77_zmq
  use bitmasks
  implicit none
  integer, intent(in)            :: i_generator
  double precision, intent(in)   :: thr
  double precision, intent(in)   :: E0(N_states)
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  BEGIN_DOC
! Select determinants connected to i_det by H
  END_DOC
  ASSERT (thr >= 0.d0)
  integer(bit_kind)              :: hole_mask(N_int,2), particle_mask(N_int,2)

  double precision               :: fock_diag_tmp(2,mo_tot_num+1)

  call build_fock_tmp(fock_diag_tmp,psi_det_generators(1,1,i_generator),N_int)
  integer :: k,l
  do l=1,N_generators_bitmask
    do k=1,N_int
      hole_mask(k,1) = iand(generators_bitmask(k,1,s_hole,l), psi_det_generators(k,1,i_generator))
      hole_mask(k,2) = iand(generators_bitmask(k,2,s_hole,l), psi_det_generators(k,2,i_generator)) 
      particle_mask(k,1) = iand(generators_bitmask(k,1,s_part,l), not(psi_det_generators(k,1,i_generator)) )
      particle_mask(k,2) = iand(generators_bitmask(k,2,s_part,l), not(psi_det_generators(k,2,i_generator)) )
    enddo
    call select_singles(i_generator,thr,hole_mask,particle_mask,fock_diag_tmp,E0,zmq_socket_push)
    call select_doubles(i_generator,thr,hole_mask,particle_mask,fock_diag_tmp,E0,zmq_socket_push)
  enddo
end

subroutine receive_selected_determinants()
  use f77_zmq
  use bitmasks
  implicit none
  BEGIN_DOC
! Receive via ZMQ the selected determinants
  END_DOC
  integer(ZMQ_PTR) :: zmq_socket_pull
  integer(ZMQ_PTR) :: new_zmq_pull_socket

  integer(bit_kind) :: received_det(N_int,2)
  integer :: msg_size, rc
  msg_size = bit_kind*N_int*2

  zmq_socket_pull = new_zmq_pull_socket()

  do while (f77_zmq_recv(zmq_socket_pull, received_det, msg_size, 0) == msg_size)
    call debug_det(received_det,N_int)
  end do
  call end_zmq_pull_socket(zmq_socket_pull)

end

subroutine select_singles(i_generator,thr,hole_mask,particle_mask,fock_diag_tmp,E0,zmq_socket_push)
  use f77_zmq
  use bitmasks
  implicit none
  BEGIN_DOC
! Select determinants connected to i_det by H
  END_DOC
  integer, intent(in)            :: i_generator
  double precision, intent(in)   :: thr
  double precision, intent(in)   :: fock_diag_tmp(mo_tot_num)
  integer(bit_kind), intent(in)  :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)   :: E0(N_states)
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  
  ASSERT (thr >= 0.d0)

  integer                        :: i,j,k,l

  integer :: msg_size
  msg_size = bit_kind*N_int*2

  ! Apply hole and particle masks
  ! -----------------------------

  integer(bit_kind)              :: hole(N_int,2), particle(N_int,2)
  do k=1,N_int
    hole    (k,1) = iand(psi_det_generators(k,1,i_generator), hole_mask(k,1))
    hole    (k,2) = iand(psi_det_generators(k,2,i_generator), hole_mask(k,2))
    particle(k,1) = iand(not(psi_det_generators(k,1,i_generator)), particle_mask(k,1))
    particle(k,2) = iand(not(psi_det_generators(k,2,i_generator)), particle_mask(k,2))
  enddo


  ! Create lists of holes and particles
  ! -----------------------------------

  integer                        :: N_holes(2), N_particles(2)
  integer                        :: hole_list(N_int*bit_kind_size,2)
  integer                        :: particle_list(N_int*bit_kind_size,2)

  call bitstring_to_list_ab(hole    , hole_list    , N_holes    , N_int)
  call bitstring_to_list_ab(particle, particle_list, N_particles, N_int)

  ! Create excited determinants
  ! ---------------------------

  integer                        :: ispin, other_spin
  integer(bit_kind)              :: exc_det(N_int,2), ion_det(N_int,2)

  do k=1,N_int
    exc_det(k,1) = psi_det_generators(k,1,i_generator)
    exc_det(k,2) = psi_det_generators(k,2,i_generator)
    ion_det(k,1) = psi_det_generators(k,1,i_generator)
    ion_det(k,2) = psi_det_generators(k,2,i_generator)
  enddo

  do ispin=1,2
    do k=1,N_int
      ion_det(k,ispin) = psi_det_generators(k,ispin,i_generator)
    enddo

    do i=1, N_holes(ispin)
      integer :: i_hole
      i_hole = hole_list(i,ispin)

      ! Apply the hole
      integer :: j_hole, k_hole
      k_hole = ishft(i_hole-1,-bit_kind_shift)+1        ! N_int
      j_hole = i_hole-ishft(k_hole-1,bit_kind_shift)-1  ! bit index
      ion_det(k_hole,ispin) = ibclr(psi_det_generators(k_hole,ispin,i_generator),j_hole)

      ! Create the mini wave function where <i|H|psi_mini> = <i|H|psi>
      ! --------------------------------------------------------------

!       integer(bit_kind) :: psi_det_connected(N_int,2,psi_selectors_size)
!       double precision  :: psi_coef_connected(psi_selectors_size,N_states)

 

      integer :: idx_microlist(N_det_selectors * 4), ptr_microlist(0:mo_tot_num * 2 + 1), N_microlist(0:mo_tot_num * 2)
      integer(bit_kind) :: microlist(N_int, 2, N_det_selectors * 4)
      double precision :: psi_coef_microlist(psi_selectors_size * 4, N_states)
      
      call create_microlist_single(psi_selectors, i_generator, N_det_selectors, ion_det, microlist, idx_microlist, N_microlist, ptr_microlist, N_int)
      
      do j=1, ptr_microlist(mo_tot_num * 2 + 1) - 1
        psi_coef_microlist(j,:) = psi_selectors_coef(idx_microlist(j),:)
      enddo
      
      if(ptr_microlist(mo_tot_num * 2 + 1) == 1) then
        cycle
      endif

      ! Create particles
      ! ----------------

      do j=1,N_particles(ispin)
        exc_det(k_hole,ispin) = ion_det(k_hole,ispin)

        integer :: i_particle
        i_particle = particle_list(j,ispin)

        ! Apply the particle
        integer :: j_particle, k_particle
        k_particle = ishft(i_particle-1,-bit_kind_shift)+1            ! N_int
        j_particle = i_particle-ishft(k_particle-1,bit_kind_shift)-1  ! bit index
        exc_det(k_particle,ispin) = ibset(ion_det(k_particle,ispin),j_particle)
        
        ! TODO
        
        logical, external :: is_in_wavefunction
        ! TODO : Check connected to ref
        if (.not. is_in_wavefunction(exc_det,N_int)) then
          ! Compute perturbative contribution and select determinant
          double precision :: i_H_psi_value(N_states), i_H_psi_value2(N_states)
          i_H_psi_value = 0d0
          i_H_psi_value2 = 0d0
          
          integer :: sporb
          
!           call i_H_psi(exc_det,psi_selectors,psi_selectors_coef,N_int,N_det_selectors,psi_selectors_size,N_states,i_H_psi_value)
! 
          sporb = i_particle + (ispin - 1) * mo_tot_num
          if(N_microlist(0) > 0) call i_H_psi(exc_det,microlist,psi_coef_microlist,N_int,N_microlist(0),psi_selectors_size*4,N_states,i_H_psi_value)
          if(N_microlist(sporb) > 0) call i_H_psi(exc_det,microlist(1,1,ptr_microlist(sporb)),psi_coef_microlist(ptr_microlist(sporb), 1),N_int,N_microlist(sporb),psi_selectors_size*4,N_states,i_H_psi_value2)
          i_H_psi_value(:) = i_H_psi_value(:) + i_H_psi_value2(:)
          double precision :: Hii, diag_H_mat_elem_fock
          Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),exc_det,fock_diag_tmp,N_int)

          double precision :: delta_E, e_pert
          do k=1,N_states
            if (i_H_psi_value(k) == 0.d0) cycle
            delta_E = E0(k) - Hii
            if (delta_E < 0.d0) then
              e_pert = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            else
              e_pert = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            endif
            
            
            if (dabs(e_pert) > thr) then
              call debug_det(exc_det, N_int)
!               integer :: rc
!               rc = f77_zmq_send(zmq_socket_push, exc_det, msg_size,0)
!               if (rc /= msg_size) then
!                 stop 'Unable to send selected determinant'
!               endif
            endif
            
            
          enddo
        endif

        ! Reset exc_det
        exc_det(k_particle,ispin) = psi_det_generators(k_particle,ispin,i_generator)
      enddo ! j

      ! Reset ion_det
      ion_det(k_hole,ispin) = psi_det_generators(k_hole,ispin,i_generator)
    enddo ! i
  enddo ! ispin
end



subroutine select_doubles(i_generator,thr,hole_mask,particle_mask,fock_diag_tmp,E0,zmq_socket_push)
  use f77_zmq
  use bitmasks
  implicit none
  BEGIN_DOC
! Select determinants connected to i_det by H
  END_DOC
  integer, intent(in)            :: i_generator
  double precision, intent(in)   :: thr
  double precision, intent(in)   :: fock_diag_tmp(mo_tot_num)
  integer(bit_kind), intent(in)  :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)   :: E0(N_states)
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  
  ASSERT (thr >= 0.d0)

  integer                        :: i,j,k,l,j1,j2,i1,i2,ib,jb

  integer :: msg_size
  msg_size = bit_kind*N_int*2

  ! Apply hole and particle masks
  ! -----------------------------

  integer(bit_kind)              :: hole(N_int,2), particle(N_int,2)
  do k=1,N_int
    hole    (k,1) = iand(psi_det_generators(k,1,i_generator), hole_mask(k,1))
    hole    (k,2) = iand(psi_det_generators(k,2,i_generator), hole_mask(k,2))
    particle(k,1) = iand(not(psi_det_generators(k,1,i_generator)), particle_mask(k,1))
    particle(k,2) = iand(not(psi_det_generators(k,2,i_generator)), particle_mask(k,2))
  enddo


  ! Create lists of holes and particles
  ! -----------------------------------

  integer                        :: N_holes(2), N_particles(2)
  integer                        :: hole_list(N_int*bit_kind_size,2)
  integer                        :: particle_list(N_int*bit_kind_size,2)

  call bitstring_to_list_ab(hole    , hole_list    , N_holes    , N_int)
  call bitstring_to_list_ab(particle, particle_list, N_particles, N_int)

  ! Create excited determinants
  ! ---------------------------

  integer                        :: ispin1, ispin2, other_spin
  integer(bit_kind)              :: exc_det(N_int,2), ion_det(N_int,2)

  do k=1,N_int
    exc_det(k,1) = psi_det_generators(k,1,i_generator)
    exc_det(k,2) = psi_det_generators(k,2,i_generator)
    ion_det(k,1) = psi_det_generators(k,1,i_generator)
    ion_det(k,2) = psi_det_generators(k,2,i_generator)
  enddo

  do ispin1=1,2
  do ispin2=1,ispin1
!     do k=1,N_int
!       ion_det(k,1) = psi_det_generators(k,1,i_generator)
!       ion_det(k,2) = psi_det_generators(k,2,i_generator)
!     enddo
    integer :: i_hole1, i_hole2, j_hole, k_hole
    do i1=1, N_holes(ispin1)
    ib = 1
    if(ispin1 == ispin2) ib = i1+1
    do i2=ib, N_holes(ispin2)
      ion_det(:,:) = psi_det_generators(:,:,i_generator)
      i_hole1 = hole_list(i1,ispin1)
      k_hole = ishft(i_hole1-1,-bit_kind_shift)+1        ! N_int
      j_hole = i_hole1-ishft(k_hole-1,bit_kind_shift)-1  ! bit index
      ion_det(k_hole,ispin1) = ibclr(ion_det(k_hole,ispin1),j_hole)
      
      i_hole2 = hole_list(i2,ispin2)
      k_hole = ishft(i_hole2-1,-bit_kind_shift)+1        ! N_int
      j_hole = i_hole2-ishft(k_hole-1,bit_kind_shift)-1  ! bit index
      ion_det(k_hole,ispin2) = ibclr(ion_det(k_hole,ispin2),j_hole)
      ! Create the mini wave function where <i|H|psi_mini> = <i|H|psi>
      ! --------------------------------------------------------------

!       integer(bit_kind) :: psi_det_connected(N_int,2,psi_selectors_size)
!       double precision  :: psi_coef_connected(psi_selectors_size,N_states)

 

      integer :: idx_microlist(N_det_selectors * 4), ptr_microlist(0:mo_tot_num * 2 + 1), N_microlist(0:mo_tot_num * 2)
      integer(bit_kind) :: microlist(N_int, 2, N_det_selectors * 4)
      double precision :: psi_coef_microlist(psi_selectors_size * 4, N_states)
      
      
      call create_microlist_double(psi_selectors, i_generator, N_det_selectors, ion_det, microlist, idx_microlist, N_microlist, ptr_microlist, N_int)
      
      do j=1, ptr_microlist(mo_tot_num * 2 + 1) - 1
        psi_coef_microlist(j,:) = psi_selectors_coef(idx_microlist(j),:) !!!!!! :
      enddo
      
      if(ptr_microlist(mo_tot_num * 2 + 1) == 1) then
        cycle
      endif
      ! Create particles
      ! ----------------

      do j1=1,N_particles(ispin1)
      jb = 1
      if(ispin1 == ispin2) jb = j1+1
      do j2=jb,N_particles(ispin2)
        exc_det = ion_det
        
        integer :: i_particle2, k_particle, j_particle
        i_particle2 = particle_list(j2, ispin2)
        ! Apply the particle
        k_particle = ishft(i_particle2-1,-bit_kind_shift)+1            ! N_int
        j_particle = i_particle2-ishft(k_particle-1,bit_kind_shift)-1  ! bit index
        exc_det(k_particle,ispin2) = ibset(exc_det(k_particle,ispin2),j_particle)
        
        integer :: i_particle1
        i_particle1 = particle_list(j1, ispin1)
        ! Apply the particle
        k_particle = ishft(i_particle1-1,-bit_kind_shift)+1            ! N_int
        j_particle = i_particle1-ishft(k_particle-1,bit_kind_shift)-1  ! bit index
        exc_det(k_particle,ispin1) = ibset(exc_det(k_particle,ispin1),j_particle)
        
        
        if(N_microlist(i_particle1 + (ispin1 - 1) * mo_tot_num) < N_microlist(i_particle2 + (ispin2 - 1) * mo_tot_num)) then
          sporb = i_particle1 + (ispin1 - 1) * mo_tot_num
        else
          sporb = i_particle2 + (ispin2 - 1) * mo_tot_num
        endif
        ! TODO
        
        logical, external :: is_in_wavefunction
        ! TODO : Check connected to ref
        if (.not. is_in_wavefunction(exc_det,N_int)) then
          ! Compute perturbative contribution and select determinant
          double precision :: i_H_psi_value(N_states), i_H_psi_value2(N_states)
          i_H_psi_value = 0d0
          i_H_psi_value2 = 0d0
          integer :: sporb
!           call i_H_psi(exc_det,psi_selectors,psi_selectors_coef,N_int,N_det_selectors,psi_selectors_size,N_states,i_H_psi_value)
          
          if(N_microlist(0) > 0) call i_H_psi(exc_det,microlist,psi_coef_microlist,N_int,N_microlist(0),psi_selectors_size*4,N_states,i_H_psi_value)
          if(N_microlist(sporb) > 0) call i_H_psi(exc_det,microlist(1,1,ptr_microlist(sporb)),psi_coef_microlist(ptr_microlist(sporb), 1),N_int,N_microlist(sporb),psi_selectors_size*4,N_states,i_H_psi_value2)
          i_H_psi_value = i_H_psi_value + i_H_psi_value2
          double precision :: Hii, diag_H_mat_elem_fock
          Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),exc_det,fock_diag_tmp,N_int)
          double precision :: delta_E, e_pert
          do k=1,N_states
            if (i_H_psi_value(k) == 0.d0) cycle
            delta_E = E0(k) - Hii
            if (delta_E < 0.d0) then
              e_pert = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            else
              e_pert = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            endif
            if (dabs(e_pert) > thr) then
!               call debug_det(exc_det, N_int)
              integer :: rc
              rc = f77_zmq_send(zmq_socket_push, exc_det, msg_size,0)
              if (rc /= msg_size) then
                stop 'Unable to send selected determinant'
              endif
            endif
          enddo
        endif

        ! Reset exc_det
!         exc_det(k_particle,ispin) = psi_det_generators(k_particle,ispin,i_generator)
      enddo ! j
      enddo
      ! Reset ion_det
!       ion_det(k_hole,ispin) = psi_det_generators(k_hole,ispin,i_generator)
    enddo ! i
    enddo
  enddo ! ispin
  enddo
end



subroutine create_microlist_single(minilist, i_cur, N_minilist, key_mask, microlist, idx_microlist, N_microlist, ptr_microlist, Nint)
  use bitmasks
  integer, intent(in) :: Nint, i_cur, N_minilist
  integer(bit_kind), intent(in) :: minilist(Nint,2,N_minilist), key_mask(Nint,2)
  
  integer, intent(out) :: N_microlist(0:mo_tot_num*2), ptr_microlist(0:mo_tot_num*2+1), idx_microlist(N_minilist*4)
  integer(bit_kind), intent(out) :: microlist(Nint,2,N_minilist*4)
  
  integer :: i,j,k,s,nt,n_element(2)
  integer :: list(Nint*bit_kind_size,2), cur_microlist(0:mo_tot_num*2+1)
  integer(bit_kind) :: key_mask_neg(Nint,2), mobileMask(Nint,2)
  integer :: mo_tot_num_2
  mo_tot_num_2 = mo_tot_num+mo_tot_num

  
  do i=1,Nint
    key_mask_neg(i,1) = not(key_mask(i,1))
    key_mask_neg(i,2) = not(key_mask(i,2))
  end do
  
  do i=0,mo_tot_num_2
    N_microlist(i) = 0
  enddo
  
  do i=1, N_minilist
    nt = 0
    do j=1,Nint
      mobileMask(j,1) = iand(key_mask_neg(j,1), minilist(j,1,i))
      mobileMask(j,2) = iand(key_mask_neg(j,2), minilist(j,2,i))
      nt += popcnt(mobileMask(j, 1)) + popcnt(mobileMask(j, 2))
    end do
    
    if(nt > 3) then !! TOO MANY DIFFERENCES
      continue
    else if(nt < 3) then
      if(i < i_cur .and. .false.) then !!!!!!!!!!!!!!!!!!!!! DESACTIVADO
        N_microlist = 0  !!!! PAST LINKED TO EVERYBODY!
        ptr_microlist = 1
        return  
      else !! FUTUR LINKED TO EVERYBODY
        N_microlist(0) = N_microlist(0) + 1
      endif
    else
      call bitstring_to_list(mobileMask(1,1), list(1,1), n_element(1), Nint)
      call bitstring_to_list(mobileMask(1,2), list(1,2), n_element(2), Nint)
      
      do s=1,2
        do j=1,n_element(s)
	  nt = list(j,s) + mo_tot_num * (s-1)
	  N_microlist(nt) = N_microlist(nt) + 1
        end do
      end do
    endif
  end do
  
  ptr_microlist(0) = 1
  do i=1,mo_tot_num_2+1
    ptr_microlist(i) = ptr_microlist(i-1) + N_microlist(i-1)
  end do

  do i=0,mo_tot_num_2+1
    cur_microlist(i) = ptr_microlist(i)
  end do
  
  
  do i=1, N_minilist
    do j=1,Nint
      mobileMask(j,1) = iand(key_mask_neg(j,1), minilist(j,1,i))
      mobileMask(j,2) = iand(key_mask_neg(j,2), minilist(j,2,i))
    end do
    
    call bitstring_to_list(mobileMask(1,1), list(1,1), n_element(1), Nint)
    call bitstring_to_list(mobileMask(1,2), list(1,2), n_element(2), Nint)

    
    if(n_element(1) + n_element(2) < 3) then
      idx_microlist(cur_microlist(0)) = i
      do k=1,Nint
        microlist(k,1,cur_microlist(0)) = minilist(k,1,i)
        microlist(k,2,cur_microlist(0)) = minilist(k,2,i)
      enddo
      cur_microlist(0) = cur_microlist(0) + 1
    else if(n_element(1) + n_element(2) == 3) then
      do s = 1, 2
        do j=1,n_element(s)
          nt = list(j,s) + mo_tot_num * (s-1)
          idx_microlist(cur_microlist(nt)) = i
          do k=1,Nint
            microlist(k,1,cur_microlist(nt)) = minilist(k,1,i)
            microlist(k,2,cur_microlist(nt)) = minilist(k,2,i)
          enddo
          cur_microlist(nt) = cur_microlist(nt) + 1
        end do
      end do
    end if
  end do
end subroutine


subroutine create_microlist_double(minilist, i_cur, N_minilist, key_mask, microlist, idx_microlist, N_microlist, ptr_microlist, Nint)
  use bitmasks
  integer, intent(in) :: Nint, i_cur, N_minilist
  integer(bit_kind), intent(in) :: minilist(Nint,2,N_minilist), key_mask(Nint,2)
  
  integer, intent(out) :: N_microlist(0:mo_tot_num*2), ptr_microlist(0:mo_tot_num*2+1), idx_microlist(N_minilist*4)
  integer(bit_kind), intent(out) :: microlist(Nint,2,N_minilist*4)
  
  integer :: i,j,k,s,nt,n_element(2)
  integer :: list(Nint*bit_kind_size,2), cur_microlist(0:mo_tot_num*2+1)
  integer(bit_kind) :: key_mask_neg(Nint,2), mobileMask(Nint,2)
  integer :: mo_tot_num_2
  mo_tot_num_2 = mo_tot_num+mo_tot_num

  
  do i=1,Nint
    key_mask_neg(i,1) = not(key_mask(i,1))
    key_mask_neg(i,2) = not(key_mask(i,2))
  end do
  
  do i=0,mo_tot_num_2
    N_microlist(i) = 0
  enddo
  
  do i=1, N_minilist
    nt = 0
    do j=1,Nint
      mobileMask(j,1) = iand(key_mask_neg(j,1), minilist(j,1,i))
      mobileMask(j,2) = iand(key_mask_neg(j,2), minilist(j,2,i))
      nt += popcnt(mobileMask(j, 1)) + popcnt(mobileMask(j, 2))
    end do
    
    if(nt > 4) then !! TOO MANY DIFFERENCES
      continue
    else if(nt < 3 .and. i < i_cur .and. .false.) then
      N_microlist = 0  !!!! PAST LINKED TO EVERYBODY!
      ptr_microlist = 1
      return  
    else if(nt == 4) then
      call bitstring_to_list(mobileMask(1,1), list(1,1), n_element(1), Nint)
      call bitstring_to_list(mobileMask(1,2), list(1,2), n_element(2), Nint)
      
      do s=1,2
        do j=1,n_element(s)
          nt = list(j,s) + mo_tot_num * (s-1)
          N_microlist(nt) = N_microlist(nt) + 1
        end do
      end do
    else
      N_microlist(0) = N_microlist(0) + 1
    endif
  end do
  
  ptr_microlist(0) = 1
  do i=1,mo_tot_num_2+1
    ptr_microlist(i) = ptr_microlist(i-1) + N_microlist(i-1)
  end do

  do i=0,mo_tot_num_2+1
    cur_microlist(i) = ptr_microlist(i)
  end do
  
  
  do i=1, N_minilist
    do j=1,Nint
      mobileMask(j,1) = iand(key_mask_neg(j,1), minilist(j,1,i))
      mobileMask(j,2) = iand(key_mask_neg(j,2), minilist(j,2,i))
    end do
    
    call bitstring_to_list(mobileMask(1,1), list(1,1), n_element(1), Nint)
    call bitstring_to_list(mobileMask(1,2), list(1,2), n_element(2), Nint)

    
    if(n_element(1) + n_element(2) < 4) then
      idx_microlist(cur_microlist(0)) = i
      do k=1,Nint
        microlist(k,1,cur_microlist(0)) = minilist(k,1,i)
        microlist(k,2,cur_microlist(0)) = minilist(k,2,i)
      enddo
      cur_microlist(0) = cur_microlist(0) + 1
    else if(n_element(1) + n_element(2) == 4) then
      do s = 1, 2
        do j=1,n_element(s)
          nt = list(j,s) + mo_tot_num * (s-1)
          idx_microlist(cur_microlist(nt)) = i
          do k=1,Nint
            microlist(k,1,cur_microlist(nt)) = minilist(k,1,i)
            microlist(k,2,cur_microlist(nt)) = minilist(k,2,i)
          enddo
          cur_microlist(nt) = cur_microlist(nt) + 1
        end do
      end do
    end if
  end do
end subroutine

