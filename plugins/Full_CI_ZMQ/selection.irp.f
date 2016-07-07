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

  double precision               :: fock_diag_tmp(mo_tot_num)
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

      integer(bit_kind) :: psi_det_connected(N_int,2,psi_selectors_size)
      double precision  :: psi_coef_connected(psi_selectors_size,N_states)

      integer :: n_diff, N_det_connected
      N_det_connected = 0
      do l=1,N_det_selectors
!       TODO : provide not_psi_selectors
        n_diff = popcnt( iand(not(psi_selectors(1,1,l)), ion_det(1,1)) )     &
               + popcnt( iand(not(psi_selectors(1,2,l)), ion_det(1,2)) )
        do k=2,N_int
          n_diff = n_diff                                            &
              + popcnt( iand(not(psi_selectors(k,1,l)), ion_det(k,1)) )   &
              + popcnt( iand(not(psi_selectors(k,2,l)), ion_det(k,2)) )
        enddo
        if (n_diff <= 2) then 
          N_det_connected = N_det_connected+1
          do k=1,N_int
            psi_det_connected(k,1,N_det_connected) = psi_selectors(k,1,l)
            psi_det_connected(k,2,N_det_connected) = psi_selectors(k,2,l)
          enddo
          do k=1,N_states
            psi_coef_connected(N_det_connected,k) = psi_selectors_coef(l,k)
          enddo
        endif
      enddo
      if (N_det_connected == 0) then
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
        ! micro_list : et verifier sur la mouche si le det est connecte au passe

        logical, external :: is_in_wavefunction
        ! TODO : Check connected to ref
        if (.not. is_in_wavefunction(exc_det,N_int)) then
          ! Compute perturbative contribution and select determinant
          double precision :: i_H_psi_value(N_states)
          call i_H_psi(exc_det,psi_det_connected,psi_coef_connected,N_int,N_det_connected,psi_selectors_size,N_states,i_H_psi_value)

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
              integer :: rc
              rc = f77_zmq_send(zmq_socket_push, exc_det, msg_size,0)
              if (rc /= msg_size) then
                stop 'Unable to send selected determinant'
              endif
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

