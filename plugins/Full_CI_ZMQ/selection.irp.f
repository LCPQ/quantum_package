

subroutine selection_slave(thread,iproc)
  use f77_zmq
  use selection_types
  implicit none
  
  integer,  intent(in)            :: thread, iproc
  integer                        :: rc, i

  integer                        :: worker_id, task_id(100), ctask, ltask
  character*(512)                :: task 
 
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
 
  integer(ZMQ_PTR), external     :: new_zmq_push_socket 
  integer(ZMQ_PTR)               :: zmq_socket_push 
  
  type(selection_buffer) :: buf
  logical :: done
  double precision :: pt2(N_states)
  
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)
  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  
  buf%N = 0
  ctask = 1
  pt2 = 0d0
  
  do 
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id(ctask), task)
    done = task_id(ctask) == 0
    if (.not. done) then
      integer :: i_generator, N
      read (task,*) i_generator, N
      if(buf%N == 0) then
        call create_selection_buffer(N, N*2, buf)
      else
        if(N /= buf%N) stop "N changed... wtf man??"
      end if
      call select_connected(i_generator,ci_electronic_energy,pt2,buf) !! ci_electronic_energy ??
    end if
    
    if(done) ctask = ctask - 1
    
    if(done .or. ctask == size(task_id)) then 
      if(ctask > 0 .and. buf%N /= 0) then
        call push_selection_results(zmq_socket_push, pt2, buf, task_id(1), ctask)
        pt2 = 0d0
        buf%cur = 0
      end if
      
      do i=1, ctask
         call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id(i))
      end do
      ctask = 0
    end if
    
    if(done) exit
    ctask = ctask + 1
  end do
  
  call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)
  
end subroutine


subroutine push_selection_results(zmq_socket_push, pt2, b, task_id, ntask)
  use f77_zmq
  use selection_types
  implicit none
  
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push 
  double precision, intent(in)   :: pt2(N_states)
  type(selection_buffer), intent(inout) :: b
  integer, intent(in) :: ntask, task_id(*)
  integer :: rc
  
  call sort_selection_buffer(b)
  
  rc = f77_zmq_send( zmq_socket_push, b%cur, 4, ZMQ_SNDMORE)
  
  rc = f77_zmq_send( zmq_socket_push, pt2, 8*N_states, ZMQ_SNDMORE)
  
  rc = f77_zmq_send( zmq_socket_push, b%val(1), 8*b%cur, ZMQ_SNDMORE) 
  
  rc = f77_zmq_send( zmq_socket_push, b%det(1,1,1), bit_kind*N_int*2*b%cur, ZMQ_SNDMORE) 
  
  rc = f77_zmq_send( zmq_socket_push, ntask, 4, ZMQ_SNDMORE) 
  
  rc = f77_zmq_send( zmq_socket_push, task_id(1), ntask*4, 0) 
  
end subroutine


subroutine pull_selection_results(zmq_socket_pull, pt2, val, det, N, task_id, ntask)
  use f77_zmq
  use selection_types
  implicit none
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull 
  double precision, intent(inout) :: pt2(N_states)
  double precision, intent(out) :: val(*)
  integer(bit_kind), intent(out) :: det(N_int, 2, *)
  integer, intent(out) :: N, ntask, task_id(*)
  integer :: rc, rn, i
  
  rc = f77_zmq_recv( zmq_socket_pull, N, 4, ZMQ_SNDMORE) 
  
  rc = f77_zmq_recv( zmq_socket_pull, pt2, N_states*8, ZMQ_SNDMORE) 
  
  rc = f77_zmq_recv( zmq_socket_pull, val(1), 8*N, ZMQ_SNDMORE) 
  
  rc = f77_zmq_recv( zmq_socket_pull, det(1,1,1), bit_kind*N_int*2*N, ZMQ_SNDMORE) 
  
  rc = f77_zmq_recv( zmq_socket_pull, ntask, 4, ZMQ_SNDMORE) 
  
  rc = f77_zmq_recv( zmq_socket_pull, task_id(1), ntask*4, 0) 
end subroutine


subroutine select_connected(i_generator,E0,pt2,b)
  use f77_zmq
  use bitmasks
  use selection_types
  implicit none
  integer, intent(in)            :: i_generator
  type(selection_buffer), intent(inout) :: b
  double precision, intent(inout)  :: pt2(N_states)
  integer :: k,l
  double precision, intent(in)   :: E0(N_states)

  integer(bit_kind)              :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision               :: fock_diag_tmp(2,mo_tot_num+1)
  
  
  call build_fock_tmp(fock_diag_tmp,psi_det_generators(1,1,i_generator),N_int)
  
  do l=1,N_generators_bitmask
    do k=1,N_int
      hole_mask(k,1) = iand(generators_bitmask(k,1,s_hole,l), psi_det_generators(k,1,i_generator))
      hole_mask(k,2) = iand(generators_bitmask(k,2,s_hole,l), psi_det_generators(k,2,i_generator)) 
      particle_mask(k,1) = iand(generators_bitmask(k,1,s_part,l), not(psi_det_generators(k,1,i_generator)) )
      particle_mask(k,2) = iand(generators_bitmask(k,2,s_part,l), not(psi_det_generators(k,2,i_generator)) )
      
      hole_mask(k,1) = ior(generators_bitmask(k,1,s_hole,l), generators_bitmask(k,1,s_part,l))
      hole_mask(k,2) = ior(generators_bitmask(k,2,s_hole,l), generators_bitmask(k,2,s_part,l))
      particle_mask(k,:) = hole_mask(k,:)
    enddo
    
    call select_singles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
    call select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
  enddo
end

  
subroutine create_selection_buffer(N, siz, res)
  use selection_types
  implicit none
  
  integer, intent(in) :: N, siz
  type(selection_buffer), intent(out) :: res
  
  allocate(res%det(N_int, 2, siz), res%val(siz))
  
  res%val = 0d0
  res%det = 0_8
  res%N = N
  res%mini = 0d0
  res%cur = 0
end subroutine


subroutine add_to_selection_buffer(b, det, val)
  use selection_types
  implicit none
  
  type(selection_buffer), intent(inout) :: b
  integer(bit_kind), intent(in) :: det(N_int, 2)
  double precision, intent(in) :: val
  integer :: i
  
  if(dabs(val) >= b%mini) then
    b%cur += 1
    b%det(:,:,b%cur) = det(:,:)
    b%val(b%cur) = val
    if(b%cur == size(b%val)) then
      call sort_selection_buffer(b)
    end if
  end if
end subroutine


subroutine sort_selection_buffer(b)
  use selection_types
  implicit none
  
  type(selection_buffer), intent(inout) :: b
  double precision, allocatable :: vals(:), absval(:)
  integer, allocatable :: iorder(:)
  integer(bit_kind), allocatable :: detmp(:,:,:)
  integer :: i, nmwen
  logical, external :: detEq
  nmwen = min(b%N, b%cur)
  
  
  allocate(iorder(b%cur), detmp(N_int, 2, nmwen), absval(b%cur), vals(nmwen))
  absval = -dabs(b%val(:b%cur))
  do i=1,b%cur
    iorder(i) = i
  end do
  call dsort(absval, iorder, b%cur)
  
  do i=1, nmwen
    detmp(:,:,i) = b%det(:,:,iorder(i))
    vals(i) = b%val(iorder(i))
  end do
  b%det(:,:,:nmwen) = detmp(:,:,:)
  b%det(:,:,nmwen+1:) = 0_bit_kind
  b%val(:nmwen) = vals(:)
  b%val(nmwen+1:) = 0d0
  b%mini = dabs(b%val(b%N))
  b%cur = nmwen
end subroutine


subroutine selection_collector(b, pt2)
  use f77_zmq
  use selection_types
  use bitmasks
  implicit none
  
  
  type(selection_buffer), intent(inout) :: b
  double precision, intent(out)       :: pt2(N_states)
  double precision                   :: pt2_mwen(N_states)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
   
  integer(ZMQ_PTR), external     :: new_zmq_pull_socket 
  integer(ZMQ_PTR)               :: zmq_socket_pull 

  integer :: msg_size, rc, more
  integer :: acc, i, j, robin, N, ntask
  double precision, allocatable :: val(:)
  integer(bit_kind), allocatable :: det(:,:,:)
  integer, allocatable :: task_id(:)
  
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket() 
  zmq_socket_pull = new_zmq_pull_socket() 
  allocate(val(b%N), det(N_int, 2, b%N), task_id(N_det))
  pt2 = 0d0
  more = 1 
  do while (more == 1) 
    call pull_selection_results(zmq_socket_pull, pt2_mwen, val(1), det(1,1,1), N, task_id, ntask)
    pt2 += pt2_mwen
    do i=1, N
      call add_to_selection_buffer(b, det(1,1,i), val(i))
    end do
      
    do i=1, ntask
      if (task_id(i) /= 0) then 
        call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id(i),more) 
      endif
    end do
  end do

  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_pull_socket(zmq_socket_pull)
  call sort_selection_buffer(b)
end subroutine
  

subroutine select_singles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,buf)
  use f77_zmq
  use bitmasks
  use selection_types
  implicit none
  BEGIN_DOC
! Select determinants connected to i_det by H
  END_DOC
  integer, intent(in)            :: i_generator
  double precision, intent(in)   :: fock_diag_tmp(mo_tot_num)
  double precision, intent(inout)   :: pt2(N_states)
  integer(bit_kind), intent(in)  :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)   :: E0(N_states)
  type(selection_buffer), intent(inout) :: buf
  
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
  
  
      ! Create the mini wave function where <i|H|psi_mini> = <i|H|psi>
      ! --------------------------------------------------------------

!       integer(bit_kind) :: psi_det_connected(N_int,2,psi_selectors_size)
!       double precision  :: psi_coef_connected(psi_selectors_size,N_states)
  
  integer :: ptr_microlist(0:mo_tot_num * 2 + 1), N_microlist(0:mo_tot_num * 2)
  integer, allocatable :: idx_microlist(:)
  integer(bit_kind), allocatable :: microlist(:, :, :)
  double precision, allocatable :: psi_coef_microlist(:,:)
  
  allocate(microlist(N_int, 2, N_det_selectors * 3), psi_coef_microlist(psi_selectors_size * 3, N_states), idx_microlist(N_det_selectors * 3))
      
  do ispin=1,2
!     do k=1,N_int
!       ion_det(k,ispin) = psi_det_generators(k,ispin,i_generator)
!     enddo
    

    do i=1, N_holes(ispin)
      ion_det(:,:) = psi_det_generators(:,:,i_generator)
      integer :: i_hole
      i_hole = hole_list(i,ispin)

      ! Apply the hole
      integer :: j_hole, k_hole
      k_hole = ishft(i_hole-1,-bit_kind_shift)+1        ! N_int
      j_hole = i_hole-ishft(k_hole-1,bit_kind_shift)-1  ! bit index
!       ion_det(k_hole,ispin) = ibclr(psi_det_generators(k_hole,ispin,i_generator),j_hole)
      ion_det(k_hole,ispin) = ibclr(ion_det(k_hole,ispin),j_hole)

      
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
!         exc_det(k_hole,ispin) = ion_det(k_hole,ispin)
        exc_det(:,:) = ion_det(:,:)
        
        integer :: i_particle
        i_particle = particle_list(j,ispin)

        ! Apply the particle
        integer :: j_particle, k_particle
        k_particle = ishft(i_particle-1,-bit_kind_shift)+1            ! N_int
        j_particle = i_particle-ishft(k_particle-1,bit_kind_shift)-1  ! bit index
!         exc_det(k_particle,ispin) = ibset(ion_det(k_particle,ispin),j_particle)
        exc_det(k_particle,ispin) = ibset(exc_det(k_particle,ispin),j_particle)
        
        ! TODO
        
        logical, external :: is_in_wavefunction
        logical :: nok
        ! TODO : Check connected to ref
        if (.not. is_in_wavefunction(exc_det,N_int)) then
          ! Compute perturbative contribution and select determinant
          double precision :: i_H_psi_value(N_states), i_H_psi_value2(N_states)
          double precision :: i_H_full(N_states)
          i_H_psi_value = 0d0
          i_H_psi_value2 = 0d0
          i_H_full = 0d0
          integer :: sporb
          
!            call i_H_psi(exc_det,psi_selectors,psi_selectors_coef,N_int,N_det_selectors,psi_selectors_size,N_states,i_H_full)
!               
          nok = .false.
          sporb = i_particle + (ispin - 1) * mo_tot_num                                              
! ! !           subroutine check_past(det, list, idx, N, cur, ok, Nint)
          if(N_microlist(sporb) > 0) call check_past(exc_det, microlist(1,1,ptr_microlist(sporb)), idx_microlist(ptr_microlist(sporb)), N_microlist(sporb), i_generator,nok, N_int)
          if(nok) cycle
!         
          if(N_microlist(0) > 0) call i_H_psi(exc_det,microlist,psi_coef_microlist,N_int,N_microlist(0),psi_selectors_size*3,N_states,i_H_psi_value)
          if(N_microlist(sporb) > 0) call i_H_psi(exc_det,microlist(1,1,ptr_microlist(sporb)),psi_coef_microlist(ptr_microlist(sporb), 1),N_int,N_microlist(sporb),psi_selectors_size*3,N_states,i_H_psi_value2)
          
          
          i_H_psi_value(:) = i_H_psi_value(:) + i_H_psi_value2(:)
          double precision :: Hii, diag_H_mat_elem_fock
          Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),exc_det,fock_diag_tmp,N_int)
          
          double precision :: delta_E, e_pert(N_states), e_pertm
          e_pert(:) = 0d0
          e_pertm = 0d0
          
          do k=1,N_states
!             if(dabs(1d0 - i_H_psi_value(k)/i_H_full(k)) > 1d-6) then
!               stop "PAS BON, PAS BOOOOON!! (single)"
!             endif
            if (i_H_psi_value(k) == 0.d0) cycle
            delta_E = E0(k) - Hii
            if (delta_E < 0.d0) then
              e_pert(k) = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            else
              e_pert(k) = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            endif
            if(dabs(e_pert(k)) > dabs(e_pertm)) e_pertm = e_pert(k)
            pt2(k) += e_pert(k)
          enddo
          call add_to_selection_buffer(buf, exc_det, e_pertm)
        endif

        ! Reset exc_det
        exc_det(k_particle,ispin) = psi_det_generators(k_particle,ispin,i_generator)
      enddo ! j

      ! Reset ion_det
      ion_det(k_hole,ispin) = psi_det_generators(k_hole,ispin,i_generator)
    enddo ! i
  enddo ! ispin
end



subroutine select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,buf)
  use f77_zmq
  use bitmasks
  use selection_types
  implicit none
  BEGIN_DOC
! Select determinants connected to i_det by H
  END_DOC
  integer, intent(in)            :: i_generator
  double precision, intent(in)   :: fock_diag_tmp(mo_tot_num)
  double precision, intent(inout)   :: pt2(N_states)
  integer(bit_kind), intent(in)  :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision, intent(in)   :: E0(N_states)
  type(selection_buffer), intent(inout) :: buf
  logical :: isinwf(mo_tot_num*2, mo_tot_num*2)
  double precision :: d0s(mo_tot_num, mo_tot_num, N_states)
  d0s = 0d0
!   double precision, save :: d0 = 0d0
!   double precision, save :: d1 = 0d0
!   double precision, save :: d2 = 0d0
  
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
  
  
  integer :: ptr_microlist(0:mo_tot_num * 2 + 1), N_microlist(0:mo_tot_num * 2)
  double precision, allocatable :: psi_coef_microlist(:,:)
  
  integer :: ptr_tmicrolist(0:mo_tot_num * 2 + 1), N_tmicrolist(0:mo_tot_num * 2)
  double precision, allocatable :: psi_coef_tmicrolist(:,:)
   
  integer, allocatable :: idx_tmicrolist(:), idx_microlist(:)
  integer(bit_kind), allocatable :: microlist(:,:,:), tmicrolist(:,:,:)
  
  integer :: ptr_futur_microlist(0:mo_tot_num * 2 + 1), ptr_futur_tmicrolist(0:mo_tot_num * 2 + 1)
  integer :: N_futur_microlist(0:mo_tot_num * 2), N_futur_tmicrolist(0:mo_tot_num * 2)
  logical :: pastlink
  
  allocate(idx_tmicrolist(N_det_selectors * 3), idx_microlist(N_det_selectors * 4))
  allocate(microlist(N_int, 2, N_det_selectors * 4), tmicrolist(N_int, 2, N_det_selectors * 3))
  allocate(psi_coef_tmicrolist(psi_selectors_size * 3, N_states), psi_coef_microlist(psi_selectors_size * 4, N_states))
  
  do k=1,N_int
    exc_det(k,1) = psi_det_generators(k,1,i_generator)
    exc_det(k,2) = psi_det_generators(k,2,i_generator)
    ion_det(k,1) = psi_det_generators(k,1,i_generator)
    ion_det(k,2) = psi_det_generators(k,2,i_generator)
  enddo

  do ispin1=1,2
  do ispin2=1,ispin1
    integer :: i_hole1, i_hole2, j_hole, k_hole
    do i1=1, N_holes(ispin1)
    ib = 1
    if(ispin1 == ispin2) ib = i1+1
    do i2=ib, N_holes(ispin2)
      ion_det(:,:) = psi_det_generators(:,:,i_generator)
!       call set_hole(ion_det, hole_list(i1,ispin1), ispin1, hole_list(i1,ispin1), ispin1, Nint)
      i_hole1 = hole_list(i1,ispin1)
      k_hole = ishft(i_hole1-1,-bit_kind_shift)+1        ! N_int
      j_hole = i_hole1-ishft(k_hole-1,bit_kind_shift)-1  ! bit index
      ion_det(k_hole,ispin1) = ibclr(ion_det(k_hole,ispin1),j_hole)
      
      i_hole2 = hole_list(i2,ispin2)
      k_hole = ishft(i_hole2-1,-bit_kind_shift)+1        ! N_int
      j_hole = i_hole2-ishft(k_hole-1,bit_kind_shift)-1  ! bit index
      ion_det(k_hole,ispin2) = ibclr(ion_det(k_hole,ispin2),j_hole)
      
      call create_microlist_double(psi_selectors, i_generator, N_det_selectors, ion_det, &
            microlist, idx_microlist, N_microlist, ptr_microlist, &
            tmicrolist, idx_tmicrolist, N_tmicrolist, ptr_tmicrolist, &
            isinwf, d0s, N_int)
      if(N_microlist(0) > 0 .and. idx_microlist(1) > i_generator) stop "wtf..."
      if(ptr_microlist(mo_tot_num * 2 + 1) == 1 .and. ptr_tmicrolist(mo_tot_num * 2 + 1) == 1) cycle
      
      call finish_isinwf(ion_det, psi_det_sorted(1,1,N_det_selectors+1), N_det - N_det_selectors, isinwf)
      
      
      call create_futur_ptr(ptr_microlist, idx_microlist, ptr_futur_microlist, N_futur_microlist, i_generator)
      call create_futur_ptr(ptr_tmicrolist, idx_tmicrolist, ptr_futur_tmicrolist, N_futur_tmicrolist, i_generator)
      
      
      do j=1, ptr_microlist(mo_tot_num * 2 + 1) - 1
        psi_coef_microlist(j,:) = psi_selectors_coef(idx_microlist(j),:)
      enddo
      do j=1, ptr_tmicrolist(mo_tot_num * 2 + 1) - 1
        psi_coef_tmicrolist(j,:) = psi_selectors_coef(idx_tmicrolist(j),:)
      enddo
      
      
      ! Create particles
      ! ----------------
      integer :: i_particle1, i_particle2, k_particle, j_particle
      integer :: p1, p2, sporb, lorb
      
      do j1=1,N_particles(ispin1)
      i_particle1 = particle_list(j1, ispin1)
      p1 = i_particle1 + (ispin1 - 1) * mo_tot_num
      if(N_tmicrolist(p1) > 0 .and. idx_tmicrolist(ptr_tmicrolist(p1+1)-1) > i_generator) cycle
      jb = 1
      if(ispin1 == ispin2) jb = j1+1
      do j2=jb,N_particles(ispin2)
        
        i_particle2 = particle_list(j2, ispin2)
        
        
        
        p2 = i_particle2 + (ispin2 - 1) * mo_tot_num
        if(N_tmicrolist(p2) > 0 .and. idx_tmicrolist(ptr_tmicrolist(p2+1)-1) > i_generator) cycle
        if(isinwf(p1, p2)) cycle
        exc_det = ion_det
        
          
        if(N_microlist(p1) < N_microlist(p2)) then
          sporb = p1
          lorb = p2
        else
          sporb = p2
          lorb = p1
        endif
        
        
        ! Apply the particle
        k_particle = ishft(i_particle2-1,-bit_kind_shift)+1            ! N_int
        j_particle = i_particle2-ishft(k_particle-1,bit_kind_shift)-1  ! bit index
        exc_det(k_particle,ispin2) = ibset(exc_det(k_particle,ispin2),j_particle)
        
        ! Apply the particle
        k_particle = ishft(i_particle1-1,-bit_kind_shift)+1            ! N_int
        j_particle = i_particle1-ishft(k_particle-1,bit_kind_shift)-1  ! bit index
        exc_det(k_particle,ispin1) = ibset(exc_det(k_particle,ispin1),j_particle)
        
        
!        if(.false. .or. (is_in_wavefunction(exc_det,N_int) .and. .not. isinwf(p1,p2))) then
!           print *, p1, p2
!           call debug_det(ion_det, N_int)
!           call debug_det(exc_det, N_int)
! !           do i=1,mo_tot_num*2
! !             print *, isinwf(:, i)
! !           end do
!           print *, isinwf(p1, p2)
!           stop "isw"
!         end if
        
        ! TODO
        
        logical, external :: is_in_wavefunction
        logical :: nok
        ! TODO : Check connected to ref
!         if (.not. is_in_wavefunction(exc_det,N_int)) then
        
        
          ! Compute perturbative contribution and select determinant
          double precision :: i_H_psi_value(N_states), i_H_psi_value2(N_states)
          double precision :: i_H_full(N_states)
          i_H_psi_value = 0d0
          i_H_psi_value2 = 0d0
          i_H_full = 0d0
          
!            call i_H_psi(exc_det,psi_selectors,psi_selectors_coef,N_int,N_det_selectors,psi_selectors_size,N_states,i_H_full)
          
          
          
!           call check_past(exc_det, microlist, idx_microlist, N_microlist(0), i_generator, nok, N_int)
!           if(nok) cycle
          
          nok = .false.
          
          call check_futur(exc_det, microlist(1,1,ptr_futur_microlist(sporb)), N_futur_microlist(sporb), nok, N_int)
          
          if(nok) cycle

          if(N_microlist(0)-N_futur_microlist(0) > 0) then
            call i_H_psi(exc_det,microlist(1,1,ptr_microlist(0)),psi_coef_microlist(ptr_microlist(0), 1),N_int,N_microlist(0)-N_futur_microlist(0),psi_selectors_size*4,N_states,i_H_psi_value)
!             if(i_H_psi_value(1) /= d0s(p1, p2, 1) .and. d0s(p1, p2, 1) /= 0d0) then
!               print *, d0s(p1, p2, 1), i_H_psi_value(1)
!               print *, d0s(:3, :3, 1)
!               stop "SKSL"
!             end if
          end if
! if(N_futur_microlist(sporb) > 0) call i_H_psi(exc_det,microlist(1,1,ptr_futur_microlist(sporb)),psi_coef_microlist(ptr_futur_microlist(sporb), 1),N_int,N_futur_microlist(sporb),psi_selectors_size*4,N_states,i_H_psi_value2)
!           !$OMP ATOMIC
!           d0 += dabs(i_H_psi_value(1))
!           d2 += N_futur_microlist(sporb)


          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          if(N_microlist(sporb)-N_futur_microlist(sporb) > 0) then
! !             if(dfloat(N_futur_microlist(lorb)) / dfloat(N_futur_microlist(sporb)) < 2d0) then
!               c1 = ptr_futur_microlist(p1)
!               c2 = ptr_futur_microlist(p2)
!               do while(c1 < ptr_microlist(p1+1) .and. c2 < ptr_microlist(p2+1))
!                 if(idx_microlist(c1) < idx_microlist(c2)) then
!                   c1 += 1
!                 else if(idx_microlist(c1) > idx_microlist(c2)) then
!                   c2 += 1
!                 else
!                   call i_H_j(exc_det,microlist(1,1,c1),N_int,hij)
!                   do j = 1, N_states
!                     i_H_psi_value2(j) = i_H_psi_value2(j) + psi_coef_microlist(c1,j)*hij
!                   end do
!                   c1 += 1
!                   c2 += 1
!                 endif
!               end do
!             else
              call i_H_psi(exc_det,microlist(1,1,ptr_microlist(sporb)),psi_coef_microlist(ptr_microlist(sporb), 1),N_int,N_microlist(sporb)-N_futur_microlist(sporb),psi_selectors_size*4,N_states,i_H_psi_value2)
              i_H_psi_value = i_H_psi_value + i_H_psi_value2
!               !$OMP ATOMIC
!               d2 += dabs(i_H_psi_value2(1))
!               d2 += N_futur_microlist(sporb)
! !             end if
          end if
          
            
!           enddo 2099.3283623955813
          
          
          !!!!!!!!!!!!!!!!!!
!           if(N_microlist(0) > 0) call i_H_psi(exc_det,microlist,psi_coef_microlist(ptr_microlist(0), 1),N_int,N_microlist(0),psi_selectors_size*4,N_states,i_H_psi_value)
!           if(N_microlist(sporb) > 0) call i_H_psi(exc_det,microlist(1,1,ptr_microlist(sporb)),psi_coef_microlist(ptr_microlist(sporb), 1),N_int,N_microlist(sporb),psi_selectors_size*4,N_states,i_H_psi_value2)
          
!           i_H_psi_value = i_H_psi_value + i_H_psi_value2
          
          integer :: c1, c2
          double precision :: hij
          c1 = ptr_tmicrolist(p1)
          c2 = ptr_tmicrolist(p2)
          do while(.true.)
            if(c1 >= ptr_futur_tmicrolist(p1) .or. c2 >= ptr_futur_tmicrolist(p2)) then
              if(ptr_futur_tmicrolist(p1) /= c1) then
                call i_H_psi(exc_det,tmicrolist(1,1,c1),psi_coef_tmicrolist(c1, 1),N_int, ptr_futur_tmicrolist(p1)-c1 ,psi_selectors_size*3,N_states,i_H_psi_value2)
                i_H_psi_value = i_H_psi_value + i_H_psi_value2
! !                 !$OMP ATOMIC
!                 d1 += dabs(i_H_psi_value2(1))
              end if
              
              if(ptr_futur_tmicrolist(p2) /= c2) then
                call i_H_psi(exc_det,tmicrolist(1,1,c2),psi_coef_tmicrolist(c2, 1),N_int, ptr_futur_tmicrolist(p2)-c2 ,psi_selectors_size*3,N_states,i_H_psi_value2)
                i_H_psi_value = i_H_psi_value + i_H_psi_value2
!                 !$OMP ATOMIC
!                 d1 += dabs(i_H_psi_value2(1))
              endif
              
              exit
            endif
            
            if(idx_tmicrolist(c1) < idx_tmicrolist(c2)) then
              call i_H_j(exc_det,tmicrolist(1,1,c1),N_int,hij)
              do j = 1, N_states
                i_H_psi_value(j) = i_H_psi_value(j) + psi_coef_tmicrolist(c1,j)*hij
              enddo
!               !$OMP ATOMIC
!               d1 += dabs(psi_coef_tmicrolist(c1,1)*hij)
              c1 += 1
            else
              call i_H_j(exc_det,tmicrolist(1,1,c2),N_int,hij)
              do j = 1, N_states
                i_H_psi_value(j) = i_H_psi_value(j) + psi_coef_tmicrolist(c2,j)*hij
              enddo
              if(idx_tmicrolist(c1) == idx_tmicrolist(c2)) c1 = c1 + 1
!               !$OMP ATOMIC
!               d1 += dabs(psi_coef_tmicrolist(c2,1)*hij)
              c2 += 1
            end if
          enddo
          
          double precision :: Hii, diag_H_mat_elem_fock
          Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),exc_det,fock_diag_tmp,N_int)
          double precision :: delta_E, e_pert(N_states), e_pertm
          e_pert(:) = 0d0
          e_pertm = 0d0
          
          do k=1,N_states
!              if(dabs(1d0 - i_H_psi_value(k)/i_H_full(k)) > 1d-6) then
!               print *, i_H_psi_value(k), i_H_full(k), i_H_psi_value(k)/i_H_full(k)
!               stop "PAS BON, PAS BOOON (double)"
!               
!             endif
            
            if (i_H_psi_value(k) == 0.d0) cycle
            delta_E = E0(k) - Hii
            if (delta_E < 0.d0) then
              e_pert(k) = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            else
              e_pert(k) = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E) 
            endif
            if(dabs(e_pert(k)) > dabs(e_pertm)) e_pertm = e_pert(k)
            pt2(k) += e_pert(k)
          enddo
          if(dabs(e_pertm) > dabs(buf%mini)) then
            call add_to_selection_buffer(buf, exc_det, e_pertm)
          end if
!         endif ! iwf

        
        
        
        
        
        
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
  !print *, "D ::: ", d0/1000000, d1/1000000, d2/1000000
end



subroutine create_futur_ptr(ptr_microlist, idx_microlist, ptr_futur_microlist, N_futur_microlist, i_generator)
  integer, intent(in) :: ptr_microlist(0:mo_tot_num * 2 + 1), idx_microlist(*), i_generator
  integer, intent(out) :: ptr_futur_microlist(0:mo_tot_num * 2 + 1), N_futur_microlist(0:mo_tot_num * 2)
  integer :: i, j
  
  N_futur_microlist = 0
  do i=0,mo_tot_num*2
    ptr_futur_microlist(i) = ptr_microlist(i+1)
    do j=ptr_microlist(i), ptr_microlist(i+1) - 1
      if(idx_microlist(j) > i_generator) then
        ptr_futur_microlist(i) = j
        N_futur_microlist(i) = ptr_microlist(i+1) - j
        exit
      end if
    end do
  end do
end subroutine


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
      if(i > i_cur) then
        N_microlist(:) = 0  !!!! PAST LINKED TO EVERYBODY!
        ptr_microlist(:) = 1
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


subroutine finish_isinwf(key_mask, keys, N_keys, isinwf)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: key_mask(N_int, 2), keys(N_int, 2, N_keys)
  integer(bit_kind) :: key_mask_neg(N_int, 2)
  integer(bit_kind) :: mobileMask(N_int, 2)
  logical,intent(inout) :: isinwf(mo_tot_num*2, mo_tot_num*2)
  integer, intent(in) :: N_keys
  integer :: i,j,nt,nt2,list(2,2), n_element(2)
  logical, external :: detEq
  
  do i=1,N_int
    key_mask_neg(i,1) = not(key_mask(i,1))
    key_mask_neg(i,2) = not(key_mask(i,2))
  end do
  
  do i=1, N_keys
    nt = 0
    
    do j=1,N_int
      mobileMask(j,1) = iand(key_mask_neg(j,1), keys(j,1,i))
      mobileMask(j,2) = iand(key_mask_neg(j,2), keys(j,2,i))
      nt += popcnt(mobileMask(j, 1)) + popcnt(mobileMask(j, 2))
    end do
    
    if(nt /= 2) cycle
    
    call bitstring_to_list(mobileMask(1,1), list(1,1), n_element(1), N_int)
    call bitstring_to_list(mobileMask(1,2), list(1,2), n_element(2), N_int)
          
    if(n_element(1) >= 1) nt = list(1,1)
    if(n_element(1) == 2) nt2 = list(2,1)
    if(n_element(2) == 2) nt = list(2,2) + mo_tot_num
    if(n_element(2) >= 1) nt2 = list(1,2) + mo_tot_num
    
    isinwf(nt, nt2) = .true.
    isinwf(nt2, nt) = .true.
  end do
end subroutine


subroutine create_microlist_double(minilist, i_cur, N_minilist, key_mask, microlist, idx_microlist, N_microlist, ptr_microlist, &
                                                                          tmicrolist, idx_tmicrolist, N_tmicrolist, ptr_tmicrolist, &
                                                                          isinwf, d0s, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint, i_cur, N_minilist
  integer(bit_kind), intent(in) :: minilist(Nint,2,N_minilist), key_mask(Nint,2)
  
  integer, intent(out) :: N_microlist(0:mo_tot_num*2), ptr_microlist(0:mo_tot_num*2+1), idx_microlist(N_minilist*4)
  integer(bit_kind), intent(out) :: microlist(Nint,2,N_minilist*4)
  
  integer, intent(out) :: N_tmicrolist(0:mo_tot_num*2), ptr_tmicrolist(0:mo_tot_num*2+1), idx_tmicrolist(N_minilist*4)
  integer(bit_kind), intent(out) :: tmicrolist(Nint,2,N_minilist*4)
  
  
  integer :: i,j,k,s,nt,nt2,n_element(2,N_minilist), idx(0:N_minilist)
  integer :: list(4,2,N_minilist), cur_microlist(0:mo_tot_num*2+1), cur_tmicrolist(0:mo_tot_num*2+1)
  integer(bit_kind) :: key_mask_neg(Nint,2), mobileMask(Nint,2)
  integer :: mo_tot_num_2
  logical,intent(out) :: isinwf(mo_tot_num*2, mo_tot_num*2)
  double precision, intent(out) :: d0s(mo_tot_num, mo_tot_num, N_states)
  double precision :: integ(mo_tot_num, mo_tot_num)
  isinwf = .false.
  integ = 0d0
  d0s = 0d0
  mo_tot_num_2 = mo_tot_num+mo_tot_num
  
  idx(0) = 0
  do i=1,Nint
    key_mask_neg(i,1) = not(key_mask(i,1))
    key_mask_neg(i,2) = not(key_mask(i,2))
  end do
  
  do i=0,mo_tot_num_2
    N_microlist(i) = 0
    N_tmicrolist(i) = 0
  enddo
  
  do i=1, N_minilist
    nt = 0
    do j=1,Nint
      mobileMask(j,1) = iand(key_mask_neg(j,1), minilist(j,1,i))
      mobileMask(j,2) = iand(key_mask_neg(j,2), minilist(j,2,i))
      nt += popcnt(mobileMask(j, 1)) + popcnt(mobileMask(j, 2))
    end do
    
    if(nt > 4) cycle !! TOO MANY DIFFERENCES
    
    idx(0) += 1
    idx(idx(0)) = i
    
    call bitstring_to_list(mobileMask(1,1), list(1,1,idx(0)), n_element(1, idx(0)), Nint)
    call bitstring_to_list(mobileMask(1,2), list(1,2,idx(0)), n_element(2, idx(0)), Nint)
      
      
    if(nt <= 2) then
      if(i > i_cur) then
        N_microlist = 0 
        ptr_microlist = 1
        N_tmicrolist = 0 
        ptr_tmicrolist = 1
        return  
      else
        !n_element(:, idx(0)) = (/2, 0/)
        N_microlist(0) = N_microlist(0) + 1
        
        if(n_element(1,idx(0)) >= 1) nt = list(1,1,idx(0))
        if(n_element(1,idx(0)) == 2) nt2 = list(2,1,idx(0))
        if(n_element(2,idx(0)) == 2) nt = list(2,2,idx(0)) + mo_tot_num
        if(n_element(2,idx(0)) >= 1) nt2 = list(1,2,idx(0)) + mo_tot_num
        
        isinwf(nt, nt2) = .true.
        isinwf(nt2, nt) = .true.
        double precision, external :: get_mo_bielec_integral
        nt = mod(nt, mo_tot_num)
        nt2 = mod(nt2, mo_tot_num)
        ! call get_mo_bielec_integrals_ij(nt, nt2 ,mo_tot_num,integ,mo_integrals_map)
        
!         do j=1, N_states
!           call i_h_j
!           d0s(:,:,j) += integ(:,:) * psi_selectors_coef(i,j)                   !!!!!!!!!!!!!!!!!!! MOOOOOCHE !!!!! suppose que minilist = psi_selectors .....
!         end do
!         print *, "TO ", integ(mod(nt, mo_tot_num), mod(nt2, mo_tot_num))
      endif
    else
      do s=1,2
        do j=1,n_element(s,idx(0))
          k = list(j,s,idx(0)) + mo_tot_num * (s-1)
          if(nt == 4) N_microlist(k) = N_microlist(k) + 1
          if(nt == 3) N_tmicrolist(k) = N_tmicrolist(k) + 1
        end do
      end do
    endif
  end do
  
  ptr_microlist(0) = 1
  ptr_tmicrolist(0) = 1
  do i=1,mo_tot_num_2+1
    ptr_microlist(i) = ptr_microlist(i-1) + N_microlist(i-1)
    ptr_tmicrolist(i) = ptr_tmicrolist(i-1) + N_tmicrolist(i-1)
  end do

  do i=0,mo_tot_num_2+1
    cur_microlist(i) = ptr_microlist(i)
    cur_tmicrolist(i) = ptr_tmicrolist(i)
  end do
  
  
  do i=1, idx(0)
    if(n_element(1, i) + n_element(2, i) > 4) stop "wired"
    if(n_element(1, i) + n_element(2, i) <= 2) then
      idx_microlist(cur_microlist(0)) = idx(i)
      do k=1,Nint
        microlist(k,1,cur_microlist(0)) = minilist(k,1,idx(i))
        microlist(k,2,cur_microlist(0)) = minilist(k,2,idx(i))
      enddo
      cur_microlist(0) = cur_microlist(0) + 1
    else
      do s = 1, 2
        do j=1,n_element(s,i)
          nt = list(j,s,i) + mo_tot_num * (s-1)
          
          if(n_element(1,i) + n_element(2,i) == 4) then
            idx_microlist(cur_microlist(nt)) = idx(i)
            do k=1,Nint
              microlist(k,1,cur_microlist(nt)) = minilist(k,1,idx(i))
              microlist(k,2,cur_microlist(nt)) = minilist(k,2,idx(i))
            enddo
            cur_microlist(nt) = cur_microlist(nt) + 1
          else
            idx_tmicrolist(cur_tmicrolist(nt)) = idx(i)
            do k=1,Nint
              tmicrolist(k,1,cur_tmicrolist(nt)) = minilist(k,1,idx(i))
              tmicrolist(k,2,cur_tmicrolist(nt)) = minilist(k,2,idx(i))
            enddo
            cur_tmicrolist(nt) = cur_tmicrolist(nt) + 1
          endif
        end do
      end do
    end if
  end do
end subroutine


subroutine check_past(det, list, idx, N, cur, ok, Nint)
  implicit none
  use bitmasks
  
  integer(bit_kind), intent(in) :: det(Nint, 2), list(Nint, 2, N)
  integer, intent(in) :: Nint, idx(N), N, cur
  logical, intent(out) :: ok
  integer :: i,s,ni
  
  ok = .false.
  do i=N,1,-1
    if(idx(i) <= cur) exit
    s = 0
    do ni=1,Nint
      s += popcnt(xor(det(ni,1), list(ni,1,i))) + popcnt(xor(det(ni,2), list(ni,2,i)))
    end do
    if(s <= 4) then
      ok = .true.
      return
    end if
  end do
end subroutine


subroutine check_futur(det, list, N, ok, Nint)
  implicit none
  use bitmasks
  
  integer(bit_kind), intent(in) :: det(Nint, 2), list(Nint, 2, N)
  integer, intent(in) :: Nint, N
  logical, intent(out) :: ok
  integer :: i,s,ni
  
  ok = .false.
  do i=1,N
    s = 0
    do ni=1,Nint
      s += popcnt(xor(det(ni,1), list(ni,1,i))) + popcnt(xor(det(ni,2), list(ni,2,i)))
    end do
    if(s <= 4) then
      ok = .true.
      return
    end if
  end do
end subroutine

