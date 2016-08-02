

BEGIN_PROVIDER [ double precision, integral8, (mo_tot_num,  mo_tot_num, mo_tot_num, mo_tot_num) ]
  integral8 = 0d0
  integer :: h1, h2
  do h1=1, mo_tot_num
    do h2=1, mo_tot_num
      call get_mo_bielec_integrals_ij(h1, h2 ,mo_tot_num,integral8(1,1,h1,h2),mo_integrals_map)
    end do
  end do
END_PROVIDER


subroutine selection_slaved(thread,iproc,energy)
  use f77_zmq
  use selection_types
  implicit none

  double precision, intent(in)    :: energy(N_states_diag)
  integer,  intent(in)            :: thread, iproc
  integer                        :: rc, i

  integer                        :: worker_id, task_id(1), ctask, ltask
  character*(512)                :: task

  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push

  type(selection_buffer) :: buf, buf2
  logical :: done
  double precision :: pt2(N_states)

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)
  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  if(worker_id == -1) then
    print *, "WORKER -1"
    !call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    call end_zmq_push_socket(zmq_socket_push,thread)
    return
  end if
  buf%N = 0
  ctask = 1
  pt2 = 0d0

  do
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id(ctask), task)
    done = task_id(ctask) == 0
    if (done) then
      ctask = ctask - 1
    else
      integer :: i_generator, i_generator_start, i_generator_max, step, N
      read (task,*) i_generator_start, i_generator_max, step, N
      if(buf%N == 0) then
        ! Only first time 
        call create_selection_buffer(N, N*2, buf)
        call create_selection_buffer(N, N*3, buf2)
      else
        if(N /= buf%N) stop "N changed... wtf man??"
      end if
      !print *, "psi_selectors_coef ", psi_selectors_coef(N_det_selectors-5:N_det_selectors, 1)
      !call debug_det(psi_selectors(1,1,N_det_selectors), N_int)
      do i_generator=i_generator_start,i_generator_max,step
        call select_connected(i_generator,energy,pt2,buf)
      enddo
    endif

    if(done .or. ctask == size(task_id)) then
      if(buf%N == 0 .and. ctask > 0) stop "uninitialized selection_buffer"
      do i=1, ctask
         call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id(i))
      end do
      if(ctask > 0) then
        call push_selection_results(zmq_socket_push, pt2, buf, task_id(1), ctask)
        do i=1,buf%cur
          call add_to_selection_buffer(buf2, buf%det(1,1,i), buf%val(i))
        enddo
        call sort_selection_buffer(buf2)
        buf%mini = buf2%mini
        pt2 = 0d0
        buf%cur = 0
      end if
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
  if(rc /= 4) stop "push"
  rc = f77_zmq_send( zmq_socket_push, pt2, 8*N_states, ZMQ_SNDMORE)
  if(rc /= 8*N_states) stop "push"

  rc = f77_zmq_send( zmq_socket_push, b%val(1), 8*b%cur, ZMQ_SNDMORE)
  if(rc /= 8*b%cur) stop "push"

  rc = f77_zmq_send( zmq_socket_push, b%det(1,1,1), bit_kind*N_int*2*b%cur, ZMQ_SNDMORE)
  if(rc /= bit_kind*N_int*2*b%cur) stop "push"

  rc = f77_zmq_send( zmq_socket_push, ntask, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "push"

  rc = f77_zmq_send( zmq_socket_push, task_id(1), ntask*4, 0)
  if(rc /= 4*ntask) stop "push"

! Activate is zmq_socket_push is a REQ
!  rc = f77_zmq_recv( zmq_socket_push, task_id(1), ntask*4, 0)
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

  rc = f77_zmq_recv( zmq_socket_pull, N, 4, 0)
  if(rc /= 4) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, pt2, N_states*8, 0)
  if(rc /= 8*N_states) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, val(1), 8*N, 0)
  if(rc /= 8*N) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, det(1,1,1), bit_kind*N_int*2*N, 0)
  if(rc /= bit_kind*N_int*2*N) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, ntask, 4, 0)
  if(rc /= 4) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, task_id(1), ntask*4, 0)
  if(rc /= 4*ntask) stop "pull"

! Activate is zmq_socket_pull is a REP
!  rc = f77_zmq_send( zmq_socket_pull, task_id(1), ntask*4, 0)
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

    call select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
    call select_singles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
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
  b%mini = max(b%mini,dabs(b%val(b%N)))
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
  integer :: done
  real :: time, time0
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  allocate(val(b%N), det(N_int, 2, b%N), task_id(N_det))
  done = 0
  more = 1
  pt2(:) = 0d0
  call CPU_TIME(time0)
  do while (more == 1)
    call pull_selection_results(zmq_socket_pull, pt2_mwen, val(1), det(1,1,1), N, task_id, ntask)
    pt2 += pt2_mwen
    do i=1, N
      call add_to_selection_buffer(b, det(1,1,i), val(i))
    end do

    do i=1, ntask
      if(task_id(i) == 0) then
          print *,  "Error in collector"
      endif
      call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id(i),more)
    end do
    done += ntask
    call CPU_TIME(time)
!    print *, "DONE" , done, time - time0
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



  integer :: ptr_microlist(0:mo_tot_num * 2 + 1), N_microlist(0:mo_tot_num * 2)
  integer, allocatable :: idx_microlist(:)
  integer(bit_kind), allocatable :: microlist(:, :, :)
  double precision, allocatable :: psi_coef_microlist(:,:)

  allocate(microlist(N_int, 2, N_det_selectors * 3), psi_coef_microlist(psi_selectors_size * 3, N_states), idx_microlist(N_det_selectors * 3))

  do ispin=1,2


    do i=1, N_holes(ispin)
      ion_det(:,:) = psi_det_generators(:,:,i_generator)
      integer :: i_hole
      i_hole = hole_list(i,ispin)

      ! Apply the hole
      integer :: j_hole, k_hole
      k_hole = ishft(i_hole-1,-bit_kind_shift)+1        ! N_int
      j_hole = i_hole-ishft(k_hole-1,bit_kind_shift)-1  ! bit index
      ion_det(k_hole,ispin) = ibclr(ion_det(k_hole,ispin),j_hole)


      call create_microlist_single(psi_selectors, i_generator, N_det_selectors, ion_det, microlist, idx_microlist, N_microlist, ptr_microlist, N_int)

      do j=1, ptr_microlist(mo_tot_num * 2 + 1) - 1
        psi_coef_microlist(j,:) = psi_selectors_coef_transp(:,idx_microlist(j))
      enddo

      if(ptr_microlist(mo_tot_num * 2 + 1) == 1) then
        cycle
      endif


      do j=1,N_particles(ispin)
        exc_det(:,:) = ion_det(:,:)

        integer :: i_particle
        i_particle = particle_list(j,ispin)

        integer :: j_particle, k_particle
        k_particle = ishft(i_particle-1,-bit_kind_shift)+1            ! N_int
        j_particle = i_particle-ishft(k_particle-1,bit_kind_shift)-1  ! bit index
        exc_det(k_particle,ispin) = ibset(exc_det(k_particle,ispin),j_particle)


        logical, external :: is_in_wavefunction
        logical :: nok
        if (.not. is_in_wavefunction(exc_det,N_int)) then
          double precision :: i_H_psi_value(N_states), i_H_psi_value2(N_states)
          i_H_psi_value = 0d0
          i_H_psi_value2 = 0d0
          integer :: sporb


          nok = .false.
          sporb = i_particle + (ispin - 1) * mo_tot_num

          if(N_microlist(sporb) > 0) call check_past(exc_det, microlist(1,1,ptr_microlist(sporb)), idx_microlist(ptr_microlist(sporb)), N_microlist(sporb), i_generator, nok, N_int)
          if(nok) cycle

          if(N_microlist(0) > 0) call i_H_psi(exc_det,microlist,psi_coef_microlist,N_int,N_microlist(0),psi_selectors_size*3,N_states,i_H_psi_value)
          if(N_microlist(sporb) > 0) call i_H_psi(exc_det,microlist(1,1,ptr_microlist(sporb)),psi_coef_microlist(ptr_microlist(sporb), 1),N_int,N_microlist(sporb),psi_selectors_size*3,N_states,i_H_psi_value2)
          i_H_psi_value(:) = i_H_psi_value(:) + i_H_psi_value2(:)
          double precision :: Hii, diag_H_mat_elem_fock
          Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),exc_det,fock_diag_tmp,N_int)

          double precision :: delta_E, e_pert(N_states), e_pertm
          e_pert(:) = 0d0
          e_pertm = 0d0

          do k=1,N_states
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
    do i1=N_holes(ispin1),1,-1   ! Generate low excitations first
    if(ispin1 == ispin2) then
      ib = i1+1
    else
      ib = 1
    endif
    do i2=N_holes(ispin2),ib,-1   ! Generate low excitations first
      ion_det(:,:) = psi_det_generators(:,:,i_generator)

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
      
      if(ptr_microlist(mo_tot_num * 2 + 1) == 1 .and. ptr_tmicrolist(mo_tot_num * 2 + 1) == 1) cycle

      call finish_isinwf(ion_det, psi_det_sorted(1,1,N_det_selectors+1), N_det - N_det_selectors, isinwf)


      call create_futur_ptr(ptr_microlist, idx_microlist, ptr_futur_microlist, N_futur_microlist, i_generator)
      call create_futur_ptr(ptr_tmicrolist, idx_tmicrolist, ptr_futur_tmicrolist, N_futur_tmicrolist, i_generator)


      do j=1, ptr_microlist(mo_tot_num * 2 + 1) - 1
        psi_coef_microlist(j,:) = psi_selectors_coef_transp(:,idx_microlist(j))
      enddo
      do j=1, ptr_tmicrolist(mo_tot_num * 2 + 1) - 1
        psi_coef_tmicrolist(j,:) = psi_selectors_coef_transp(:,idx_tmicrolist(j))
      enddo


      ! Create particles
      ! ----------------
      integer :: i_particle1, i_particle2, k_particle, j_particle
      integer :: p1, p2, sporb, lorb

      do j1=1,N_particles(ispin1)
      i_particle1 = particle_list(j1, ispin1)
      p1 = i_particle1 + (ispin1 - 1) * mo_tot_num
      if(N_tmicrolist(p1) > 0 .and. idx_tmicrolist(ptr_tmicrolist(p1)) < i_generator) cycle
      jb = 1
      if(ispin1 == ispin2) jb = j1+1
      do j2=jb,N_particles(ispin2)

        i_particle2 = particle_list(j2, ispin2)



        p2 = i_particle2 + (ispin2 - 1) * mo_tot_num
        if(N_tmicrolist(p2) > 0 .and. idx_tmicrolist(ptr_tmicrolist(p2)) < i_generator) cycle
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

        logical, external :: is_in_wavefunction
        logical :: nok
        ! Compute perturbative contribution and select determinant
        double precision :: i_H_psi_value(N_states), i_H_psi_value2(N_states)
        i_H_psi_value = 0d0
        i_H_psi_value2 = 0d0

        nok = .false.
        call check_past_s(exc_det, microlist(1,1,ptr_microlist(sporb)), N_microlist(sporb) - N_futur_microlist(sporb), nok, N_int)
        if(nok) cycle
        !DET DRIVEN
!         if(N_futur_microlist(0) > 0) then
!           call i_H_psi(exc_det,microlist(1,1,ptr_futur_microlist(0)),psi_coef_microlist(ptr_futur_microlist(0), 1),N_int,N_futur_microlist(0),psi_selectors_size*4,N_states,i_H_psi_value)
!         end if
        !INTEGRAL DRIVEN
        do j=1, N_states
          i_H_psi_value(j) = d0s(mod(p1-1, mo_tot_num)+1, mod(p2-1, mo_tot_num)+1, j)
        end do

        
        if(N_futur_microlist(sporb) > 0) then
        !!! COMPUTE INTERSECTION
        !!!!!!!!!!!!!
!             if(dfloat(N_futur_microlist(lorb)) / dfloat(N_futur_microlist(sporb)) < 2d0) then
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
          call i_H_psi(exc_det,microlist(1,1,ptr_futur_microlist(sporb)),psi_coef_microlist(ptr_futur_microlist(sporb), 1),N_int,N_futur_microlist(sporb),psi_selectors_size*4,N_states,i_H_psi_value2)
          i_H_psi_value = i_H_psi_value + i_H_psi_value2
        end if

        if(.false.) then ! DET DRIVEN
          integer :: c1, c2
          double precision :: hij
          c1 = ptr_futur_tmicrolist(p1)
          c2 = ptr_futur_tmicrolist(p2)
          do while(.true.)
            if(c1 >= ptr_tmicrolist(p1+1) .or. c2 >= ptr_tmicrolist(p2+1)) then
              if(ptr_tmicrolist(p1+1) /= c1) then
                call i_H_psi(exc_det,tmicrolist(1,1,c1),psi_coef_tmicrolist(c1, 1),N_int, ptr_tmicrolist(p1+1)-c1 ,psi_selectors_size*3,N_states,i_H_psi_value2)
                i_H_psi_value = i_H_psi_value + i_H_psi_value2
              end if

              if(ptr_tmicrolist(p2+1) /= c2) then
                call i_H_psi(exc_det,tmicrolist(1,1,c2),psi_coef_tmicrolist(c2, 1),N_int, ptr_tmicrolist(p2+1)-c2 ,psi_selectors_size*3,N_states,i_H_psi_value2)
                i_H_psi_value = i_H_psi_value + i_H_psi_value2
              endif

              exit
            endif

            if(idx_tmicrolist(c1) < idx_tmicrolist(c2)) then
              call i_H_j(exc_det,tmicrolist(1,1,c1),N_int,hij)
              do j = 1, N_states
                i_H_psi_value(j) = i_H_psi_value(j) + psi_coef_tmicrolist(c1,j)*hij
              enddo
              c1 += 1
            else
              call i_H_j(exc_det,tmicrolist(1,1,c2),N_int,hij)
              do j = 1, N_states
                i_H_psi_value(j) = i_H_psi_value(j) + psi_coef_tmicrolist(c2,j)*hij
              enddo
              if(idx_tmicrolist(c1) == idx_tmicrolist(c2)) c1 = c1 + 1
              c2 += 1
            end if
          enddo
        end if

        double precision :: Hii, diag_H_mat_elem_fock
        Hii = diag_H_mat_elem_fock(psi_det_generators(1,1,i_generator),exc_det,fock_diag_tmp,N_int)
        double precision :: delta_E, e_pert(N_states), e_pertm
        e_pert(:) = 0d0
        e_pertm = 0d0

        do k=1,N_states
          if (i_H_psi_value(k) == 0.d0) cycle
          delta_E = E0(k) - Hii
          if (delta_E < 0.d0) then
            e_pert(k) = 0.5d0 * (-dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E)
          else
            e_pert(k) = 0.5d0 * ( dsqrt(delta_E * delta_E + 4.d0 * i_H_psi_value(k) * i_H_psi_value(k)) - delta_E)
          endif
          e_pertm += dabs(e_pert(k))
!          if(dabs(e_pert(k)) > dabs(e_pertm)) e_pertm = e_pert(k)
          pt2(k) += e_pert(k)
        enddo
        if(dabs(e_pertm) > dabs(buf%mini)) then
          call add_to_selection_buffer(buf, exc_det, e_pertm)
        end if
      enddo 
      enddo
    enddo
    enddo
  enddo
  enddo
end



subroutine create_futur_ptr(ptr_microlist, idx_microlist, ptr_futur_microlist, N_futur_microlist, i_generator)
  integer, intent(in) :: ptr_microlist(0:mo_tot_num * 2 + 1), idx_microlist(*), i_generator
  integer, intent(out) :: ptr_futur_microlist(0:mo_tot_num * 2 + 1), N_futur_microlist(0:mo_tot_num * 2)
  integer :: i, j

  N_futur_microlist = 0
  do i=0,mo_tot_num*2
    ptr_futur_microlist(i) = ptr_microlist(i+1)
    do j=ptr_microlist(i), ptr_microlist(i+1) - 1
      if(idx_microlist(j) >= i_generator) then
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
      if(i < i_cur) then !!!!!!!!!!!!!!!!!!!!! DESACTIVADO
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


  integer :: i,j,k,s,nt,nt2
  integer, allocatable :: n_element(:,:), idx(:), list(:,:,:)
  integer :: cur_microlist(0:mo_tot_num*2+1), cur_tmicrolist(0:mo_tot_num*2+1)
  integer(bit_kind) :: key_mask_neg(Nint,2), mobileMask(Nint,2), tmp_det(Nint, 2)
  integer :: mo_tot_num_2, pwen(4), pweni
  logical,intent(out) :: isinwf(mo_tot_num*2, mo_tot_num*2)
  double precision, intent(out) :: d0s(mo_tot_num, mo_tot_num, N_states)
  double precision :: integ(mo_tot_num, mo_tot_num)
  logical :: localbanned(mo_tot_num*2), banned(mo_tot_num*2), banned_pair(mo_tot_num*2, mo_tot_num*2), ok
  banned = .false.
  banned_pair = .false.

  allocate(list(4,2,N_minilist), n_element(2,N_minilist), idx(0:N_minilist))

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


    if(nt == 2) then
      if(i < i_cur) then
        N_microlist(:) = 0
        ptr_microlist = 1
        N_tmicrolist = 0
        ptr_tmicrolist = 1
        return
      else
        N_microlist(0) = N_microlist(0) + 1
      endif
    else
      do s=1,2
        do j=1,n_element(s,idx(0))
          k = list(j,s,idx(0)) + mo_tot_num * (s-1)
          if(nt == 4) N_microlist(k) = N_microlist(k) + 1
          if(nt == 3) then
            N_tmicrolist(k) = N_tmicrolist(k) + 1
            if(idx(i) < i_cur) banned(nt) = .true.
          end if
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
    if(n_element(1, i) + n_element(2, i) == 2) cycle
    pweni = 0
    do s = 1, 2
      do j=1,n_element(s,i)
        nt = list(j,s,i) + mo_tot_num * (s-1)
        pweni += 1
        pwen(pweni) = nt
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
    if(idx(i) < i_cur .and. pweni == 4) then
      do j=1,4
      do k=j+1,4
        banned_pair(pwen(j), pwen(k)) = .true.
        banned_pair(pwen(k), pwen(j)) = .true.
      end do
      end do
    end if
  end do
  
  
  do i=1, idx(0)
    if(n_element(1, i) + n_element(2, i) <= 2) then
      idx_microlist(cur_microlist(0)) = idx(i)
      do k=1,Nint
        microlist(k,1,cur_microlist(0)) = minilist(k,1,idx(i))
        microlist(k,2,cur_microlist(0)) = minilist(k,2,idx(i))
      enddo
      cur_microlist(0) = cur_microlist(0) + 1

      if(n_element(1,i) >= 1) nt = list(1,1,i)
      if(n_element(1,i) == 2) nt2 = list(2,1,i)
      if(n_element(2,i) == 2) nt = list(2,2,i) + mo_tot_num
      if(n_element(2,i) >= 1) nt2 = list(1,2,i) + mo_tot_num

      isinwf(nt, nt2) = .true.
      isinwf(nt2, nt) = .true.
      !!!! INTEGRAL DRIVEN
!       !!!!!!!!!!!!!!!!!!!!
      call get_d0(minilist(1,1,idx(i)), banned, banned_pair, d0s, key_mask, 1+(nt2-1)/mo_tot_num, 1+(nt-1)/mo_tot_num, &
      mod(nt2-1, mo_tot_num)+1, mod(nt-1, mo_tot_num)+1, psi_selectors_coef_transp(1,idx(i)))

!       do j=1, N_states
!       do nt2=1, mo_tot_num
!       do nt=1, mo_tot_num
!           d0s(nt,nt2,j) = d0s(nt,nt2,j) + (integ(nt,nt2) * psi_selectors_coef(idx(i), j)) !!! SUPPOSE MINILIST = SELECTORS !!!!
!       end do
!       end do
!       end do
    else if(.true. .and. n_element(1, i) + n_element(2, i) == 3) then ! INTEGRAL DRIVEN
      !  -459.6399263191298 
      pweni = 0
      do s = 1, 2
        do j=1,n_element(s,i)
          nt = list(j,s,i) + mo_tot_num * (s-1)
          pweni += 1
          pwen(pweni) = nt
        end do
      end do
      
      call get_d1(minilist(1,1,idx(i)), banned, banned_pair, d0s, key_mask, pwen, psi_selectors_coef_transp(1,idx(i)))
          
!       do k=1, N_states
!       do nt2=1, mo_tot_num
!       do nt=1, mo_tot_num
!           d0s(nt,nt2,k) = d0s(nt,nt2,k) + (integ(nt,nt2) * psi_selectors_coef(idx(i), k)) !!! SUPPOSE MINILIST = SELECTORS !!!!
!       end do
!       end do
!       end do
    end if
  end do
  
  
end subroutine


subroutine get_d1(gen, banned, banned_pair, mat, mask, pwen, coefs)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in) :: mask(N_int, 2), gen(N_int, 2)
  logical, intent(in) :: banned(mo_tot_num*2), banned_pair(mo_tot_num*2, mo_tot_num*2)
  integer(bit_kind) :: deth(N_int, 2), det(N_int, 2), i8
  double precision, intent(in) :: coefs(N_states)
  double precision, intent(inout) :: mat(mo_tot_num, mo_tot_num, N_states)
  double precision :: hij, phase, inv, inv2
  integer, intent(in) :: pwen(3)
  integer :: s(3), p(3), i, j, k, h1, h2, ns(2), sm, mwen, a1, a2, pwens(2), sp, st
  integer :: sfix, pfix
  integer :: exc(0:2, 2, 2)
  logical :: lbanned(mo_tot_num*2)
  logical :: ok, mono, ab
  integer :: tmp_array(4)
  
  lbanned = banned
  !mat = 0d0
  pwens = 0
  
  ns = 0
  do sp=1,2
    do i=1, N_int
      ns(sp) += popcnt(gen(i, sp)) - popcnt(mask(i, sp))
      i8 = iand(not(gen(i, sp)), mask(i, sp))
      if(i8 /= 0_8) then
        sfix = sp
        pfix = 1+trailz(i8) + bit_kind*8*(i-1)
      end if
    end do
  end do
  
  
  do i=1,3
    s(i) = 1+(pwen(i)-1)/mo_tot_num
    p(i) = 1+mod(pwen(i)-1, mo_tot_num)
    pwens(s(i)) += 1
  end do
  
  do i=1,3
    if(s(i) == 1 .and. ns(1) == 0) cycle
    if(s(i) == 2 .and. ns(2) == 0) cycle
    if(lbanned(pwen(i))) cycle
    ab = pwens(s(i)) == 2
    
    if(ns(1) == 1) sm = mod(s(i), 2) + 1
    if(ns(1) == 2) sm = 1
    if(ns(2) == 2) sm = 2
    
    lbanned(pwen(i)) = .true.
    
    if(ab) then
      if(s(mod(i,3)+1) == 2) then
        a1 = mod(i, 3) + 1
        a2 = mod(i+1, 3) + 1
      else
        a2 = mod(i,3)+1
        a1 = mod(i+1,3)+1
      end if
      
      exc(0, :, 1) = 1
      exc(0, :, 2) = 1
      exc(1, 1, 1) = p(a2)
      exc(1, 1, 2) = p(a1)
      exc(1, 2, sfix) = pfix
      
      tmp_array = (/0, 0 ,s(i), p(i) /)
      call apply_particle(mask, tmp_array, deth, ok, N_int)
      
      do j=1,mo_tot_num
        mwen = j + (sm-1)*mo_tot_num
        if(lbanned(mwen)) cycle
        tmp_array =  (/0,0,sm,j/)
        call apply_particle(deth, tmp_array, det, ok, N_int)
        if(.not. ok) cycle
        
        mono = mwen == pwen(a1) .or. mwen == pwen(a2)
        if(mono) then
          call i_h_j(gen, det, N_int, hij)
        else
          exc(1, 2, sm) = j
          call get_double_excitation_phase(gen, det, exc, phase, N_int)
          if(sfix == 1) hij = integral8(j, pfix, p(a1), p(a2)) * phase
          if(sfix == 2) hij = integral8(pfix, j, p(a1), p(a2)) * phase
        end if
        
        if(ns(1) == 1) then
          do st=1, N_states
            if(sm == 2) mat(j, p(i), st) = mat(j, p(i), st) + hij * coefs(st)
            if(sm == 1) mat(p(i), j, st) = mat(p(i), j, st) + hij * coefs(st)
          end do
        else
          do st=1, N_states
            mat(j, p(i), st) += hij * coefs(st)
            mat(p(i), j, st) += hij * coefs(st)
          end do
        end if
      end do
    
    else !! AA / BB
      a1 = mod(i,3)+1
      a2 = mod(i+1,3)+1
      
      h1 = p(a1)
      h2 = p(a2)
      inv = 1d0
      if(h1 > h2) inv = -1d0
      
      if(pwens(s(i)) == 1) sp = mod(s(i), 2)+1
      if(pwens(s(i)) == 3) sp = s(i)
      
      exc(0, :, sp) = 2
      exc(0, :, mod(sp, 2)+1) = 0
      exc(1, 1, sp) = min(h1, h2)
      exc(2, 1, sp) = max(h1, h2)
      
      tmp_array = (/0, 0 ,s(i), p(i) /)
      call apply_particle(mask, tmp_array, deth, ok, N_int)
    
      do j=1,mo_tot_num
        if(j == pfix) inv = -inv
        mwen = j + (sm-1)*mo_tot_num
        if(lbanned(mwen)) cycle
        call apply_particle(deth, tmp_array, det, ok, N_int)
        if(.not. ok) cycle
        
        mono = mwen == pwen(a1) .or. mwen == pwen(a2)
        if(mono) then
          call i_h_j(gen, det, N_int, hij)
        else
          exc(1, 2, sfix) = min(j,pfix)
          exc(2, 2, sp) = max(j,pfix)
          call get_double_excitation_phase(gen, det, exc, phase, N_int)
          hij = (integral8(j, pfix, h1, h2) - integral8(pfix,j, h1, h2))*phase*inv
        end if
        if(ns(1) == 1) then
          do st=1, N_states
            if(sm == 2) mat(j, p(i), st) = mat(j, p(i), st) + hij * coefs(st)
            if(sm == 1) mat(p(i), j, st) = mat(p(i), j, st) + hij * coefs(st)
          end do
        else
          do st=1, N_states
            mat(j, p(i), st) += hij * coefs(st)
            mat(p(i), j, st) += hij * coefs(st)
          end do
        end if
      end do
    end if
  end do
  
end subroutine


subroutine get_d0(gen, banned, banned_pair, mat, mask, s1, s2, h1, h2, coefs)
  use bitmasks
  implicit none

  double precision, intent(inout) :: mat(mo_tot_num, mo_tot_num, N_states)
  logical, intent(in) :: banned(mo_tot_num*2), banned_pair(mo_tot_num*2, mo_tot_num*2)
  double precision :: mat_mwen(mo_tot_num, mo_tot_num) 
  double precision, intent(in) :: coefs(N_states)
  integer, intent(in) :: h1, h2, s1, s2
  integer(bit_kind), intent(in) :: mask(N_int, 2), gen(N_int, 2)
  integer(bit_kind) :: det1(N_int, 2), det2(N_int, 2)
  logical :: ok, mono
  double precision :: phase, phase2, inv, hij
  integer :: p1, p2, hmi, hma, ns1, ns2, st
  logical, external :: detEq
  integer :: exc(0:2, 2, 2), exc2(0:2,2,2)
  integer :: tmp_array(4)
  
  exc = 0
!   mat_mwen = integral8(:,:,h1,h2)
  !call get_mo_bielec_integrals_ij(h1, h2 ,mo_tot_num,mat_mwen,mo_integrals_map)
  
  ns1 = mo_tot_num*(s1-1)
  ns2 = mo_tot_num*(s2-1)
  
  !mat = 0d0 
  if(s1 == s2) then
    hmi = min(h1, h2)
    hma = max(h1, h2)
    inv = 1d0
    if(h1 > h2) inv = -1d0
    exc(0, :, s1) = 2
    exc(1, 1, s1) = hmi
    exc(2, 1, s1) = hma
    do p2=1,mo_tot_num
    if(banned(p2 + ns2)) cycle
    do p1=1,mo_tot_num
      if(banned(p1 + ns1)) cycle
      if(p1 == p2) cycle
      if(banned_pair(p1 + ns1, p2 + ns2)) cycle
      tmp_array = (/s1,p1,s2,p2/)
      call apply_particle(mask, tmp_array, det2, ok, N_int)
      if(.not. ok) cycle
      mono = (hmi == p1 .or. hma == p2 .or. hmi == p2 .or. hma == p1)
      if(mono) then
        
        call i_h_j(gen, det2, N_int, hij)
        do st=1, N_states
          mat(p1, p2, st) += hij * coefs(st)
        end do
      else
        exc(1, 2, s1) = min(p1, p2)
        exc(2, 2, s2) = max(p2, p1)
        call get_double_excitation_phase(gen, det2, exc, phase, N_int)
        do st=1, N_states
          mat(p1, p2, st) += coefs(st) * inv * (integral8(p1, p2, h1, h2) - integral8(p2, p1, h1, h2)) * phase
        end do
      end if
    end do
    end do
  else
    exc(0, :, 1) = 1
    exc(0, :, 2) = 1
    if(s1 /= 2) stop "alpha beta inversified"
    exc(1, 1, 1) = h2
    exc(1, 1, 2) = h1

    do p2=1, mo_tot_num
    if(banned(p2 + ns2)) cycle
    do p1=1, mo_tot_num
      if(banned(p1 + ns1)) cycle
      if(banned_pair(p1 + ns1, p2 + ns2)) cycle
      tmp_array = (/s1,p1,s2,p2/)
      call apply_particle(mask, tmp_array, det2, ok, N_int)
      if(.not. ok) cycle
      mono = (h1 == p1 .or. h2 == p2)
      if(mono) then
        call i_h_j(gen, det2, N_int, hij)
        do st=1, N_states
          mat(p1, p2, st) += hij * coefs(st)
        end do
      else
        exc(1, 2, s1) = p1
        exc(1, 2, s2) = p2
        call get_double_excitation_phase(gen, det2, exc, phase, N_int)
        do st=1, N_states
          mat(p1, p2, st) += coefs(st) * integral8(p1, p2, h1, h2) * phase
        end do
        !mat(p1, p2) = integral8(p1, p2, h1, h2) * phase
      end if
    end do
    end do
  end if
end subroutine


subroutine apply_particle(det, exc, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: exc(4)
  integer :: s1, s2, p1, p2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  s1 = exc(1)
  p1 = exc(2)
  s2 = exc(3)
  p2 = exc(4)
  res = det 
  
  if(p1 /= 0) then
  ii = (p1-1)/bit_kind_size + 1 
  pos = mod(p1-1, 64)!iand(p1-1,bit_kind_size-1)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s1) = ibset(res(ii, s1), pos)
  end if

  ii = (p2-1)/bit_kind_size + 1 
  pos = mod(p2-1, 64)!iand(p2-1,bit_kind_size-1)
  if(iand(det(ii, s2), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s2) = ibset(res(ii, s2), pos)

  ok = .true.
end subroutine


subroutine apply_hole(det, exc, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: exc(4)
  integer :: s1, s2, p1, p2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  s1 = exc(1)
  p1 = exc(2)
  s2 = exc(3)
  p2 = exc(4)
  res = det 
  
  if(p1 /= 0) then
  ii = (p1-1)/bit_kind_size + 1 
  pos = mod(p1-1, 64)!iand(p1-1,bit_kind_size-1)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s1) = ibclr(res(ii, s1), pos)
  end if

  ii = (p2-1)/bit_kind_size + 1 
  pos = mod(p2-1, 64)!iand(p2-1,bit_kind_size-1)
  if(iand(det(ii, s2), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s2) = ibclr(res(ii, s2), pos)

  ok = .true.
end subroutine



subroutine get_double_excitation_phase(det1,det2,exc,phase,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns the two excitation operators between two doubly excited determinants and the phase
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: det1(Nint,2)
  integer(bit_kind), intent(in)  :: det2(Nint,2)
  integer, intent(in)           :: exc(0:2,2,2)
  double precision, intent(out)  :: phase
  integer                        :: tz
  integer                        :: l, ispin, idx_hole, idx_particle, ishift
  integer                        :: nperm
  integer                        :: i,j,k,m,n
  integer                        :: high, low
  integer                        :: a,b,c,d
  integer(bit_kind)              :: hole, particle, tmp
  double precision, parameter    :: phase_dble(0:1) = (/ 1.d0, -1.d0 /)

  ASSERT (Nint > 0)
  nperm = 0
  do ispin = 1,2
    select case (exc(0,1,ispin))
      case(0)
        cycle

      case(1)
        low  = min(exc(1,1,ispin), exc(1,2,ispin))
        high = max(exc(1,1,ispin), exc(1,2,ispin))

        ASSERT (low > 0)
        j = ishft(low-1,-bit_kind_shift)+1   ! Find integer in array(Nint)
        n = iand(low-1,bit_kind_size-1)+1        ! mod(low,bit_kind_size)
        ASSERT (high > 0)
        k = ishft(high-1,-bit_kind_shift)+1
        m = iand(high-1,bit_kind_size-1)+1

        if (j==k) then
          nperm = nperm + popcnt(iand(det1(j,ispin),                 &
              iand( ibset(0_bit_kind,m-1)-1_bit_kind,                &
              ibclr(-1_bit_kind,n)+1_bit_kind ) ))
        else
          nperm = nperm + popcnt(iand(det1(k,ispin),                 &
              ibset(0_bit_kind,m-1)-1_bit_kind))
          if (n < bit_kind_size) then
              nperm = nperm + popcnt(iand(det1(j,ispin), ibclr(-1_bit_kind,n) +1_bit_kind))
          endif
          do i=j+1,k-1
            nperm = nperm + popcnt(det1(i,ispin))
          end do
        endif

      case (2)

        do i=1,2
          low  = min(exc(i,1,ispin), exc(i,2,ispin))
          high = max(exc(i,1,ispin), exc(i,2,ispin))

          ASSERT (low > 0)
          j = ishft(low-1,-bit_kind_shift)+1   ! Find integer in array(Nint)
          n = iand(low-1,bit_kind_size-1)+1        ! mod(low,bit_kind_size)
          ASSERT (high > 0)
          k = ishft(high-1,-bit_kind_shift)+1
          m = iand(high-1,bit_kind_size-1)+1

          if (j==k) then
            nperm = nperm + popcnt(iand(det1(j,ispin),               &
                iand( ibset(0_bit_kind,m-1)-1_bit_kind,              &
                ibclr(-1_bit_kind,n)+1_bit_kind ) ))
          else
            nperm = nperm + popcnt(iand(det1(k,ispin),               &
                ibset(0_bit_kind,m-1)-1_bit_kind))
            if (n < bit_kind_size) then
               nperm = nperm + popcnt(iand(det1(j,ispin), ibclr(-1_bit_kind,n) +1_bit_kind))
            endif
            do l=j+1,k-1
              nperm = nperm + popcnt(det1(l,ispin))
            end do
          endif

        enddo

        a = min(exc(1,1,ispin), exc(1,2,ispin))
        b = max(exc(1,1,ispin), exc(1,2,ispin))
        c = min(exc(2,1,ispin), exc(2,2,ispin))
        d = max(exc(2,1,ispin), exc(2,2,ispin))
        if (c>a .and. c<b .and. d>b) then
          nperm = nperm + 1
        endif
        exit
    end select

  enddo
  phase = phase_dble(iand(nperm,1))
end



subroutine check_past(det, list, idx, N, cur, ok, Nint)
  implicit none
  use bitmasks

  integer(bit_kind), intent(in) :: det(Nint, 2), list(Nint, 2, N)
  integer, intent(in) :: Nint, idx(N), N, cur
  logical, intent(out) :: ok
  integer :: i,s,ni

  ok = .false.
  do i=1,N
    if(idx(i) >= cur) exit
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


subroutine check_past_s(det, list, N, ok, Nint)
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

