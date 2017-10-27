subroutine broadcast_wf(energy)
  implicit none
  BEGIN_DOC
  ! Segment corresponding to the wave function. This is segment 0.
  END_DOC
  use bitmasks
  use GASPI
  use ISO_C_BINDING
  
  double precision, intent(inout) :: energy(N_states)
  integer(gaspi_return_t)        :: res
  
  if (is_gaspi_master) then
    call broadcast_wf_put(energy)
  else
    call broadcast_wf_get(energy)
  endif

  res = gaspi_barrier(GASPI_GROUP_ALL, GASPI_BLOCK)
  if(res .ne. GASPI_SUCCESS) then
     write(*,*) "gaspi_barrier failed"
     stop -1
  end if

  
  integer(gaspi_segment_id_t)    :: seg_id
  do seg_id=0,3
    res = gaspi_segment_delete(seg_id)
    if(res .ne. GASPI_SUCCESS) then
      write(*,*) "gaspi_segment_delete failed", seg_id
      stop -1
    end if
  end do

end





subroutine broadcast_wf_put(energy)
  implicit none
  BEGIN_DOC
  ! Initiates the broadcast of the wave function
  END_DOC
  use bitmasks
  use GASPI
  use ISO_C_BINDING
  
  double precision, intent(in)    :: energy(N_states)
  integer(gaspi_segment_id_t)    :: seg_id
  integer(gaspi_alloc_t)         :: seg_alloc_policy
  integer(gaspi_size_t)          :: seg_size(0:3)
  type(c_ptr)                    :: seg_ptr(0:3)
  integer, pointer               :: params_int(:)       ! Segment 0
  double precision, pointer      :: psi_coef_tmp(:,:)   ! Segment 1
  integer(bit_kind), pointer     :: psi_det_tmp(:,:,:)  ! Segment 2
  double precision, pointer      :: params_double(:)    ! Segment 3
  
  integer(gaspi_return_t)        :: res
  
  
  seg_alloc_policy = GASPI_MEM_UNINITIALIZED

  seg_size(0) = 4 * 5 
  seg_id=0
  res = gaspi_segment_create(seg_id, seg_size(seg_id), GASPI_GROUP_ALL, &
      GASPI_BLOCK, seg_alloc_policy)
  if(res .ne. GASPI_SUCCESS) then
    write(*,*) "gaspi_create_segment failed", gaspi_rank, seg_id
    stop -1
  end if

  res = gaspi_segment_ptr(seg_id, seg_ptr(seg_id))
  if(res .ne. GASPI_SUCCESS) then
    write(*,*) "gaspi_segment_ptr failed", gaspi_rank
    stop -1
  end if

  call c_f_pointer(seg_ptr(0), params_int, shape=(/ 5 /))
  params_int(1) = N_states
  params_int(2) = N_det
  params_int(3) = psi_det_size

  res = gaspi_barrier(GASPI_GROUP_ALL, GASPI_BLOCK)
  if(res .ne. GASPI_SUCCESS) then
     write(*,*) "gaspi_barrier failed", gaspi_rank
     stop -1
  end if

  seg_size(1) = 8 * psi_det_size * N_states
  seg_size(2) = bit_kind * psi_det_size * 2 * N_int
  seg_size(3) = 8 * N_states

  do seg_id=1, 3
    res = gaspi_segment_create(seg_id, seg_size(seg_id), GASPI_GROUP_ALL, &
        GASPI_BLOCK, seg_alloc_policy)
    if(res .ne. GASPI_SUCCESS) then
      write(*,*) "gaspi_create_segment failed", gaspi_rank, seg_id
      stop -1
    end if

    res = gaspi_segment_ptr(seg_id, seg_ptr(seg_id))
    if(res .ne. GASPI_SUCCESS) then
      write(*,*) "gaspi_segment_ptr failed", gaspi_rank
      stop -1
    end if
  end do

  call c_f_pointer(seg_ptr(1), psi_coef_tmp, shape=shape(psi_coef))
  call c_f_pointer(seg_ptr(2), psi_det_tmp, shape=shape(psi_det))
  call c_f_pointer(seg_ptr(3), params_double, shape=(/ N_states /))

  psi_coef_tmp = psi_coef
  psi_det_tmp  = psi_det
  params_double = energy

  res = gaspi_barrier(GASPI_GROUP_ALL, GASPI_BLOCK)
  if(res .ne. GASPI_SUCCESS) then
     write(*,*) "gaspi_barrier failed", gaspi_rank
     stop -1
  end if

end







subroutine broadcast_wf_get(energy)
  implicit none
  BEGIN_DOC
  ! Gets the broadcasted wave function
  END_DOC
  use bitmasks
  use GASPI
  use ISO_C_BINDING
  
  double precision, intent(out)  :: energy(N_states)
  integer(gaspi_segment_id_t)    :: seg_id
  integer(gaspi_alloc_t)         :: seg_alloc_policy
  integer(gaspi_size_t)          :: seg_size(0:3)
  type(c_ptr)                    :: seg_ptr(0:3)
  integer, pointer               :: params_int(:)       ! Segment 0
  double precision, pointer      :: psi_coef_tmp(:,:)   ! Segment 1
  integer(bit_kind), pointer     :: psi_det_tmp(:,:,:)  ! Segment 2
  double precision, pointer      :: params_double(:)    ! Segment 3
  
  integer(gaspi_return_t)        :: res
  
  
  seg_alloc_policy = GASPI_MEM_UNINITIALIZED
  
  seg_size(0) = 4 * 5
  seg_id=0
  res = gaspi_segment_create(seg_id, seg_size(seg_id), GASPI_GROUP_ALL,&
      GASPI_BLOCK, seg_alloc_policy)
  if(res .ne. GASPI_SUCCESS) then
    write(*,*) "gaspi_create_segment failed"
    stop -1
  end if
  
  res = gaspi_segment_ptr(seg_id, seg_ptr(seg_id))
  if(res .ne. GASPI_SUCCESS) then
    write(*,*) "gaspi_segment_ptr failed"
    stop -1
  end if
  
  res = gaspi_barrier(GASPI_GROUP_ALL, GASPI_BLOCK)
  if(res .ne. GASPI_SUCCESS) then
     write(*,*) "gaspi_barrier failed"
     stop -1
  end if

  integer(gaspi_offset_t)        :: localOff, remoteOff
  integer(gaspi_rank_t)          :: remoteRank
  integer(gaspi_queue_id_t)      :: queue
  localOff = 0
  remoteRank = 0
  queue = 0
  res = gaspi_read(seg_id, localOff, remoteRank,                     &
      seg_id, remoteOff, seg_size(seg_id), queue, GASPI_BLOCK)
  if(res .ne. GASPI_SUCCESS) then
    write(*,*) "gaspi_read failed"
    stop -1
  end if

  res = gaspi_wait(queue, GASPI_BLOCK)
  if(res .ne. GASPI_SUCCESS) then
    write(*,*) "gaspi_wait failed"
    stop -1
  end if

  call c_f_pointer(seg_ptr(0), params_int, shape=shape( (/ 5 /) ))

  N_states         = params_int(1)  
  N_det            = params_int(2)  
  psi_det_size     = params_int(3)  
  TOUCH N_states N_det psi_det_size

  seg_size(1) = 8 * psi_det_size * N_states
  seg_size(2) = bit_kind * psi_det_size * 2 * N_int
  seg_size(3) = 8 * N_states

  do seg_id=1, 3
    res = gaspi_segment_create(seg_id, seg_size(seg_id), GASPI_GROUP_ALL, &
        GASPI_BLOCK, seg_alloc_policy)
    if(res .ne. GASPI_SUCCESS) then
      write(*,*) "gaspi_create_segment failed"
      stop -1
    end if

    res = gaspi_segment_ptr(seg_id, seg_ptr(seg_id))
    if(res .ne. GASPI_SUCCESS) then
      write(*,*) "gaspi_segment_ptr failed"
      stop -1
    end if
  end do

  res = gaspi_barrier(GASPI_GROUP_ALL, GASPI_BLOCK)
  if(res .ne. GASPI_SUCCESS) then
     write(*,*) "gaspi_barrier failed"
     stop -1
  end if

  do seg_id=1, 3
    res = gaspi_read(seg_id, localOff, remoteRank,                     &
        seg_id, remoteOff, seg_size(seg_id), queue, GASPI_BLOCK)
    if(res .ne. GASPI_SUCCESS) then
      write(*,*) "gaspi_read failed"
      stop -1
    end if
    res = gaspi_wait(queue, GASPI_BLOCK)
    if(res .ne. GASPI_SUCCESS) then
      write(*,*) "gaspi_wait failed"
      stop -1
    end if
  end do

  call c_f_pointer(seg_ptr(1), psi_coef_tmp, shape=shape(psi_coef))
  call c_f_pointer(seg_ptr(2), psi_det_tmp, shape=shape(psi_det))
  call c_f_pointer(seg_ptr(3), params_double, shape=shape(energy))

  psi_coef = psi_coef_tmp
  psi_det  = psi_det_tmp
  energy   = params_double

end




