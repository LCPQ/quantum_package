integer function zmq_put_psi(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put the wave function on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  character*(256)                :: msg

  integer, external              :: zmq_put_N_states
  integer, external              :: zmq_put_N_det
  integer, external              :: zmq_put_psi_det_size
  integer, external              :: zmq_put_psi_det
  integer, external              :: zmq_put_psi_coef

  zmq_put_psi = 0
  if (zmq_put_N_states(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_put_psi = -1
    return
  endif
  if (zmq_put_N_det(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_put_psi = -1
    return
  endif
  if (zmq_put_psi_det_size(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_put_psi = -1
    return
  endif
  if (zmq_put_psi_det(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_put_psi = -1
    return
  endif
  if (zmq_put_psi_coef(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_put_psi = -1
    return
  endif

end



BEGIN_TEMPLATE 

integer function zmq_put_$X(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put $X on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  character*(256)                :: msg

  zmq_put_$X = 0

  write(msg,'(A,1X,I8,1X,A200)') 'put_data '//trim(zmq_state), worker_id, '$X'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    zmq_put_$X = -1
    return
  endif

  rc = f77_zmq_send(zmq_to_qp_run_socket,$X,4,0)
  if (rc /= 4) then
    zmq_put_$X = -1
    return
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_data_reply ok') then
    zmq_put_$X = -1
    return
  endif

end

integer function zmq_get_$X(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get $X from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  character*(256)                :: msg

  PROVIDE zmq_state
  zmq_get_$X = 0
  if (mpi_master) then
    write(msg,'(A,1X,I8,1X,A200)') 'get_data '//trim(zmq_state), worker_id, '$X'
    rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
    if (rc /= len(trim(msg))) then
      zmq_get_$X = -1
      go to 10
    endif

    rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
    if (msg(1:14) /= 'get_data_reply') then
      zmq_get_$X = -1
      go to 10
    endif

    rc = f77_zmq_recv(zmq_to_qp_run_socket,$X,4,0)
    if (rc /= 4) then
      zmq_get_$X = -1
      go to 10
    endif

  endif

 10 continue
  IRP_IF MPI
    include 'mpif.h'
    integer :: ierr

    call MPI_BCAST (zmq_get_$X, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      stop 'Unable to broadcast zmq_get_psi_det'
    endif
    call MPI_BCAST ($X, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      stop 'Unable to broadcast zmq_get_psi_det'
    endif
  IRP_ENDIF

end

SUBST [ X ]

N_states ;;
N_det ;;
psi_det_size ;;

END_TEMPLATE

integer function zmq_put_psi_det(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put psi_det on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  zmq_put_psi_det = 0

  write(msg,'(A,1X,I8,1X,A200)') 'put_data '//trim(zmq_state), worker_id, 'psi_det'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    zmq_put_psi_det = -1
    return
  endif

  rc8 = f77_zmq_send8(zmq_to_qp_run_socket,psi_det,int(N_int*2_8*N_det*bit_kind,8),0)
  if (rc8 /= N_int*2_8*N_det*bit_kind) then
    zmq_put_psi_det = -1
    return
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_data_reply ok') then
    zmq_put_psi_det = -1
    return
  endif
end

integer function zmq_put_psi_coef(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put psi_coef on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  zmq_put_psi_coef = 0

  write(msg,'(A,1X,I8,1X,A200)') 'put_data '//trim(zmq_state), worker_id, 'psi_coef'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    zmq_put_psi_coef = -1
    return
  endif

  rc8 = f77_zmq_send8(zmq_to_qp_run_socket,psi_coef,int(psi_det_size*N_states*8_8,8),0)
  if (rc8 /= psi_det_size*N_states*8_8) then
    zmq_put_psi_coef = -1
    return
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_data_reply ok') then
    zmq_put_psi_coef = -1
    return
  endif

end

!---------------------------------------------------------------------------


integer function zmq_get_psi(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get the wave function from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id

  integer, external              :: zmq_get_N_states
  integer, external              :: zmq_get_N_det
  integer, external              :: zmq_get_psi_det_size
  integer, external              :: zmq_get_psi_det
  integer, external              :: zmq_get_psi_coef

  zmq_get_psi = 0

  if (zmq_get_N_states(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_get_psi = -1
    return
  endif
  if (zmq_get_N_det(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_get_psi = -1
    return
  endif
  if (zmq_get_psi_det_size(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_get_psi = -1
    return
  endif
  
  if (size(psi_det) /= N_int*2_8*psi_det_size*bit_kind) then
    deallocate(psi_det)
    allocate(psi_det(N_int,2,psi_det_size))
  endif

  if (size(psi_coef) /= psi_det_size*N_states) then
    deallocate(psi_coef)
    allocate(psi_coef(psi_det_size,N_states))
  endif

  if (zmq_get_psi_det(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_get_psi = -1
    return
  endif
  if (zmq_get_psi_coef(zmq_to_qp_run_socket, worker_id) == -1) then
    zmq_get_psi = -1
    return
  endif
  SOFT_TOUCH psi_det psi_coef psi_det_size N_det N_states

end


integer function zmq_get_psi_det(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get psi_det from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  PROVIDE zmq_state
  zmq_get_psi_det = 0
  if (mpi_master) then
    write(msg,'(A,1X,I8,1X,A200)') 'get_data '//trim(zmq_state), worker_id, 'psi_det'
    rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
    if (rc /= len(trim(msg))) then
      zmq_get_psi_det = -1
      go to 10
    endif

    rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
    if (msg(1:14) /= 'get_data_reply') then
      zmq_get_psi_det = -1
      go to 10
    endif

    rc8 = f77_zmq_recv8(zmq_to_qp_run_socket,psi_det,int(N_int*2_8*N_det*bit_kind,8),0)
    if (rc8 /= N_int*2_8*N_det*bit_kind) then
      zmq_get_psi_det = -1
      go to 10
    endif
  endif

  10 continue
  IRP_IF MPI
    include 'mpif.h'
    integer :: ierr
    call MPI_BCAST (zmq_get_psi_det, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      stop 'Unable to broadcast zmq_get_psi_det'
    endif
    call broadcast_chunks_bit_kind(psi_det,N_det*N_int*2)
  IRP_ENDIF

end

integer function zmq_get_psi_coef(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get psi_coef from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  PROVIDE zmq_state psi_det_size
  zmq_get_psi_coef = 0
  if (mpi_master) then
    write(msg,'(A,1X,I8,1X,A200)') 'get_data '//trim(zmq_state), worker_id, 'psi_coef'
    rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
    if (rc /= len(trim(msg))) then
      zmq_get_psi_coef = -1
      go to 10
    endif

    rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
    if (msg(1:14) /= 'get_data_reply') then
      zmq_get_psi_coef = -1
      go to 10
    endif

    rc8 = f77_zmq_recv8(zmq_to_qp_run_socket,psi_coef,int(psi_det_size*N_states*8_8,8),0)
    if (rc8 /= psi_det_size*N_states*8_8) then
      zmq_get_psi_coef = -1
      go to 10
    endif
  endif
  
  10 continue

  IRP_IF MPI
    include 'mpif.h'
    integer :: ierr
    call MPI_BCAST (zmq_get_psi_coef, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      stop 'Unable to broadcast zmq_get_psi_coef'
    endif
    call broadcast_chunks_double(psi_coef,N_states*N_det)
  IRP_ENDIF

end


