subroutine zmq_put_psi(zmq_to_qp_run_socket,worker_id, energy, size_energy)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put the wave function on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer, intent(in)            :: size_energy
  double precision, intent(out)  :: energy(size_energy)
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  write(msg,*) 'put_psi ', worker_id, N_states, N_det, psi_det_size, n_det_generators, n_det_selectors

  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    print *, 'f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)'
    stop 'error'
  endif

  rc8 = f77_zmq_send8(zmq_to_qp_run_socket,psi_det,N_int*2_8*N_det*bit_kind,ZMQ_SNDMORE)
  if (rc8 /= N_int*2_8*N_det*bit_kind) then
    print *, 'f77_zmq_send8(zmq_to_qp_run_socket,psi_det,N_int*2_8*N_det*bit_kind,ZMQ_SNDMORE)'
    stop 'error'
  endif

  rc8 = f77_zmq_send8(zmq_to_qp_run_socket,psi_coef,psi_det_size*N_states*8_8,ZMQ_SNDMORE)
  if (rc8 /= psi_det_size*N_states*8_8) then
    print *, 'f77_zmq_send8(zmq_to_qp_run_socket,psi_coef,psi_det_size*N_states*8_8,ZMQ_SNDMORE)'
    stop 'error'
  endif

  rc = f77_zmq_send(zmq_to_qp_run_socket,energy,size_energy*8,0)
  if (rc /= size_energy*8) then
    print *, 'f77_zmq_send(zmq_to_qp_run_socket,energy,size_energy*8,0)'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_psi_reply 1') then
    print *,  rc, trim(msg)
    print *,  'Error in put_psi_reply'
    stop 'error'
  endif

end



subroutine zmq_get_psi(zmq_to_qp_run_socket, worker_id, energy, size_energy)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get the wave function from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer, intent(in)            :: size_energy
  double precision, intent(out)  :: energy(size_energy)
  integer                        :: rc
  integer*8                      :: rc8
  character*(64)                 :: msg

  write(msg,*) 'get_psi ', worker_id

  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
  if (rc /= len(trim(msg))) then
    print *, 'f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:13) /= 'get_psi_reply') then
    print *,  rc, trim(msg)
    print *,  'Error in get_psi_reply'
    stop 'error'
  endif
  
  integer :: N_states_read, N_det_read, psi_det_size_read
  integer :: N_det_selectors_read, N_det_generators_read
  read(msg(14:rc),*) N_states_read, N_det_read, psi_det_size_read, &
    N_det_generators_read, N_det_selectors_read

  N_states = N_states_read
  N_det    = N_det_read
  psi_det_size = psi_det_size_read
  TOUCH psi_det_size N_det N_states

  rc8 = f77_zmq_recv8(zmq_to_qp_run_socket,psi_det,N_int*2_8*N_det*bit_kind,0)
  if (rc8 /= N_int*2_8*N_det*bit_kind) then
    print *, 'f77_zmq_recv(zmq_to_qp_run_socket,psi_det,N_int*2*N_det*bit_kind,ZMQ_SNDMORE)'
    stop 'error'
  endif

  rc8 = f77_zmq_recv8(zmq_to_qp_run_socket,psi_coef,psi_det_size*N_states*8_8,0)
  if (rc8 /= psi_det_size*N_states*8_8) then
    print *, '77_zmq_recv8(zmq_to_qp_run_socket,psi_coef,psi_det_size*N_states*8_8,ZMQ_SNDMORE)'
    stop 'error'
  endif
  TOUCH psi_det psi_coef

  rc = f77_zmq_recv(zmq_to_qp_run_socket,energy,size_energy*8,0)
  if (rc /= size_energy*8) then
    print *, '77_zmq_recv(zmq_to_qp_run_socket,energy,size_energy*8,0)'
    stop 'error'
  endif

  if (N_det_generators_read > 0) then
    N_det_generators = N_det_generators_read
    TOUCH N_det_generators
  endif
  if (N_det_selectors_read > 0) then
    N_det_selectors  = N_det_selectors_read
    TOUCH N_det_selectors
  endif

end


