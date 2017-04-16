
BEGIN_PROVIDER [ logical, ao_bielec_integrals_in_map ]
  implicit none
  use f77_zmq
  use map_module
  BEGIN_DOC
  !  Map of Atomic integrals
  !     i(r1) j(r2) 1/r12 k(r1) l(r2)
  END_DOC
  
  integer                        :: i,j,k,l
  double precision               :: ao_bielec_integral,cpu_1,cpu_2, wall_1, wall_2
  double precision               :: integral, wall_0
  include 'Utils/constants.include.F'
  
  ! For integrals file
  integer(key_kind),allocatable  :: buffer_i(:)
  integer,parameter              :: size_buffer = 1024*64
  real(integral_kind),allocatable :: buffer_value(:)
  
  integer                        :: n_integrals, rc
  integer                        :: kk, m, j1, i1, lmax
  character*(64)                 :: fmt
  
  integral = ao_bielec_integral(1,1,1,1)
  
  real                           :: map_mb
  PROVIDE read_ao_integrals disk_access_ao_integrals
  if (read_ao_integrals) then
    print*,'Reading the AO integrals'
      call map_load_from_disk(trim(ezfio_filename)//'/work/ao_ints',ao_integrals_map)
      print*, 'AO integrals provided'
      ao_bielec_integrals_in_map = .True.
      return
  endif
  
  print*, 'Providing the AO integrals'
  call wall_time(wall_0)
  call wall_time(wall_1)
  call cpu_time(cpu_1)

  integer(ZMQ_PTR) :: zmq_to_qp_run_socket
  call new_parallel_job(zmq_to_qp_run_socket,'ao_integrals')

  character(len=:), allocatable :: task
  allocate(character(len=ao_num*12) :: task)
  write(fmt,*) '(', ao_num, '(I5,X,I5,''|''))'
  do l=1,ao_num
    write(task,fmt) (i,l, i=1,l)
    call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task))
  enddo
  deallocate(task)
  
  call zmq_set_running(zmq_to_qp_run_socket)

  PROVIDE nproc
  !$OMP PARALLEL DEFAULT(private) num_threads(nproc+1)
      i = omp_get_thread_num()
      if (i==0) then
        call ao_bielec_integrals_in_map_collector(i)
      else
        call ao_bielec_integrals_in_map_slave_inproc(i)
      endif
  !$OMP END PARALLEL

  call end_parallel_job(zmq_to_qp_run_socket, 'ao_integrals')


  print*, 'Sorting the map'
  call map_sort(ao_integrals_map)
  call cpu_time(cpu_2)
  call wall_time(wall_2)
  integer(map_size_kind)         :: get_ao_map_size, ao_map_size
  ao_map_size = get_ao_map_size()
  
  print*, 'AO integrals provided:'
  print*, ' Size of AO map :         ', map_mb(ao_integrals_map) ,'MB'
  print*, ' Number of AO integrals :', ao_map_size
  print*, ' cpu  time :',cpu_2 - cpu_1, 's'
  print*, ' wall time :',wall_2 - wall_1, 's  ( x ', (cpu_2-cpu_1)/(wall_2-wall_1+tiny(1.d0)), ' )'
  
  ao_bielec_integrals_in_map = .True.

  if (write_ao_integrals) then
    call ezfio_set_work_empty(.False.)
    call map_save_to_disk(trim(ezfio_filename)//'/work/ao_ints',ao_integrals_map)
    call ezfio_set_integrals_erf_disk_access_ao_integrals("Read")
  endif
  
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, ao_bielec_integral_schwartz,(ao_num,ao_num)  ]
  implicit none
  BEGIN_DOC
  !  Needed to compute Schwartz inequalities
  END_DOC
  
  integer                        :: i,k
  double precision               :: ao_bielec_integral,cpu_1,cpu_2, wall_1, wall_2
  
  ao_bielec_integral_schwartz(1,1) = ao_bielec_integral(1,1,1,1)
  !$OMP PARALLEL DO PRIVATE(i,k)                                     &
      !$OMP DEFAULT(NONE)                                            &
      !$OMP SHARED (ao_num,ao_bielec_integral_schwartz)              &
      !$OMP SCHEDULE(dynamic)
  do i=1,ao_num
    do k=1,i
      ao_bielec_integral_schwartz(i,k) = dsqrt(ao_bielec_integral(i,k,i,k))
      ao_bielec_integral_schwartz(k,i) = ao_bielec_integral_schwartz(i,k)
    enddo
  enddo
  !$OMP END PARALLEL DO
  
END_PROVIDER


