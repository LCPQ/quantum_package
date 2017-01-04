program fci_zmq
  implicit none
  integer                        :: i,j,k
  logical, external              :: detEq
  
  double precision, allocatable  :: pt2(:)
  integer                        :: degree
  
  allocate (pt2(N_states))
  
  pt2 = 1.d0
  diag_algorithm = "Lapack"
  
  if (N_det > N_det_max) then
    call diagonalize_CI
    call save_wavefunction
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = N_det_max
    soft_touch N_det psi_det psi_coef
    call diagonalize_CI
    call save_wavefunction
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    do k=1,N_states
      print*,'State ',k
      print *,  'PT2      = ', pt2(k)
      print *,  'E        = ', CI_energy(k)
      print *,  'E+PT2    = ', CI_energy(k) + pt2(k)
      print *,  '-----'
    enddo
  endif
  double precision               :: E_CI_before(N_states)
  
  
  integer                        :: n_det_before
  print*,'Beginning the selection ...'
  E_CI_before(1:N_states) = CI_energy(1:N_states)
  
  do while ( (N_det < N_det_max) .and. (maxval(abs(pt2(1:N_states))) > pt2_max) )
    n_det_before = N_det
    call ZMQ_selection(max(1024-N_det, N_det), pt2)
    
    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    call diagonalize_CI
    call save_wavefunction
    
    if (N_det > N_det_max) then
      psi_det = psi_det_sorted
      psi_coef = psi_coef_sorted
      N_det = N_det_max
      soft_touch N_det psi_det psi_coef
      call diagonalize_CI
      call save_wavefunction
    endif
    
    print *,  'N_det          = ', N_det
    print *,  'N_states       = ', N_states
    do k=1, N_states
      print*,'State ',k
      print *,  'PT2            = ', pt2(k)
      print *,  'E              = ', CI_energy(k)
      print *,  'E(before)+PT2  = ', E_CI_before(k)+pt2(k)
    enddo
    print *,  '-----'
    if(N_states.gt.1)then
      print*,'Variational Energy difference'
      do i = 2, N_states
        print*,'Delta E = ',CI_energy(i) - CI_energy(1)
      enddo
    endif
    if(N_states.gt.1)then
      print*,'Variational + perturbative Energy difference'
      do i = 2, N_states
        print*,'Delta E = ',E_CI_before(i)+ pt2(i) - (E_CI_before(1) + pt2(1))
      enddo
    endif
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    call ezfio_set_full_ci_energy(CI_energy)
  enddo

  if(do_pt2_end)then
    print*,'Last iteration only to compute the PT2'
    threshold_selectors = 1.d0
    threshold_generators = 1d0 ! 0.9999d0
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    !call ZMQ_selection(0, pt2) pour non-stochastic
    call ZMQ_pt2(pt2)
    print *,  'Final step'
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    do k=1,N_states
      print *, 'State', k
      print *,  'PT2      = ', pt2
      print *,  'E        = ', E_CI_before
      print *,  'E+PT2    = ', E_CI_before+pt2
      print *,  '-----'
    enddo
    call ezfio_set_full_ci_energy_pt2(E_CI_before+pt2)
  endif
  call save_wavefunction
end

! subroutine ZMQ_pt2(pt2)
!   use f77_zmq
!   use selection_types
!   
!   implicit none
!   
!   character*(1000000)                :: task 
!   integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
!   type(selection_buffer)         :: b
!   integer                        :: i, N
!   integer, external              :: omp_get_thread_num
!   double precision, intent(out)  :: pt2(N_states)
!   
!   integer*8, allocatable :: bulk(:), tirage(:)
!   integer, allocatable :: todo(:)
!   double precision, allocatable :: pt2_detail(:,:), val(:,:), weight(:)
!   double precision :: sume, sume2
!   double precision :: tot_n
!   
!   allocate(bulk(N_det), tirage(N_det), todo(0:N_det), pt2_detail(N_states, N_det), val(N_states, N_det))
!   
!   sume = 0d0
!   sume2 = 0d0
!   tot_n = 0d0
!   bulk = 0
!   tirage = 0
!   todo = 0
!   
!   
!   N = 1
!   provide nproc
!   provide ci_electronic_energy
!   call new_parallel_job(zmq_to_qp_run_socket,"pt2")
!   call zmq_put_psi(zmq_to_qp_run_socket,1,ci_electronic_energy,size(ci_electronic_energy))
!   call zmq_set_running(zmq_to_qp_run_socket)
!   call create_selection_buffer(N, N*2, b)
! 
!   integer :: i_generator, i_generator_end, generator_per_task, step
!   
!   integer :: mergeN
!   mergeN = 100
!   call get_carlo_workbatch(tirage, weight, todo, bulk, 1d-2, mergeN)
!   print *, "CARLO", todo(0), mergeN 
!   
!   generator_per_task = todo(0)/1000 + 1
!   do i=1,todo(0),generator_per_task
!     i_generator_end = min(i+generator_per_task-1, todo(0))
!     print *, "TASK", (i_generator_end-i+1), todo(i:i_generator_end)
!     write(task,*) (i_generator_end-i+1), todo(i:i_generator_end)
!     call add_task_to_taskserver(zmq_to_qp_run_socket,task)
!   end do
!   print *, "tasked"
!   pt2_detail = 0d0
!   !$OMP PARALLEL DEFAULT(shared)  SHARED(b, pt2)  PRIVATE(i) NUM_THREADS(nproc+1)
!     i = omp_get_thread_num()
!     if (i==0) then
!       call pt2_collector(b, pt2_detail)
!     else
!       call pt2_slave_inproc(i)
!     endif
!   !$OMP END PARALLEL
!   call end_parallel_job(zmq_to_qp_run_socket, 'pt2') 
!   print *, "daune"
!   val += pt2_detail
!   call perform_carlo(tirage, weight, bulk, val, sume, sume2, mergeN)
!   tot_n = 0
!   double precision :: sweight
!   sweight = 0d0
!   do i=1,N_det
!     if(weight(i) /= 0) tot_n = tot_n + dfloat(bulk(i))
!     sweight += weight(i)
!   end do
!   print *, "PT2_DETAIL", tot_n, sume/tot_n, sume, sume2
!   pt2 = 0d0
!   do i=1,N_det
!     if(weight(i) /= 0d0) exit
!     pt2(:) += pt2_detail(:,i)
!   end do
!   print *, "N_determinist = ", i-1
! end subroutine


subroutine ZMQ_pt2(pt2)
  use f77_zmq
  use selection_types
  
  implicit none
  
  character*(1000000)            :: task 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
  type(selection_buffer)         :: b
  integer, external              :: omp_get_thread_num
  double precision, intent(out)  :: pt2(N_states)

  
  double precision :: pt2_detail(N_states, N_det_generators), comb(100000)
  logical :: computed(N_det_generators)
  integer :: tbc(0:N_det_generators)
  integer :: i, Ncomb, generator_per_task, i_generator_end
  integer, external :: pt2_find
  
  double precision :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  double precision, external :: omp_get_wtime 
  double precision :: time0, time

  
  provide nproc

  call random_seed()
  
  computed = .false.
  tbc(0) = first_det_of_comb - 1
  do i=1, tbc(0)
    tbc(i) = i
    computed(i) = .true.
  end do
  pt2_detail = 0d0

  time0 = omp_get_wtime()
  print *, "grep - time - avg - err - n_combs"
  do while(.true.)
  
  call new_parallel_job(zmq_to_qp_run_socket,"pt2")
  call zmq_put_psi(zmq_to_qp_run_socket,1,ci_electronic_energy,size(ci_electronic_energy))
  call zmq_set_running(zmq_to_qp_run_socket)
  call create_selection_buffer(1, 1*2, b)
   





  call get_carlo_workbatch(1d-3, computed, comb, Ncomb, tbc)
  generator_per_task = tbc(0)/1000 + 1
  do i=1,tbc(0),generator_per_task
    i_generator_end = min(i+generator_per_task-1, tbc(0))
    !print *, "TASK", (i_generator_end-i+1), tbc(i:i_generator_end)
    write(task,*) (i_generator_end-i+1), tbc(i:i_generator_end)
    call add_task_to_taskserver(zmq_to_qp_run_socket,task)
  end do
  
  !$OMP PARALLEL DEFAULT(shared)  SHARED(b, pt2)  PRIVATE(i) NUM_THREADS(nproc+1)
    i = omp_get_thread_num()
    if (i==0) then
      call pt2_collector(b, pt2_detail)
    else
      call pt2_slave_inproc(i)
    endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, 'pt2')
  double precision :: E0, avg, eqt
  call do_carlo(tbc, Ncomb, comb, pt2_detail, sumabove, sum2above, Nabove)
  !END LOOP?
  integer :: tooth
  !-8.091550677158776E-003
  call get_first_tooth(computed, tooth)
  print *, "TOOTH ", tooth 
  
  !!! ASSERT
  do i=1,first_det_of_teeth(tooth)-1
    if(not(computed(i))) stop "deter non calc"
  end do
  logical :: ok
  ok = .false.
  do i=first_det_of_teeth(tooth), first_det_of_teeth(tooth+1)-1
    if(not(computed(i))) ok = .true.
  end do
  if(not(ok)) stop "not OK..."
  !!!!!

  if(Nabove(tooth) >= 30) then
    E0 = sum(pt2_detail(1,:first_det_of_teeth(tooth)-1))
    avg = E0 + (sumabove(tooth) / Nabove(tooth))
    eqt = sqrt(1d0 / (Nabove(tooth)-1) * abs(sum2above(tooth) / Nabove(tooth) - (sumabove(tooth)/Nabove(tooth))**2))
    time = omp_get_wtime()
    print "(A, 5(E15.7))", "PT2stoch ", time - time0, avg, eqt, Nabove(tooth)
  else
    print *, Nabove(tooth), "< 30 combs"
  end if
  tbc(0) = 0
  end do

  pt2 = 0d0
end subroutine


subroutine do_carlo(tbc, Ncomb, comb, pt2_detail, sumabove, sum2above, Nabove)
  integer, intent(in) :: tbc(0:N_det_generators), Ncomb
  double precision, intent(in) :: comb(Ncomb), pt2_detail(N_states, N_det_generators)
  double precision, intent(inout) :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  integer :: i, dets(comb_teeth)
  double precision :: myVal, myVal2


  do i=1,Ncomb
    call get_comb(comb(i), dets)
    myVal = 0d0
    myVal2 = 0d0
    do j=comb_teeth,1,-1
      !if(pt2_detail(1, dets(j)) == -1d0) print *, "uncalculatedidified", dets(j), pt2_detail(1, dets(j)-1:dets(j)+1)
      myVal += pt2_detail(1, dets(j)) / weight(dets(j)) * comb_step
      sumabove(j) += myVal
      sum2above(j) += myVal**2
      Nabove(j) += 1
    end do
  end do
end subroutine


subroutine ZMQ_selection(N_in, pt2)
  use f77_zmq
  use selection_types
  
  implicit none
  
  character*(1000000)                :: task 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
  integer, intent(in)            :: N_in
  type(selection_buffer)         :: b
  integer                        :: i, N
  integer, external              :: omp_get_thread_num
  double precision, intent(out)  :: pt2(N_states)
  
  
  N = max(N_in,1)
  provide nproc
  provide ci_electronic_energy
  call new_parallel_job(zmq_to_qp_run_socket,"selection")
  call zmq_put_psi(zmq_to_qp_run_socket,1,ci_electronic_energy,size(ci_electronic_energy))
  call zmq_set_running(zmq_to_qp_run_socket)
  call create_selection_buffer(N, N*2, b)

  integer :: i_generator, i_generator_start, i_generator_max, step
!  step = int(max(1.,10*elec_num/mo_tot_num)

  step = int(5000000.d0 / dble(N_int * N_states * elec_num * elec_num * mo_tot_num * mo_tot_num ))
  step = max(1,step)
  do i= N_det_generators, 1, -step
    i_generator_start = max(i-step+1,1)
    i_generator_max = i
    write(task,*) i_generator_start, i_generator_max, 1, N
    call add_task_to_taskserver(zmq_to_qp_run_socket,task)
  end do

    !$OMP PARALLEL DEFAULT(none)  SHARED(b, pt2)  PRIVATE(i) NUM_THREADS(nproc+1) shared(ci_electronic_energy_is_built, n_det_generators_is_built, n_states_is_built, n_int_is_built, nproc_is_built)
      i = omp_get_thread_num()
      if (i==0) then
        call selection_collector(b, pt2)
      else
        call selection_slave_inproc(i)
      endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, 'selection') 
  if (N_in > 0) then
    call fill_H_apply_buffer_no_selection(b%cur,b%det,N_int,0) !!! PAS DE ROBIN
    call copy_H_apply_buffer_to_wf()
  endif
end subroutine


subroutine selection_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call run_selection_slave(1,i,ci_electronic_energy)
end

subroutine pt2_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call run_pt2_slave(1,i,ci_electronic_energy)
end

subroutine pt2_collector(b, pt2_detail)
  use f77_zmq
  use selection_types
  use bitmasks
  implicit none


  type(selection_buffer), intent(inout) :: b
  double precision, intent(inout)       :: pt2_detail(N_states, N_det)
  double precision                   :: pt2_mwen(N_states, N_det)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull

  integer :: msg_size, rc, more
  integer :: acc, i, j, robin, N, ntask
  double precision, allocatable :: val(:)
  integer(bit_kind), allocatable :: det(:,:,:)
  integer, allocatable :: task_id(:)
  integer :: done, Nindex
  integer, allocatable :: index(:)
  real :: time, time0
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  allocate(val(b%N), det(N_int, 2, b%N), task_id(N_det), index(N_det))
  done = 0
  more = 1
  !pt2_detail = 0d0
  call CPU_TIME(time0)
  do while (more == 1)
    call pull_pt2_results(zmq_socket_pull, Nindex, index, pt2_mwen, task_id, ntask)
    do i=1,Nindex
      pt2_detail(:, index(i)) += pt2_mwen(:,i)
    end do
    
    !do i=1, N
    !  call add_to_selection_buffer(b, det(1,1,i), val(i))
    !end do

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



integer function pt2_find(v, w)
  implicit none
  double precision :: v, w(N_det)
  integer :: i,l,h

  l = 0
  h = N_det-1

  do while(h >= l)
    i = (h+l)/2
    if(w(i+1) > v) then
      h = i-1
    else
      l = i+1
    end if
  end do
  pt2_find = l+1
end function


BEGIN_PROVIDER [ integer, comb_teeth ]
  implicit none
  comb_teeth = 20
END_PROVIDER



subroutine get_first_tooth(computed, first_teeth)
  implicit none
  logical, intent(in) :: computed(N_det_generators)
  integer, intent(out) :: first_teeth
  integer :: i

  first_teeth = 1
  do i=first_det_of_comb, N_det_generators
    if(not(computed(i))) then
      first_teeth = i
      exit
    end if
  end do
  
  do i=comb_teeth, 1, -1
    if(first_det_of_teeth(i) <= first_teeth) then
      first_teeth = i
      exit
    end if
  end do
end subroutine


subroutine get_carlo_workbatch(maxWorkload, computed, comb, Ncomb, tbc)
  implicit none
  double precision, intent(in) :: maxWorkload
  double precision, intent(out) :: comb(N_det_generators)
  integer, intent(inout) :: tbc(0:N_det_generators)
  integer, intent(out) :: Ncomb
  logical, intent(inout) :: computed(N_det_generators)
  integer :: i, dets(comb_teeth)
  double precision :: myWorkload
  
  myWorkload = 0d0
  
  do i=1,size(comb)
    call RANDOM_NUMBER(comb(i))
    comb(i) = comb(i) * comb_step
    call add_comb(comb(i), computed, tbc, myWorkload)
    Ncomb = i
    if(myWorkload > maxWorkload) exit
  end do
end subroutine


subroutine get_comb(stato, dets)
  implicit none
  double precision, intent(in) :: stato
  integer, intent(out) :: dets(comb_teeth)
  double precision :: curs
  integer :: j
  integer, external :: pt2_find

  curs = 1d0 - stato
  do j = comb_teeth, 1, -1
    dets(j) = pt2_find(curs, cweight)
    curs -= comb_step
  end do
end subroutine


subroutine add_comb(comb, computed, tbc, workload)
  implicit none
  double precision, intent(in) :: comb
  logical, intent(inout) :: computed(N_det_generators)
  double precision, intent(inout) :: workload
  integer, intent(inout) :: tbc(0:N_det_generators)
  integer :: i, dets(comb_teeth)
  
  call get_comb(comb, dets)
  
  do i = 1, comb_teeth
    if(not(computed(dets(i)))) then
      tbc(0) += 1
      tbc(tbc(0)) = dets(i)
      workload += comb_workload(dets(i))
      computed(dets(i)) = .true.
    end if
  end do
end subroutine



 BEGIN_PROVIDER [ double precision, weight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, cweight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, comb_workload, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, comb_step ]
&BEGIN_PROVIDER [ integer, first_det_of_teeth, (comb_teeth+1) ]
&BEGIN_PROVIDER [ integer, first_det_of_comb ]
  implicit none
  integer :: i
  double precision :: norm_left, stato
  integer, external :: pt2_find  

  weight(1) = psi_coef_generators(1,1)**2
  cweight(1) = psi_coef_generators(1,1)**2
  
  do i=2,N_det_generators
    weight(i) = psi_coef_generators(i,1)**2
    cweight(i) = cweight(i-1) + psi_coef_generators(i,1)**2
  end do
  
  weight = weight / cweight(N_det_generators)
  cweight = cweight / cweight(N_det_generators)
  comb_workload = 1d0 / dfloat(N_det_generators)
  
  norm_left = 1d0
  
  comb_step = 1d0/dfloat(comb_teeth)
  do i=1,N_det_generators
    if(weight(i)/norm_left < comb_step/1d1) then
      first_det_of_comb = i
      exit
    end if
    norm_left -= weight(i)
  end do
  
  comb_step = 1d0 / dfloat(comb_teeth) * (1d0 - cweight(first_det_of_comb-1))
  
  stato = 1d0 - comb_step + 1d-5
  do i=comb_teeth, 1, -1
    first_det_of_teeth(i) = pt2_find(stato, cweight)
    stato -= comb_step
  end do
  first_det_of_teeth(comb_teeth+1) = N_det_generators + 1
  if(first_det_of_teeth(1) /= first_det_of_comb) stop "comb provider"
  
END_PROVIDER










