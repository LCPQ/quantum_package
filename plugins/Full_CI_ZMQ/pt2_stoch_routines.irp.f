
 BEGIN_PROVIDER [ integer, fragment_count ]
&BEGIN_PROVIDER [ integer, fragment_first ]
  fragment_count = 8
  fragment_first = 4
END_PROVIDER

subroutine ZMQ_pt2(pt2,relative_error)
  use f77_zmq
  use selection_types
  
  implicit none
  
  character*(512)            :: task 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
  type(selection_buffer)         :: b
  integer, external              :: omp_get_thread_num
  double precision, intent(in)   :: relative_error
  double precision, intent(out)  :: pt2(N_states)

  
  double precision, allocatable :: pt2_detail(:,:), comb(:)
  logical, allocatable :: computed(:)
  integer, allocatable :: tbc(:)
  integer :: i, j, Ncomb, generator_per_task, i_generator_end
  integer, external :: pt2_find
  
  double precision :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  double precision, external :: omp_get_wtime 
  double precision :: time0, time

  allocate(pt2_detail(N_states, N_det_generators), comb(100000), computed(N_det_generators), tbc(0:N_det_generators))
  sumabove = 0d0
  sum2above = 0d0
  Nabove = 0d0

  provide nproc

  !call random_seed()
  
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
    call zmq_put_psi(zmq_to_qp_run_socket,1,pt2_e0_denominator,size(pt2_e0_denominator))
    call zmq_set_running(zmq_to_qp_run_socket)
    call create_selection_buffer(1, 1*2, b)
    
    ! TODO PARAMETER : 1.d-2
    call get_carlo_workbatch(1d-2, computed, comb, Ncomb, tbc)
    generator_per_task = 1
    do i=1,tbc(0)
      i_generator_end = min(i+generator_per_task-1, tbc(0))
      if(tbc(i) > fragment_first) then
        integer :: zero
        zero = 0
        write(task,*) (i_generator_end-i+1), zero, tbc(i:i_generator_end)
        call add_task_to_taskserver(zmq_to_qp_run_socket,task)
      else
        do j=1,fragment_count
          write(task,*) (i_generator_end-i+1), j, tbc(i:i_generator_end)
          call add_task_to_taskserver(zmq_to_qp_run_socket,task)
        end do
      end if
    end do

    !$OMP PARALLEL DEFAULT(shared)  SHARED(b, pt2, relative_error)  PRIVATE(i) NUM_THREADS(nproc+1)
      i = omp_get_thread_num()
      if (i==0) then
        call pt2_collector(b, tbc, comb, Ncomb, computed, pt2_detail, sumabove, sum2above, Nabove, relative_error, pt2)
      else
        call pt2_slave_inproc(i)
      endif
    !$OMP END PARALLEL
    call end_parallel_job(zmq_to_qp_run_socket, 'pt2')
    tbc(0) = 0
    if (pt2(1) /= 0.d0) then
      exit
    endif
  end do

end subroutine


subroutine do_carlo(tbc, Ncomb, comb, pt2_detail, computed, sumabove, sum2above, Nabove)
  integer, intent(in) :: tbc(0:N_det_generators), Ncomb
  logical, intent(in) :: computed(N_det_generators)
  double precision, intent(in) :: comb(Ncomb), pt2_detail(N_states, N_det_generators)
  double precision, intent(inout) :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  integer :: i, dets(comb_teeth)
  double precision :: myVal, myVal2

  mainLoop : do i=1,Ncomb
    call get_comb(comb(i), dets)
    do j=1,comb_teeth
      if(not(computed(dets(j)))) then
        exit mainLoop
      end if
    end do
    
    myVal = 0d0
    myVal2 = 0d0
    do j=comb_teeth,1,-1
      myVal += pt2_detail(1, dets(j)) / weight(dets(j)) * comb_step
      sumabove(j) += myVal
      sum2above(j) += myVal**2
      Nabove(j) += 1
    end do
  end do mainLoop
end subroutine


subroutine pt2_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call run_pt2_slave(1,i,pt2_e0_denominator)
end

subroutine pt2_collector(b, tbc, comb, Ncomb, computed, pt2_detail, sumabove, sum2above, Nabove, relative_error, pt2)
  use f77_zmq
  use selection_types
  use bitmasks
  implicit none

  
  integer, intent(in) :: Ncomb
  double precision, intent(inout) :: pt2_detail(N_states, N_det_generators)
  double precision, intent(in) :: comb(Ncomb)
  logical, intent(inout) :: computed(N_det_generators)
  integer, intent(in) :: tbc(0:N_det_generators)
  double precision, intent(inout) :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth), relative_error
  double precision, intent(out)  :: pt2(N_states)


  type(selection_buffer), intent(inout) :: b
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
  double precision, save :: time0 = -1.d0
  double precision :: time, timeLast
  double precision, external :: omp_get_wtime
  integer :: tooth, firstTBDcomb, orgTBDcomb
  integer, allocatable :: parts_to_get(:)
  logical, allocatable :: actually_computed(:)
  
  allocate(actually_computed(N_det_generators), parts_to_get(N_det_generators))
  actually_computed = computed
  
  parts_to_get(:) = 1
  if(fragment_first > 0) parts_to_get(1:fragment_first) = fragment_count

  do i=1,tbc(0)
    actually_computed(tbc(i)) = .false.
  end do
  
  orgTBDcomb = Nabove(1)
  firstTBDcomb = 1

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  allocate(val(b%N), det(N_int, 2, b%N), task_id(N_det), index(N_det))
  more = 1
  if (time0 < 0.d0) then
      time0 = omp_get_wtime()
  endif
  timeLast = time0
  
  print *, 'N_deterministic = ', first_det_of_teeth(1)-1
  pullLoop : do while (more == 1)
    call pull_pt2_results(zmq_socket_pull, Nindex, index, pt2_mwen, task_id, ntask)
    do i=1,Nindex
      pt2_detail(:, index(i)) += pt2_mwen(:,i)
      parts_to_get(index(i)) -= 1
      if(parts_to_get(index(i)) < 0) then 
        stop "PARTS ??"
      end if
      if(parts_to_get(index(i)) == 0) actually_computed(index(i)) = .true.
    end do

    do i=1, ntask
      if(task_id(i) == 0) then
          print *,  "Error in collector"
      endif
      call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id(i),more)
    end do

    time = omp_get_wtime()
  
    if(time - timeLast > 30.0 .or. more /= 1) then
      timeLast = time
      do i=1, first_det_of_teeth(1)-1
        if(not(actually_computed(i))) then
          print *, "PT2 : deterministic part not finished"
          cycle pullLoop
        end if
      end do
      
      double precision :: E0, avg, eqt, prop
      call do_carlo(tbc, Ncomb+1-firstTBDcomb, comb(firstTBDcomb), pt2_detail, actually_computed, sumabove, sum2above, Nabove)
      firstTBDcomb = Nabove(1) - orgTBDcomb + 1
      if(Nabove(1) < 2.0) cycle
      call get_first_tooth(actually_computed, tooth)
     
      done = 0
      do i=first_det_of_teeth(tooth), first_det_of_teeth(tooth+1)-1
        if(actually_computed(i)) done = done + 1
      end do

      E0 = sum(pt2_detail(1,:first_det_of_teeth(tooth)-1))
      prop = ((1d0 - dfloat(comb_teeth - tooth + 1) * comb_step) - cweight(first_det_of_teeth(tooth)-1))
      prop = prop / weight(first_det_of_teeth(tooth))
      E0 += pt2_detail(1,first_det_of_teeth(tooth)) * prop
      avg = E0 + (sumabove(tooth) / Nabove(tooth))
      eqt = sqrt(1d0 / (Nabove(tooth)-1) * abs(sum2above(tooth) / Nabove(tooth) - (sumabove(tooth)/Nabove(tooth))**2))
      time = omp_get_wtime()
      print "(A, 4(E15.7), 4(I9))", "PT2stoch ", time - time0, avg, eqt, Nabove(tooth), tooth, first_det_of_teeth(tooth)-1, done, first_det_of_teeth(tooth+1)-first_det_of_teeth(tooth)
      if (dabs(eqt/avg) < relative_error) then
        relative_error = 0.d0
        pt2(1) = avg
        exit
      endif
    end if
  end do pullLoop


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
  comb_teeth = 100
END_PROVIDER



subroutine get_first_tooth(computed, first_teeth)
  implicit none
  logical, intent(in) :: computed(N_det_generators)
  integer, intent(out) :: first_teeth
  integer :: i, first_det

  first_det = 1
  first_teeth = 1
  do i=first_det_of_comb, N_det_generators
    if(not(computed(i))) then
      first_det = i
      exit
    end if
  end do
  
  do i=comb_teeth, 1, -1
    if(first_det_of_teeth(i) < first_det) then
      first_teeth = i
      exit
    end if
  end do

end subroutine


subroutine get_last_full_tooth(computed, last_tooth)
  implicit none
  logical, intent(in) :: computed(N_det_generators)
  integer, intent(out) :: last_tooth
  integer :: i, j, missing
  
  last_tooth = 0
   combLoop : do i=comb_teeth-1, 1, -1
     missing = 1
     do j=first_det_of_teeth(i), first_det_of_teeth(i+1)-1
       if(not(computed(j))) then
         missing -= 1
         if(missing < 0) cycle combLoop
       end if
     end do
     last_tooth = i
     exit
   end do combLoop
end subroutine



subroutine get_carlo_workbatch(maxWorkload, computed, comb, Ncomb, tbc)
  implicit none
  double precision, intent(in) :: maxWorkload
  double precision, intent(out) :: comb(N_det_generators)
  integer, intent(inout) :: tbc(0:N_det_generators)
  integer, intent(out) :: Ncomb
  logical, intent(inout) :: computed(N_det_generators)
  integer :: i, j, last_full, dets(comb_teeth)
  double precision :: myWorkload
 
  myWorkload = 0d0
  
  do i=1,size(comb)
    call RANDOM_NUMBER(comb(i))
    comb(i) = comb(i) * comb_step
    call add_comb(comb(i), computed, tbc, myWorkload)
    Ncomb = i
    
    call get_last_full_tooth(computed, last_full)
    if(Ncomb >= 30 .and. last_full /= 0) then
      do j=1,first_det_of_teeth(last_full+1)-1
        if(not(computed(j))) then
          tbc(0) += 1
          tbc(tbc(0)) = j
          computed(j) = .true.
          myWorkload += comb_workload(j)
          print *, "filled ", j, "to reach tooth", last_full, "ending at", first_det_of_teeth(last_full+1)
        end if
      end do
    end if

    if(myWorkload > maxWorkload .and. i >= 100) exit
  end do
end subroutine


subroutine reorder_tbc(tbc)
  implicit none
  integer, intent(inout) :: tbc(0:N_det_generators)
  logical, allocatable :: ltbc(:)
  integer :: i, ci

  allocate(ltbc(N_det_generators))
  ltbc = .false.
  do i=1,tbc(0)
    ltbc(tbc(i)) = .true.
  end do

  ci = 0
  do i=1,N_det_generators
    if(ltbc(i)) then
      ci += 1
      tbc(ci) = i
    end if
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
    if(weight(i)/norm_left < comb_step/2d0) then
      first_det_of_comb = i
      exit
    end if
    norm_left -= weight(i)
  end do
  
  comb_step = 1d0 / dfloat(comb_teeth) * (1d0 - cweight(first_det_of_comb-1))
  
  stato = 1d0 - comb_step! + 1d-5
  do i=comb_teeth, 1, -1
    first_det_of_teeth(i) = pt2_find(stato, cweight)
    stato -= comb_step
  end do
  first_det_of_teeth(comb_teeth+1) = N_det_generators + 1
  first_det_of_teeth(1) = first_det_of_comb
  if(first_det_of_teeth(1) /= first_det_of_comb) stop "comb provider"
  
END_PROVIDER










