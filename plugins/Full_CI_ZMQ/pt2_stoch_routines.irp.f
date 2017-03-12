BEGIN_PROVIDER [ integer, fragment_first ]
  implicit none
  fragment_first = first_det_of_teeth(1)
END_PROVIDER

subroutine ZMQ_pt2(pt2,relative_error)
  use f77_zmq
  use selection_types
  
  implicit none
  
  character(len=64000)           :: task 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket, zmq_to_qp_run_socket2
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

  allocate(pt2_detail(N_states, N_det_generators), comb(N_det_generators), computed(N_det_generators), tbc(0:size_tbc))
  sumabove = 0d0
  sum2above = 0d0
  Nabove = 0d0

  provide nproc fragment_first fragment_count mo_bielec_integrals_in_map mo_mono_elec_integral

  !call random_seed()
  
  computed = .false.

  tbc(0) = first_det_of_comb - 1
  do i=1, tbc(0)
    tbc(i) = i
    computed(i) = .true.
  end do
  
  pt2_detail = 0d0
  time0 = omp_get_wtime()
  print *, "time - avg - err - n_combs"
  generator_per_task = 1
  do while(.true.)
    
    call write_time(6)
    call new_parallel_job(zmq_to_qp_run_socket,"pt2")
    call zmq_put_psi(zmq_to_qp_run_socket,1,pt2_e0_denominator,size(pt2_e0_denominator))
    call create_selection_buffer(1, 1*2, b)
    
    Ncomb=size(comb)
    call get_carlo_workbatch(computed, comb, Ncomb, tbc)

    call write_time(6)


    integer(ZMQ_PTR), external :: new_zmq_to_qp_run_socket
    integer :: ipos
    logical :: tasks
    tasks = .False.
    ipos=1

    do i=1,tbc(0)
      if(tbc(i) > fragment_first) then
        write(task(ipos:ipos+20),'(I9,X,I9,''|'')') 0, tbc(i)
        ipos += 20
        if (ipos > 64000) then
          call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos-20)))
          ipos=1
          tasks = .True.
        endif
      else
        do j=1,fragment_count
          write(task(ipos:ipos+20),'(I9,X,I9,''|'')') j, tbc(i)
          ipos += 20
          if (ipos > 64000) then
            call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos-20)))
            ipos=1
            tasks = .True.
          endif
        end do
      end if
    end do
    if (ipos > 1) then
      call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos-20)))
      tasks = .True.
    endif

    if (tasks) then
      call zmq_set_running(zmq_to_qp_run_socket)

      !$OMP PARALLEL DEFAULT(shared) NUM_THREADS(nproc+1) &
      !$OMP  PRIVATE(i)
        i = omp_get_thread_num()
        if (i==0) then
          call pt2_collector(b, tbc, comb, Ncomb, computed, pt2_detail, sumabove, sum2above, Nabove, relative_error, pt2)
        else
          call pt2_slave_inproc(i)
        endif
      !$OMP END PARALLEL

    else
       pt2(1) = sum(pt2_detail(1,:))
    endif

    call end_parallel_job(zmq_to_qp_run_socket, 'pt2')
    tbc(0) = 0
    if (pt2(1) /= 0.d0) then
      exit
    endif
  end do


end subroutine


subroutine do_carlo(tbc, Ncomb, comb, pt2_detail, computed, sumabove, sum2above, Nabove)
  integer, intent(in) :: tbc(0:size_tbc), Ncomb
  logical, intent(in) :: computed(N_det_generators)
  double precision, intent(in) :: comb(Ncomb), pt2_detail(N_states, N_det_generators)
  double precision, intent(inout) :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  integer :: i, dets(comb_teeth)
  double precision :: myVal, myVal2

  mainLoop : do i=1,Ncomb
    call get_comb(comb(i), dets, comb_teeth)
    do j=1,comb_teeth
      if(.not.(computed(dets(j)))) then
        exit mainLoop
      end if
    end do
    
    myVal = 0d0
    myVal2 = 0d0
    do j=comb_teeth,1,-1
      myVal += pt2_detail(1, dets(j)) * pt2_weight_inv(dets(j)) * comb_step
      sumabove(j) += myVal
      sum2above(j) += myVal*myVal
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
  double precision, intent(in) :: comb(Ncomb), relative_error
  logical, intent(inout) :: computed(N_det_generators)
  integer, intent(in) :: tbc(0:size_tbc)
  double precision, intent(inout) :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  double precision, intent(out)  :: pt2(N_states)


  type(selection_buffer), intent(inout) :: b
  double precision, allocatable      :: pt2_mwen(:,:)
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
  
  allocate(actually_computed(N_det_generators), parts_to_get(N_det_generators), &
    pt2_mwen(N_states, N_det_generators) )
  actually_computed(:) = computed(:)
  
  parts_to_get(:) = 1
  if(fragment_first > 0) then
    parts_to_get(1:fragment_first) = fragment_count
  endif

  do i=1,tbc(0)
    actually_computed(tbc(i)) = .false.
  end do
  
  orgTBDcomb = Nabove(1)
  firstTBDcomb = 1

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  allocate(val(b%N), det(N_int, 2, b%N), task_id(N_det_generators), index(1))
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
        print *, i, index(i), parts_to_get(index(i)), Nindex
        print *, "PARTS ??"
        print *, parts_to_get
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
  
    if(time - timeLast > 1d1 .or. more /= 1) then
      timeLast = time
      do i=1, first_det_of_teeth(1)-1
        if(.not.(actually_computed(i))) then
          print *, "PT2 : deterministic part not finished"
          cycle pullLoop
        end if
      end do
      
      double precision :: E0, avg, eqt, prop
      call do_carlo(tbc, Ncomb+1-firstTBDcomb, comb(firstTBDcomb), pt2_detail, actually_computed, sumabove, sum2above, Nabove)
      firstTBDcomb = Nabove(1) - orgTBDcomb + 1
      if(Nabove(1) < 2d0) cycle
      call get_first_tooth(actually_computed, tooth)
     
      done = 0
      do i=first_det_of_teeth(tooth), first_det_of_teeth(tooth+1)-1
        if(actually_computed(i)) done = done + 1
      end do

      E0 = sum(pt2_detail(1,:first_det_of_teeth(tooth)-1))
      prop = ((1d0 - dfloat(comb_teeth - tooth + 1) * comb_step) - pt2_cweight(first_det_of_teeth(tooth)-1))
      prop = prop * pt2_weight_inv(first_det_of_teeth(tooth))
      E0 += pt2_detail(1,first_det_of_teeth(tooth)) * prop
      avg = E0 + (sumabove(tooth) / Nabove(tooth))
      eqt = sqrt(1d0 / (Nabove(tooth)-1) * abs(sum2above(tooth) / Nabove(tooth) - (sumabove(tooth)/Nabove(tooth))**2))
      time = omp_get_wtime()
      print "(3(E22.13), 4(I9))", "PT2stoch ", time - time0, avg, eqt, Nabove(tooth), tooth, first_det_of_teeth(tooth)-1, done, first_det_of_teeth(tooth+1)-first_det_of_teeth(tooth)
      if (dabs(eqt/avg) < relative_error) then
        pt2(1) = avg
        exit pullLoop
      endif
    end if
  end do pullLoop
  print "(3(E22.13), 4(I9))", "PT2stoch ", time - time0, avg, eqt, Nabove(tooth), tooth, first_det_of_teeth(tooth)-1, done, first_det_of_teeth(tooth+1)-first_det_of_teeth(tooth)


  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_pull_socket(zmq_socket_pull)
  call sort_selection_buffer(b)
end subroutine

integer function pt2_find(v, w, sze, imin, imax)
  implicit none
  integer, intent(in) :: sze, imin, imax
  double precision, intent(in) :: v, w(sze)
  integer :: i,l,h
  integer, parameter :: block=64

  l = imin
  h = imax-1

  do while(h-l >= block)
    i = ishft(h+l,-1)
    if(w(i+1) > v) then
      h = i-1
    else
      l = i+1
    end if
  end do
  !DIR$ LOOP COUNT (64)
  do pt2_find=l,h
    if(w(pt2_find) >= v) then
      exit
    end if
  end do
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
    if(.not.(computed(i))) then
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
   combLoop : do i=comb_teeth, 1, -1
     missing = 1+ ishft(first_det_of_teeth(i+1)-first_det_of_teeth(i),-5) ! /32
     do j=first_det_of_teeth(i), first_det_of_teeth(i+1)-1
       if(.not.computed(j)) then
         missing -= 1
         if(missing < 0) cycle combLoop
       end if
     end do
     last_tooth = i
     exit
   end do combLoop
end subroutine


BEGIN_PROVIDER [ integer, size_tbc ]
  implicit none
  BEGIN_DOC
! Size of the tbc array
  END_DOC
  size_tbc = N_det_generators + fragment_count*fragment_first
END_PROVIDER

subroutine get_carlo_workbatch(computed, comb, Ncomb, tbc)
  implicit none
  double precision, intent(out) :: comb(Ncomb)
  integer, intent(inout) :: tbc(0:size_tbc)
  integer, intent(inout) :: Ncomb
  logical, intent(inout) :: computed(N_det_generators)
  integer :: i, j, last_full, dets(comb_teeth), tbc_save
  integer :: icount, n
  n = tbc(0)
  icount = 0 
  call RANDOM_NUMBER(comb)
  do i=1,size(comb)
      comb(i) = comb(i) * comb_step
      tbc_save = tbc(0)
      !DIR$ FORCEINLINE
      call add_comb(comb(i), computed, tbc, size_tbc, comb_teeth)
      if (tbc(0) < size(tbc)) then
         Ncomb = i
      else
         tbc(0) = tbc_save
         return
      endif
      icount = icount + tbc(0) - tbc_save
      if (icount > n) then
        call get_filling_teeth(computed, tbc)
        icount = 0
        n = ishft(tbc_save,-4)
      endif
  enddo

end subroutine


subroutine get_filling_teeth(computed, tbc)
  implicit none
  integer, intent(inout) :: tbc(0:size_tbc)
  logical, intent(inout) :: computed(N_det_generators)
  integer :: i, j, k, last_full, dets(comb_teeth)
 
  call get_last_full_tooth(computed, last_full)
  if(last_full /= 0) then
    if (tbc(0) > size(tbc) - first_det_of_teeth(last_full+1) -2) then
      return
    endif
    k = tbc(0)+1
    do j=1,first_det_of_teeth(last_full+1)-1
      if(.not.(computed(j))) then
        tbc(k) = j
        k=k+1
        computed(j) = .true.
      end if
    end do
    tbc(0) = k-1
  end if

end subroutine


subroutine reorder_tbc(tbc)
  implicit none
  integer, intent(inout) :: tbc(0:size_tbc)
  logical, allocatable :: ltbc(:)
  integer :: i, ci

  allocate(ltbc(size_tbc))
  ltbc(:) = .false.
  do i=1,tbc(0)
    ltbc(tbc(i)) = .true.
  end do

  ci = 0
  do i=1,size_tbc
    if(ltbc(i)) then
      ci = ci+1
      tbc(ci) = i
    end if
  end do
end subroutine


subroutine get_comb(stato, dets, ct)
  implicit none
  integer, intent(in) :: ct
  double precision, intent(in) :: stato
  integer, intent(out) :: dets(ct)
  double precision :: curs
  integer :: j
  integer, external :: pt2_find

  curs = 1d0 - stato
  do j = comb_teeth, 1, -1
    !DIR$ FORCEINLINE
    dets(j) = pt2_find(curs, pt2_cweight,size(pt2_cweight), first_det_of_teeth(j), first_det_of_teeth(j+1))
    curs -= comb_step
  end do
end subroutine


subroutine add_comb(comb, computed, tbc, stbc, ct)
  implicit none
  integer, intent(in) :: stbc, ct
  double precision, intent(in) :: comb
  logical, intent(inout) :: computed(N_det_generators)
  integer, intent(inout) :: tbc(0:stbc)
  integer :: i, k, l, dets(ct)
  
  !DIR$ FORCEINLINE
  call get_comb(comb, dets, ct)
  
  k=tbc(0)+1
  do i = 1, ct
    l = dets(i)
    if(.not.(computed(l))) then
      tbc(k) = l
      k = k+1
      computed(l) = .true.
    end if
  end do
  tbc(0) = k-1
end subroutine



 BEGIN_PROVIDER [ double precision, pt2_weight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, pt2_cweight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, pt2_cweight_cache, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, comb_step ]
&BEGIN_PROVIDER [ integer, first_det_of_teeth, (comb_teeth+1) ]
&BEGIN_PROVIDER [ integer, first_det_of_comb ]
  implicit none
  integer :: i
  double precision :: norm_left, stato
  integer, external :: pt2_find  

  pt2_weight(1) = psi_coef_generators(1,1)**2
  pt2_cweight(1) = psi_coef_generators(1,1)**2
  
  do i=2,N_det_generators
    pt2_weight(i) = psi_coef_generators(i,1)**2
    pt2_cweight(i) = pt2_cweight(i-1) + psi_coef_generators(i,1)**2
  end do
  
  pt2_weight = pt2_weight / pt2_cweight(N_det_generators)
  pt2_cweight = pt2_cweight / pt2_cweight(N_det_generators)
  
  norm_left = 1d0
  
  comb_step = 1d0/dfloat(comb_teeth)
  do i=1,N_det_generators
    if(pt2_weight(i)/norm_left < comb_step*.5d0) then
      first_det_of_comb = i
      exit
    end if
    norm_left -= pt2_weight(i)
  end do
  
  comb_step =  (1d0 - pt2_cweight(first_det_of_comb-1)) * comb_step
  
  stato = 1d0 - comb_step
  iloc = N_det_generators
  do i=comb_teeth, 1, -1
    integer :: iloc
    iloc = pt2_find(stato, pt2_cweight, N_det_generators, 1, iloc)
    first_det_of_teeth(i) = iloc
    stato -= comb_step
  end do
  first_det_of_teeth(comb_teeth+1) = N_det_generators + 1
  first_det_of_teeth(1) = first_det_of_comb
  if(first_det_of_teeth(1) /= first_det_of_comb) then
     print *, 'Error in ', irp_here
     stop "comb provider"
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, pt2_weight_inv, (N_det_generators) ]
  implicit none
  BEGIN_DOC
!  Inverse of pt2_weight array
  END_DOC
  integer :: i
  do i=1,N_det_generators
    pt2_weight_inv(i) = 1.d0/pt2_weight(i)
  enddo

END_PROVIDER






