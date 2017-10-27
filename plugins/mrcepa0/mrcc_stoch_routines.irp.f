BEGIN_PROVIDER [ integer, fragment_first ]
  implicit none
  fragment_first = first_det_of_teeth(1)
END_PROVIDER

subroutine ZMQ_mrcc(E, mrcc, delta, delta_s2, relative_error)
  use dress_types
  use f77_zmq
  
  implicit none
  
  character(len=64000)           :: task
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket, zmq_to_qp_run_socket2
  integer, external              :: omp_get_thread_num
  double precision, intent(in)   :: relative_error, E
  double precision, intent(out)  :: mrcc(N_states)
  double precision, intent(out)  :: delta(N_states, N_det_non_ref)
  double precision, intent(out)  :: delta_s2(N_states, N_det_non_ref)
  type(dress_buffer)             :: db(2)
  
  
  double precision, allocatable  :: mrcc_detail(:,:), comb(:)
  logical, allocatable           :: computed(:)
  integer, allocatable           :: tbc(:)
  integer                        :: i, j, k, Ncomb, generator_per_task, i_generator_end
  integer, external              :: mrcc_find
  
  double precision               :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  double precision, external     :: omp_get_wtime
  double precision               :: time
  
  !if (N_det < 10) then
  !  !call ZMQ_selection(0, mrcc)
  !  return
  !else


    call init_dress_buffer(db(1))
    call init_dress_buffer(db(2))

    allocate(mrcc_detail(N_states, N_det_generators+1), comb(N_det_generators), computed(N_det_generators), tbc(0:size_tbc))
    sumabove = 0d0
    sum2above = 0d0
    Nabove = 0d0
    
    provide nproc fragment_first fragment_count mo_bielec_integrals_in_map mo_mono_elec_integral mrcc_weight psi_selectors
    
    computed = .false.
    
    tbc(0) = first_det_of_comb - 1
    do i=1, tbc(0)
      tbc(i) = i
      computed(i) = .true.
    end do
    
    mrcc_detail = 0d0
    generator_per_task = 1
    print *, '========== ================= ================= ================='
    print *, ' Samples        Energy         Stat. Error         Seconds      '
    print *, '========== ================= ================= ================='
    
    call new_parallel_job(zmq_to_qp_run_socket,'mrcc')
    call zmq_put_psi(zmq_to_qp_run_socket,1,mrcc_e0_denominator,size(mrcc_e0_denominator))
    
    Ncomb=size(comb)
    call get_carlo_workbatch(computed, comb, Ncomb, tbc)
    
    do i=1,comb_teeth
      print *, "TOOTH", first_det_of_teeth(i+1) - first_det_of_teeth(i)
    end do
    !stop

    integer(ZMQ_PTR), external     :: new_zmq_to_qp_run_socket
    integer                        :: ipos
    ipos=1
    do i=1,tbc(0)
      if(tbc(i) > fragment_first) then
        write(task(ipos:ipos+20),'(I9,1X,I9,''|'')') 0, tbc(i)
        ipos += 20
        if (ipos > 63980) then
          call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos)))
          ipos=1
        endif
      else
        do j=1,fragment_count
          write(task(ipos:ipos+20),'(I9,1X,I9,''|'')') j, tbc(i)
          ipos += 20
          if (ipos > 63980) then
            call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos)))
            ipos=1
          endif
        end do
      end if
    end do
    if (ipos > 1) then
      call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos)))
    endif
    
    call zmq_set_running(zmq_to_qp_run_socket)
    
    !$OMP PARALLEL DEFAULT(shared) NUM_THREADS(nproc+1)              &
        !$OMP  PRIVATE(i)
    i = omp_get_thread_num()
    if (i==0) then
      call mrcc_collector(E, db, tbc, comb, Ncomb, computed, mrcc_detail, sumabove, sum2above, Nabove, relative_error, mrcc)
    else
      call mrcc_slave_inproc(i)
    endif
    !$OMP END PARALLEL
    call end_parallel_job(zmq_to_qp_run_socket, 'mrcc')
    
    print *, '========== ================= ================= ================='
    
  !endif
  call flush_dress_buffer(db(1), delta)
  call flush_dress_buffer(db(2), delta_s2)
  deallocate(db(1)%buf)
  deallocate(db(2)%buf)

end subroutine


subroutine do_carlo(tbc, Ncomb, comb, mrcc_detail, db, computed, sumabove, sum2above, sumin, isincomb, Nabove)
  use dress_types
  implicit none

  integer, intent(in) :: tbc(0:size_tbc), Ncomb
  logical, intent(in) :: computed(N_det_generators)
  double precision, intent(in) :: comb(Ncomb), mrcc_detail(N_states, N_det_generators)
  double precision, intent(inout) :: sumabove(comb_teeth), sum2above(comb_teeth), sumin(comb_teeth), Nabove(comb_teeth)
  double precision, intent(inout) :: isincomb(N_det_generators)
  type(dress_buffer), intent(inout) :: db(2)
  integer :: i, j, dets(comb_teeth)
  double precision :: myVal, myVal2, myValcur

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
      isincomb(dets(j)) = 1d0
      myValcur = mrcc_detail(1, dets(j)) * mrcc_weight_inv(dets(j)) * comb_step
      sumin(j) += myValcur**2
      myVal += myValcur
      sumabove(j) += myVal
      call dress_buffer_drew(db(1), dets(j), mrcc_weight_inv(dets(j)) * comb_step)
      call dress_buffer_drew(db(2), dets(j), mrcc_weight_inv(dets(j)) * comb_step)
      sum2above(j) += myVal*myVal
      Nabove(j) += 1
    end do
    db(1)%N += 1d0
    db(2)%N += 1d0
  end do mainLoop
end subroutine


subroutine mrcc_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call run_mrcc_slave(1,i,mrcc_e0_denominator)
end


subroutine mrcc_collector(E, db, tbc, comb, Ncomb, computed, mrcc_detail, sumabove, sum2above, Nabove, relative_error, mrcc)
  use dress_types
  use f77_zmq
  use bitmasks
  implicit none

  
  integer, intent(in) :: Ncomb
  double precision, intent(inout) :: mrcc_detail(N_states, N_det_generators)
  type(dress_buffer), intent(inout) :: db(2)
  double precision, intent(in) :: comb(Ncomb), relative_error, E
  logical, intent(inout) :: computed(N_det_generators)
  integer, intent(in) :: tbc(0:size_tbc)
  double precision, intent(inout) :: sumabove(comb_teeth), sum2above(comb_teeth), Nabove(comb_teeth)
  double precision, intent(out)  :: mrcc(N_states)
  

  double precision, allocatable      :: mrcc_mwen(:,:)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull

  integer :: msg_size, rc, more
  integer :: acc, i, j, robin, N, ntask
  integer(bit_kind), allocatable :: det(:,:,:)
  integer, allocatable :: task_id(:)
  integer :: Nindex
  integer, allocatable :: ind(:)
  double precision, save :: time0 = -1.d0
  double precision :: time, timeLast, Nabove_old
  double precision, external :: omp_get_wtime
  integer :: tooth, firstTBDcomb, orgTBDcomb
  integer, allocatable :: parts_to_get(:)
  logical, allocatable :: actually_computed(:)
  double precision :: ncomputed
  integer :: total_computed
  
  double precision :: sumin(comb_teeth)
  double precision :: isincomb(N_det_generators)

  print *, "collecture"

  ncomputed = 0d0
  total_computed = 0
  sumin = 0d0
  isincomb = 0d0

  character*(512) :: task
  Nabove_old = -1.d0
  
  allocate(actually_computed(N_det_generators), parts_to_get(N_det_generators), &
    mrcc_mwen(N_states, N_det_generators) )
  mrcc_mwen(1:N_states, 1:N_det_generators) =0.d0

  
  do i=1,N_det_generators
    actually_computed(i) = computed(i)
  enddo
  
  parts_to_get(:) = 1
  if(fragment_first > 0) then
    do i=1,fragment_first
      parts_to_get(i) = fragment_count
    enddo
  endif

  do i=1,tbc(0)
    actually_computed(tbc(i)) = .false.
  end do
  
  orgTBDcomb = int(Nabove(1))
  firstTBDcomb = 1

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  allocate(task_id(N_det_generators), ind(db(1)%N_slot))
  more = 1
  if (time0 < 0.d0) then
      call wall_time(time0)
  endif
  timeLast = time0

  call get_first_tooth(actually_computed, tooth)
  Nabove_old = Nabove(tooth)
  db(1)%free_under = first_det_of_teeth(1)-1 
  db(2)%free_under = first_det_of_teeth(1)-1 
  
  pullLoop : do while (more == 1)

    call pull_mrcc_results(zmq_socket_pull, Nindex, ind, mrcc_mwen, db, task_id, ntask)
    do i=1,Nindex
      mrcc_detail(1:N_states, ind(i)) += mrcc_mwen(1:N_states,i)

      parts_to_get(ind(i)) -= 1
      if(parts_to_get(ind(i)) < 0) then 
        print *, i, ind(i), parts_to_get(ind(i)), Nindex
        print *, "PARTS ??"
        print *, parts_to_get
        stop "PARTS ??"
      end if
      if(parts_to_get(ind(i)) == 0) then
        !print *, "COMPUTED", ind(i)
        actually_computed(ind(i)) = .true.
        total_computed += 1
      end if
    end do

    do i=1, ntask
      if(task_id(i) == 0) then
          print *,  "Error in collector"
      endif
      call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id(i),more)
    end do
    
    call get_first_tooth(actually_computed, tooth)
    db(1)%free_under = first_det_of_teeth(tooth)-1
    db(2)%free_under = first_det_of_teeth(tooth)-1
     
    time = omp_get_wtime()

    if(time - timeLast > 1d0 .or. more /= 1) then
      timeLast = time
      do i=1, first_det_of_teeth(1)-1
        if(.not.(actually_computed(i))) then
          cycle pullLoop
        end if
      end do
      
      double precision :: E0, fco, ffco, avg, tavg, mavg, var, su, su2, meqt, eqt, prop
      if(firstTBDcomb <= size(comb)) then
        call do_carlo(tbc, Ncomb+1-firstTBDcomb, comb(firstTBDcomb), mrcc_detail, db, actually_computed, sumabove, sum2above, sumin, isincomb, Nabove)
        firstTBDcomb = int(Nabove(1)) - orgTBDcomb + 1
      end if
      if(Nabove(1) < 5d0) cycle
            E0 = sum(mrcc_detail(1,:first_det_of_teeth(tooth)-1))
      if (tooth <= comb_teeth) then
        prop = ((1d0 - dfloat(comb_teeth - tooth + 1) * comb_step) - mrcc_cweight(first_det_of_teeth(tooth)-1))
        prop = prop * mrcc_weight_inv(first_det_of_teeth(tooth))
        E0 += mrcc_detail(1,first_det_of_teeth(tooth)) * prop
        avg = E0 + (sumabove(tooth) / Nabove(tooth))
        eqt = sqrt(1d0 / (Nabove(tooth)-1) * abs(sum2above(tooth) / Nabove(tooth) - (sumabove(tooth)/Nabove(tooth))**2))
      else
        eqt = 0.d0
      endif
      call wall_time(time)
      !if (dabs(eqt/avg) < relative_error) then
      if (dabs(eqt) < relative_error .or. total_computed == N_det_generators) then
        ! Termination
        print *, "converged", Nabove(1), first_det_of_teeth(tooth)
        !mrcc(1) = avg+E
        print '(A7, G10.3, 2X, F16.7, 2X, G16.3, 2X, F16.4, A20)', "GREPME", Nabove(tooth), E+avg, eqt, time-time0, ''
        call zmq_abort(zmq_to_qp_run_socket)
      else
        if (Nabove(tooth) > Nabove_old) then
          meqt = 0d0
          ncomputed = 0
          mavg = 0d0
          do i=tooth, comb_teeth
            !do j=first_det_of_teeth(i), first_det_of_teeth(i+1)-1
            !  if(actually_computed(j)) ncomputed(i) = ncomputed(i) + 1
            !end do
            fco = 0d0
            ffco = 0d0
            do j=first_det_of_teeth(i), first_det_of_teeth(i+1)-1
              fco += isincomb(j) * mrcc_weight(j)
              ffco += mrcc_weight(j)
            end do
            ncomputed = sum(isincomb(first_det_of_teeth(i):first_det_of_teeth(i+1)-1))

            !if(i /= comb_teeth) then
            !  var = abs((sumin(i))/Nabove(i) - ((sumabove(i)-sumabove(i+1))/Nabove(i))**2) / (Nabove(i)-1)
            !else
            !  var = abs((sumin(i))/Nabove(i) - ((sumabove(i))/Nabove(i))**2) / (Nabove(i)-1)
            !end if
            n = first_det_of_teeth(i+1) - first_det_of_teeth(i)


            !var = var * ((ffco-fco) /ffco) !((dfloat(n)-ncomputed) / dfloat(n))**2
            !eqt += var / (Nabove(i)-1)

            tavg = 0d0
            ncomputed = 0d0
            su = 0d0
            su2 = 0d0
            do j=first_det_of_teeth(i), first_det_of_teeth(i+1)-1
              if(actually_computed(j)) then
                tavg += (mrcc_detail(1, j))! * isincomb(j)
                ncomputed += 1d0 !isincomb(j)
                su2 += (mrcc_detail(1, j))**2
              end if
            end do
            if(ncomputed /= 0) then
              tavg = tavg / ncomputed * dfloat(n)
              su2 = su2 / ncomputed * dfloat(n)
              var = (su2 - (tavg**2)) / ncomputed
            else
              print *, "ZERO NCOMPUTED"
              tavg = 0d0
              su2 = 0d0
              var = 0d0
            end if
            
            !fco = ffco
            !if(i /= comb_teeth) then
            !  mavg += (tavg) + (((sumabove(i)-sumabove(i+1))/Nabove(i)) * (ffco-fco))  &
            !        / ffco
            !else
            !  mavg += (tavg) + (((sumabove(i))/Nabove(i)) * (ffco-fco))  &
            !        / ffco
            !end if


            var = var * ((dfloat(n)-ncomputed) / dfloat(n))
            meqt += var
            mavg += tavg
          end do
          meqt = sqrt(abs(meqt))
          Nabove_old = Nabove(tooth)
          
          print '(G10.3, 2X, F16.7, 2X, G16.3, 2X, F16.4, A20)', Nabove(tooth), avg+E, eqt, time-time0, ''
          !print *, "GREPME", avg, eqt, mavg+E0, meqt
        endif
      endif
    end if
  end do pullLoop

  E0 = sum(mrcc_detail(1,:first_det_of_teeth(tooth)-1))
  prop = ((1d0 - dfloat(comb_teeth - tooth + 1) * comb_step) - mrcc_cweight(first_det_of_teeth(tooth)-1))
  prop = prop * mrcc_weight_inv(first_det_of_teeth(tooth))
  E0 += mrcc_detail(1,first_det_of_teeth(tooth)) * prop
  mrcc(1) = E + E0 + (sumabove(tooth) / Nabove(tooth))
  
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_pull_socket(zmq_socket_pull)
end subroutine

integer function mrcc_find(v, w, sze, imin, imax)
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
  do mrcc_find=l,h
    if(w(mrcc_find) >= v) then
      exit
    end if
  end do
end function


BEGIN_PROVIDER [ integer, comb_teeth ]
  implicit none
  comb_teeth = 10
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


BEGIN_PROVIDER [ integer, size_tbc ]
  implicit none
  BEGIN_DOC
! Size of the tbc array
  END_DOC
  size_tbc = (comb_teeth+1)*N_det_generators + fragment_count*fragment_first
END_PROVIDER

subroutine get_carlo_workbatch(computed, comb, Ncomb, tbc)
  implicit none
  integer, intent(inout)         :: Ncomb
  double precision, intent(out)  :: comb(Ncomb)
  integer, intent(inout)         :: tbc(0:size_tbc)
  logical, intent(inout)         :: computed(N_det_generators)
  integer                        :: i, j, last_full, dets(comb_teeth)
  integer                        :: icount, n
  integer                        :: k, l
  l=first_det_of_comb
  call RANDOM_NUMBER(comb)
  do i=1,size(comb)
    comb(i) = comb(i) * comb_step
    !DIR$ FORCEINLINE
    call add_comb(comb(i), computed, tbc, size_tbc, comb_teeth)
    Ncomb = i
    if (tbc(0) == N_det_generators) return
    do while (computed(l))
      l=l+1
    enddo
    k=tbc(0)+1
    tbc(k) = l
    computed(l) = .True.
    tbc(0) = k
  enddo
  
end subroutine



subroutine get_comb(stato, dets, ct)
  implicit none
  integer, intent(in) :: ct
  double precision, intent(in) :: stato
  integer, intent(out) :: dets(ct)
  double precision :: curs
  integer :: j
  integer, external :: mrcc_find

  curs = 1d0 - stato
  do j = comb_teeth, 1, -1
    !DIR$ FORCEINLINE
    dets(j) = mrcc_find(curs, mrcc_cweight,size(mrcc_cweight), first_det_of_teeth(j), first_det_of_teeth(j+1))
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



 BEGIN_PROVIDER [ double precision, mrcc_weight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, mrcc_cweight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, mrcc_cweight_cache, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, comb_step ]
&BEGIN_PROVIDER [ integer, first_det_of_teeth, (comb_teeth+1) ]
&BEGIN_PROVIDER [ integer, first_det_of_comb ]
  implicit none
  integer :: i
  double precision :: norm_left, stato
  integer, external :: mrcc_find  

  mrcc_weight(1) = psi_coef_generators(1,1)**2
  mrcc_cweight(1) = psi_coef_generators(1,1)**2
  
  do i=1,N_det_generators
    mrcc_weight(i) = psi_coef_generators(i,1)**2
  enddo

  ! Important to loop backwards for numerical precision
  mrcc_cweight(N_det_generators) = mrcc_weight(N_det_generators)
  do i=N_det_generators-1,1,-1
    mrcc_cweight(i) = mrcc_weight(i) + mrcc_cweight(i+1) 
  end do
  
  do i=1,N_det_generators
    mrcc_weight(i)  = mrcc_weight(i) / mrcc_cweight(1)
    mrcc_cweight(i) = mrcc_cweight(i) / mrcc_cweight(1)
  enddo

  do i=1,N_det_generators-1
    mrcc_cweight(i) = 1.d0 - mrcc_cweight(i+1) 
  end do
  mrcc_cweight(N_det_generators) = 1.d0
  
  norm_left = 1d0
  
  comb_step = 1d0/dfloat(comb_teeth)
  first_det_of_comb = 1
  do i=1,N_det_generators
    if(mrcc_weight(i)/norm_left < .25d0*comb_step) then
      first_det_of_comb = i
      exit
    end if
    norm_left -= mrcc_weight(i)
  end do
  first_det_of_comb = max(2,first_det_of_comb)
  call write_int(6, first_det_of_comb-1, 'Size of deterministic set')
  

  comb_step =  (1d0 - mrcc_cweight(first_det_of_comb-1)) * comb_step
  
  stato = 1d0 - comb_step
  iloc = N_det_generators
  do i=comb_teeth, 1, -1
    integer :: iloc
    iloc = mrcc_find(stato, mrcc_cweight, N_det_generators, 1, iloc)
    first_det_of_teeth(i) = iloc
    stato -= comb_step
  end do
  first_det_of_teeth(comb_teeth+1) = N_det_generators + 1
  first_det_of_teeth(1) = first_det_of_comb
  
  !do i=1,comb_teeth
  !  mrcc_weight(first_det_of_teeth(i):first_det_of_teeth(i+1)-1) = &
  !      (mrcc_cweight(first_det_of_teeth(i+1)-1)-mrcc_cweight(first_det_of_teeth(i)-1)) / &
  !      dfloat(first_det_of_teeth(i+1)-first_det_of_teeth(i))
  !end do

  !mrcc_cweight = 0d0 
  !mrcc_cweight(N_det_generators) = mrcc_weight(N_det_generators)
  !do i=N_det_generators-1,1,-1
  !  mrcc_cweight(i) = mrcc_weight(i) + mrcc_cweight(i+1) 
  !end do
  
  !do i=1,N_det_generators
  !  mrcc_weight(i)  = mrcc_weight(i) / mrcc_cweight(1)
  !  mrcc_cweight(i) = mrcc_cweight(i) / mrcc_cweight(1)
 ! enddo

  !do i=1,N_det_generators-1
  !  mrcc_cweight(i) = 1.d0 - mrcc_cweight(i+1) 
  !end do
  !mrcc_cweight(N_det_generators) = 1.d0
  

  if(first_det_of_teeth(1) /= first_det_of_comb) then
     print *, 'Error in ', irp_here
     stop "comb provider"
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, mrcc_weight_inv, (N_det_generators) ]
  implicit none
  BEGIN_DOC
!  Inverse of mrcc_weight array
  END_DOC
  integer :: i
  do i=1,N_det_generators
    mrcc_weight_inv(i) = 1.d0/mrcc_weight(i)
  enddo

END_PROVIDER






