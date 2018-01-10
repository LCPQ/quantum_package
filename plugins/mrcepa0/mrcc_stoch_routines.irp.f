BEGIN_PROVIDER [ integer, fragment_first ]
  implicit none
  fragment_first = first_det_of_teeth(1)
END_PROVIDER


BEGIN_PROVIDER [ integer, mrcc_stoch_istate ]
 implicit none
 BEGIN_DOC
 ! State considered
 END_DOC
 mrcc_stoch_istate = 1
END_PROVIDER

subroutine ZMQ_mrcc(E, mrcc, delta, delta_s2, relative_error)
  use dress_types
  use f77_zmq
  
  implicit none
  
  character(len=64000)           :: task

  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket, zmq_socket_pull
  integer, external              :: omp_get_thread_num
  double precision, intent(in)   :: relative_error, E(N_states)
  double precision, intent(out)  :: mrcc(N_states)
  double precision, intent(out)  :: delta(N_states, N_det_non_ref)
  double precision, intent(out)  :: delta_s2(N_states, N_det_non_ref)
  
  
  integer                        :: i, j, k, Ncp
  
  double precision, external     :: omp_get_wtime
  double precision               :: time


  state_average_weight(:) = 0.d0
  state_average_weight(mrcc_stoch_istate) = 1.d0
  TOUCH state_average_weight 
  

  provide nproc fragment_first fragment_count mo_bielec_integrals_in_map mo_mono_elec_integral mrcc_weight psi_selectors

  
  
  print *, '========== ================= ================= ================='
  print *, ' Samples        Energy         Stat. Error         Seconds      '
  print *, '========== ================= ================= ================='
  

  call new_parallel_job(zmq_to_qp_run_socket,zmq_socket_pull, 'mrcc')
  
  integer, external              :: zmq_put_psi
  integer, external              :: zmq_put_N_det_generators
  integer, external              :: zmq_put_N_det_selectors
  integer, external              :: zmq_put_dvector
  
  if (zmq_put_psi(zmq_to_qp_run_socket,1) == -1) then
    stop 'Unable to put psi on ZMQ server'
  endif
  if (zmq_put_N_det_generators(zmq_to_qp_run_socket, 1) == -1) then
    stop 'Unable to put N_det_generators on ZMQ server'
  endif
  if (zmq_put_N_det_selectors(zmq_to_qp_run_socket, 1) == -1) then
    stop 'Unable to put N_det_selectors on ZMQ server'
  endif
  if (zmq_put_dvector(zmq_to_qp_run_socket,1,'energy',mrcc_e0_denominator,size(mrcc_e0_denominator)) == -1) then
    stop 'Unable to put energy on ZMQ server'
  endif

!  do i=1,comb_teeth
!    print *, "TOOTH", first_det_of_teeth(i+1) - first_det_of_teeth(i)
!  end do

  integer(ZMQ_PTR), external     :: new_zmq_to_qp_run_socket
  integer, external              :: add_task_to_taskserver, zmq_set_running
  integer                        :: ipos
  ipos=1
  do i=1,N_mrcc_jobs
    if(mrcc_jobs(i) > fragment_first) then
      write(task(ipos:ipos+20),'(I9,1X,I9,''|'')') 0, mrcc_jobs(i)
      ipos += 20
      if (ipos > 63980) then
        if (add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos))) == -1) then
          stop 'Unable to add task to task server'
        endif

        ipos=1
      endif
    else
      do j=1,fragment_count
        write(task(ipos:ipos+20),'(I9,1X,I9,''|'')') j, mrcc_jobs(i)
        ipos += 20
        if (ipos > 63980) then
          if (add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos))) == -1) then
            stop 'Unable to add task to task server'
          endif
          ipos=1
        endif
      end do
    end if
  end do
  if (ipos > 1) then
        if (add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos))) == -1) then
          stop 'Unable to add task to task server'
        endif
  endif
  
      if (zmq_set_running(zmq_to_qp_run_socket) == -1) then
        print *,  irp_here, ': Failed in zmq_set_running'
      endif

  !$OMP PARALLEL DEFAULT(shared) NUM_THREADS(nproc+1)                &
      !$OMP  PRIVATE(i)
  i = omp_get_thread_num()
  if (i==0) then
    call mrcc_collector(zmq_socket_pull,E(mrcc_stoch_istate), relative_error, delta, delta_s2, mrcc)

  else
    call mrcc_slave_inproc(i)
  endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, zmq_socket_pull, 'mrcc')
  
  print *, '========== ================= ================= ================='
end subroutine


subroutine mrcc_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call run_mrcc_slave(1,i,mrcc_e0_denominator)
end



subroutine mrcc_collector(zmq_socket_pull, E, relative_error, delta, delta_s2, mrcc)

  use dress_types
  use f77_zmq
  use bitmasks
  implicit none

  

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull

  double precision, intent(in) :: relative_error, E
  double precision, intent(out)  :: mrcc(N_states)
  double precision, allocatable  :: cp(:,:,:,:)

  double precision, intent(out)  :: delta(N_states, N_det_non_ref)
  double precision, intent(out)  :: delta_s2(N_states, N_det_non_ref)
  double precision, allocatable  :: delta_loc(:,:,:), delta_det(:,:,:,:)
  double precision, allocatable  :: mrcc_detail(:,:)
  double precision               :: mrcc_mwen(N_states)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_pull_socket

  integer :: more
  integer :: i, j, k, i_state, N, ntask
  integer, allocatable :: task_id(:)
  integer :: Nindex
  integer, allocatable :: ind(:)
  double precision, save :: time0 = -1.d0
  double precision :: time, timeLast, old_tooth
  double precision, external :: omp_get_wtime
  integer :: cur_cp, old_cur_cp
  integer, allocatable :: parts_to_get(:)
  logical, allocatable :: actually_computed(:)
  integer :: total_computed
  
  allocate(delta_det(N_states, N_det_non_ref, 0:comb_teeth+1, 2))
  allocate(cp(N_states, N_det_non_ref, N_cp, 2), mrcc_detail(N_states, N_det_generators))
  allocate(delta_loc(N_states, N_det_non_ref, 2))
  mrcc_detail = 0d0
  delta_det = 0d0
  !mrcc_detail = mrcc_detail / 0d0
  cp = 0d0
  total_computed = 0
  character*(512) :: task
  
  allocate(actually_computed(N_det_generators), parts_to_get(N_det_generators))
    
  mrcc_mwen =0.d0
  
  parts_to_get(:) = 1
  if(fragment_first > 0) then
    do i=1,fragment_first
      parts_to_get(i) = fragment_count
    enddo
  endif

  actually_computed = .false.

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  allocate(task_id(N_det_generators), ind(1))
  more = 1
  if (time0 < 0.d0) then
      call wall_time(time0)
  endif
  timeLast = time0
  cur_cp = 0
  old_cur_cp = 0
  pullLoop : do while (more == 1)
    call pull_mrcc_results(zmq_socket_pull, Nindex, ind, mrcc_mwen, delta_loc, task_id, ntask)

    if(Nindex /= 1) stop "tried pull multiple Nindex"

    do i=1,Nindex
      mrcc_detail(:, ind(i)) += mrcc_mwen(:)
      do j=1,N_cp !! optimizable
        if(cps(ind(i), j) > 0d0) then
          if(tooth_of_det(ind(i)) < cp_first_tooth(j)) stop "coef on supposedely deterministic det"
          double precision :: fac
          integer :: toothMwen
          logical :: fracted
          fac = cps(ind(i), j) / cps_N(j) * mrcc_weight_inv(ind(i)) * comb_step
          do k=1,N_det_non_ref
          do i_state=1,N_states
            cp(i_state,k,j,1) += delta_loc(i_state,k,1) * fac
            cp(i_state,k,j,2) += delta_loc(i_state,k,2) * fac
          end do
          end do
        end if
      end do
      toothMwen = tooth_of_det(ind(i))
      fracted = (toothMwen /= 0)
      if(fracted) fracted = (ind(i) == first_det_of_teeth(toothMwen))
     
      if(fracted) then
        delta_det(:,:,toothMwen-1, 1) += delta_loc(:,:,1) * (1d0-fractage(toothMwen))
        delta_det(:,:,toothMwen-1, 2) += delta_loc(:,:,2) * (1d0-fractage(toothMwen))
        delta_det(:,:,toothMwen, 1) += delta_loc(:,:,1) * (fractage(toothMwen))
        delta_det(:,:,toothMwen, 2) += delta_loc(:,:,2) * (fractage(toothMwen))
      else
        delta_det(:,:,toothMwen, 1) += delta_loc(:,:,1)
        delta_det(:,:,toothMwen, 2) += delta_loc(:,:,2)
      end if

      parts_to_get(ind(i)) -= 1
      if(parts_to_get(ind(i)) == 0) then
        actually_computed(ind(i)) = .true.
        !print *, "CONTRIB", ind(i), psi_non_ref_coef(ind(i),1), mrcc_detail(1, ind(i))
        total_computed += 1
      end if
    end do


    integer, external :: zmq_delete_tasks
    if (zmq_delete_tasks(zmq_to_qp_run_socket,zmq_socket_pull,task_id,ntask,more) == -1) then
        stop 'Unable to delete tasks'
    endif


    time = omp_get_wtime()
    
    

    if(time - timeLast > 1d0 .or. more /= 1) then
      timeLast = time
      cur_cp = N_cp
      if(.not. actually_computed(mrcc_jobs(1))) cycle pullLoop

      do i=2,N_det_generators
        if(.not. actually_computed(mrcc_jobs(i))) then
          print *, "first not comp", i
          cur_cp = done_cp_at(i-1)
          exit
        end if
      end do
      if(cur_cp == 0) cycle pullLoop
      
      !!!!!!!!!!!!
      double precision :: su, su2, eqt, avg, E0, val
      integer, external :: zmq_abort

      su = 0d0
      su2 = 0d0
      
      if(N_states > 1) stop "mrcc_stoch : N_states == 1"
      do i=1, int(cps_N(cur_cp))
        !if(.not. actually_computed(i)) stop "not computed"
        !call get_comb_val(comb(i), mrcc_detail, cp_first_tooth(cur_cp), val)
        call get_comb_val(comb(i), mrcc_detail, cur_cp, val)
        !val = mrcc_detail(1, i) * mrcc_weight_inv(i) * comb_step
        su += val ! cps(i, cur_cp) * val
        su2 += val**2 ! cps(i, cur_cp) * val**2
      end do
      avg = su / cps_N(cur_cp)
      eqt = dsqrt( ((su2 / cps_N(cur_cp)) - avg**2) / cps_N(cur_cp) )
      E0 = sum(mrcc_detail(1, :first_det_of_teeth(cp_first_tooth(cur_cp))-1))
      if(cp_first_tooth(cur_cp) <= comb_teeth) then
        E0 = E0 + mrcc_detail(1, first_det_of_teeth(cp_first_tooth(cur_cp))) * (1d0-fractage(cp_first_tooth(cur_cp)))
      end if
      call wall_time(time)
      if ((dabs(eqt) < relative_error .and. cps_N(cur_cp) >= 30)  .or. total_computed == N_det_generators) then
        ! Termination
        !print '(G10.3, 2X, F16.7, 2X, G16.3, 2X, F16.4, A20)', Nabove(tooth), avg+E, eqt, time-time0, ''

!        print *, "GREPME", cur_cp, E+E0+avg, eqt, time-time0, total_computed
        if (zmq_abort(zmq_to_qp_run_socket) == -1) then
          call sleep(1)
          if (zmq_abort(zmq_to_qp_run_socket) == -1) then
            print *, irp_here, ': Error in sending abort signal (2)'
          endif
        endif

      else
        if (cur_cp > old_cur_cp) then
          old_cur_cp = cur_cp
!          print *, "GREPME", cur_cp, E+E0+avg, eqt, time-time0, total_computed

          !print '(G10.3, 2X, F16.7, 2X, G16.3, 2X, F16.4, A20)', Nabove(tooth), avg+E, eqt, time-time0, ''
        endif
      endif
    end if
  end do pullLoop
 
  if(total_computed == N_det_generators) then
    print *, "TOTALLY COMPUTED"
    delta = 0d0
    delta_s2 = 0d0
    do i=comb_teeth+1,0,-1
      delta += delta_det(:,:,i,1)
      delta_s2 += delta_det(:,:,i,2)
    end do
  else


  delta = cp(:,:,cur_cp,1)
  delta_s2 = cp(:,:,cur_cp,2)
  
  do i=cp_first_tooth(cur_cp)-1,0,-1
    delta += delta_det(:,:,i,1)
    delta_s2 += delta_det(:,:,i,2)
  end do

  end if

  mrcc(1) = E
  
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)

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


 BEGIN_PROVIDER [ integer, gen_per_cp ]
&BEGIN_PROVIDER [ integer, comb_teeth ]
&BEGIN_PROVIDER [ integer, N_cps_max ]
  implicit none
  comb_teeth = 16
  N_cps_max = 32
  !comb_per_cp = 64
  gen_per_cp = (N_det_generators / N_cps_max) + 1
  N_cps_max += 1
  !N_cps_max = N_det_generators / comb_per_cp + 1
END_PROVIDER


 BEGIN_PROVIDER [ integer, N_cp ]
&BEGIN_PROVIDER [ double precision, cps_N, (N_cps_max) ]
&BEGIN_PROVIDER [ integer, cp_first_tooth, (N_cps_max) ]
&BEGIN_PROVIDER [ integer, done_cp_at, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, cps, (N_det_generators, N_cps_max) ]
&BEGIN_PROVIDER [ integer, N_mrcc_jobs ]
&BEGIN_PROVIDER [ integer, mrcc_jobs, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, comb, (N_det_generators) ]
! subroutine get_carlo_workbatch(Ncp, tbc, cps, done_cp_at)
  implicit none
  logical, allocatable         :: computed(:)
  integer                        :: i, j, last_full, dets(comb_teeth)
  integer                        :: k, l, cur_cp, under_det(comb_teeth+1)
  integer, allocatable :: iorder(:), first_cp(:)

  allocate(iorder(N_det_generators), first_cp(N_cps_max+1))
  allocate(computed(N_det_generators))
  first_cp = 1
  cps = 0d0
  cur_cp = 1
  done_cp_at = 0
  
  computed = .false.
  
  N_mrcc_jobs = first_det_of_comb - 1
  do i=1, N_mrcc_jobs
    mrcc_jobs(i) = i
    computed(i) = .true.
  end do
  
  l=first_det_of_comb
  call RANDOM_NUMBER(comb)
  do i=1,N_det_generators
    comb(i) = comb(i) * comb_step
    !DIR$ FORCEINLINE
    call add_comb(comb(i), computed, cps(1, cur_cp), N_mrcc_jobs, mrcc_jobs)
    
    if(N_mrcc_jobs / gen_per_cp > (cur_cp-1) .or. N_mrcc_jobs == N_det_generators) then
    !if(mod(i, comb_per_cp) == 0 .or. N_mrcc_jobs == N_det_generators) then
      first_cp(cur_cp+1) = N_mrcc_jobs
      done_cp_at(N_mrcc_jobs) = cur_cp
      cps_N(cur_cp) = dfloat(i)
      if(N_mrcc_jobs /= N_det_generators) then
        cps(:, cur_cp+1) = cps(:,  cur_cp)
        cur_cp += 1
      end if
      !cps(:, cur_cp) = cps(:, cur_cp) / dfloat(i)
      
      if (N_mrcc_jobs == N_det_generators) exit
    end if
    do while (computed(l))
      l=l+1
    enddo
    k=N_mrcc_jobs+1
    mrcc_jobs(k) = l
    computed(l) = .True.
    N_mrcc_jobs = k
  enddo
  N_cp = cur_cp
  if(N_mrcc_jobs /= N_det_generators .or. N_cp > N_cps_max) then
    print *, N_mrcc_jobs, N_det_generators, N_cp, N_cps_max
    stop "carlo workcarlo_workbatch"
  end if

  cur_cp = 0
  do i=1,N_mrcc_jobs
    if(done_cp_at(i) /= 0) cur_cp = done_cp_at(i)
    done_cp_at(i) = cur_cp
  end do
  

  under_det = 0
  cp_first_tooth = 0
  do i=1,N_mrcc_jobs
    do j=comb_teeth+1,1,-1
      if(mrcc_jobs(i) <= first_det_of_teeth(j)) then
        under_det(j) = under_det(j) + 1
        if(under_det(j) == first_det_of_teeth(j))then
          do l=done_cp_at(i)+1, N_cp
            cps(:first_det_of_teeth(j)-1, l) = 0d0
            cp_first_tooth(l) = j
          end do
          cps(first_det_of_teeth(j), done_cp_at(i)+1) = &
              cps(first_det_of_teeth(j), done_cp_at(i)+1) * fractage(j)
        end if
      else
        exit
      end if
    end do
  end do
  cps(:, N_cp) = 0d0
  cp_first_tooth(N_cp) = comb_teeth+1

  iorder = -1132154665
  do i=1,N_cp-1
    call isort(mrcc_jobs(first_cp(i)+1:first_cp(i+1)),iorder,first_cp(i+1)-first_cp(i))
  end do
! end subroutine
END_PROVIDER


subroutine get_comb_val(stato, detail, cur_cp, val)
  implicit none
  integer, intent(in)           :: cur_cp
  integer                       :: first
  double precision, intent(in)  :: stato, detail(N_states, N_det_generators)
  double precision, intent(out) :: val
  double precision :: curs
  integer :: j, k
  integer, external :: mrcc_find

  curs = 1d0 - stato
  val = 0d0
  first = cp_first_tooth(cur_cp) 

  do j = comb_teeth, first, -1
    !DIR$ FORCEINLINE
    k = mrcc_find(curs, mrcc_cweight,size(mrcc_cweight), first_det_of_teeth(j), first_det_of_teeth(j+1))
    !if(k < first_det_of_teeth(first)) exit
    if(k == first_det_of_teeth(first)) then
     val += detail(1, k) * mrcc_weight_inv(k) * comb_step * fractage(first)
    else
     val += detail(1, k) * mrcc_weight_inv(k) * comb_step
    end if

    curs -= comb_step
  end do
end subroutine


subroutine get_comb(stato, dets)
  implicit none
  double precision, intent(in) :: stato
  integer, intent(out) :: dets(comb_teeth)
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


subroutine add_comb(com, computed, cp, N, tbc)
  implicit none
  double precision, intent(in) :: com
  integer, intent(inout) :: N
  double precision, intent(inout) :: cp(N_det_non_ref)
  logical, intent(inout) :: computed(N_det_generators)
  integer, intent(inout) :: tbc(N_det_generators)
  integer :: i, k, l, dets(comb_teeth)
  
  !DIR$ FORCEINLINE
  call get_comb(com, dets)
  
  k=N+1
  do i = 1, comb_teeth
    l = dets(i)
    cp(l) += 1d0 ! mrcc_weight_inv(l) * comb_step
    if(.not.(computed(l))) then
      tbc(k) = l
      k = k+1
      computed(l) = .true.
    end if
  end do
  N = k-1
end subroutine



 BEGIN_PROVIDER [ double precision, mrcc_weight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, mrcc_weight_inv, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, mrcc_cweight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, mrcc_cweight_cache, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, fractage, (comb_teeth) ]
&BEGIN_PROVIDER [ double precision, comb_step ]
&BEGIN_PROVIDER [ integer, first_det_of_teeth, (comb_teeth+1) ]
&BEGIN_PROVIDER [ integer, first_det_of_comb ]
&BEGIN_PROVIDER [ integer, tooth_of_det, (N_det_generators) ]
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
    fractage(i) = (mrcc_cweight(iloc) - stato) / mrcc_weight(iloc)
    stato -= comb_step
  end do
  first_det_of_teeth(comb_teeth+1) = N_det_generators + 1
  first_det_of_teeth(1) = first_det_of_comb
  

  if(first_det_of_teeth(1) /= first_det_of_comb) then
     print *, 'Error in ', irp_here
     stop "comb provider"
  endif
  
  do i=1,N_det_generators
    mrcc_weight_inv(i) = 1.d0/mrcc_weight(i)
  enddo

  tooth_of_det(:first_det_of_teeth(1)-1) = 0
  do i=1,comb_teeth
    tooth_of_det(first_det_of_teeth(i):first_det_of_teeth(i+1)-1) = i
  end do

  !double precision :: cur
  !fractage = 1d0
  !do i=1,comb_teeth-1
  !  cur = 1d0 - dfloat(i)*comb_step
    
  !end do
END_PROVIDER







