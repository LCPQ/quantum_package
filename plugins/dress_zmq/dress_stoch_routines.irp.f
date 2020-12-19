BEGIN_PROVIDER [ integer, fragment_first ]
  implicit none
  fragment_first = first_det_of_teeth(1)
END_PROVIDER


subroutine ZMQ_dress(E, dress, delta_out, delta_s2_out, relative_error)
  use f77_zmq
  
  implicit none
  
  character(len=64000)           :: task
  
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket, zmq_socket_pull
  integer, external              :: omp_get_thread_num
  double precision, intent(in)   :: E(N_states), relative_error
  double precision, intent(out)  :: dress(N_states)
  double precision, intent(out)  :: delta_out(N_states, N_det)
  double precision, intent(out)  :: delta_s2_out(N_states, N_det)
  
  double precision, allocatable  :: delta(:,:)
  double precision, allocatable  :: delta_s2(:,:)
  
  integer                        :: i, j, k, Ncp
  
  double precision, external     :: omp_get_wtime
  double precision               :: time
  integer, external              :: add_task_to_taskserver
  double precision               :: state_average_weight_save(N_states)
  
  
  allocate(delta(N_states,N_det), delta_s2(N_det,N_states))
  state_average_weight_save(:) = state_average_weight(:)
  do dress_stoch_istate=1,N_states
    SOFT_TOUCH dress_stoch_istate
    state_average_weight(:) = 0.d0
    state_average_weight(dress_stoch_istate) = 1.d0
    TOUCH state_average_weight
    
    provide nproc fragment_first fragment_count mo_bielec_integrals_in_map mo_mono_elec_integral dress_weight psi_selectors
    
    
    print *, '========== ================= ================= ================='
    print *, ' Samples        Energy         Stat. Error         Seconds      '
    print *, '========== ================= ================= ================='
    
    
    call new_parallel_job(zmq_to_qp_run_socket,zmq_socket_pull, 'dress')
    
    integer, external              :: zmq_put_psi
    integer, external              :: zmq_put_N_det_generators
    integer, external              :: zmq_put_N_det_selectors
    integer, external              :: zmq_put_dvector
    integer, external              :: zmq_set_running
    if (zmq_put_psi(zmq_to_qp_run_socket,1) == -1) then
      stop 'Unable to put psi on ZMQ server'
    endif
    if (zmq_put_N_det_generators(zmq_to_qp_run_socket, 1) == -1) then
      stop 'Unable to put N_det_generators on ZMQ server'
    endif
    if (zmq_put_N_det_selectors(zmq_to_qp_run_socket, 1) == -1) then
      stop 'Unable to put N_det_selectors on ZMQ server'
    endif
    if (zmq_put_dvector(zmq_to_qp_run_socket,1,'energy',dress_e0_denominator,size(dress_e0_denominator)) == -1) then
      stop 'Unable to put energy on ZMQ server'
    endif
    
    integer(ZMQ_PTR), external     :: new_zmq_to_qp_run_socket
    integer                        :: ipos
    ipos=1
    do i=1,N_dress_jobs
      if(dress_jobs(i) > fragment_first) then
        write(task(ipos:ipos+20),'(I9,1X,I9,''|'')') 0, dress_jobs(i)
        ipos += 20
        if (ipos > 63980) then
          if (add_task_to_taskserver(zmq_to_qp_run_socket,trim(task(1:ipos))) == -1) then
            stop 'Unable to add task to task server'
          endif
          
          ipos=1
        endif
      else
        do j=1,fragment_count
          write(task(ipos:ipos+20),'(I9,1X,I9,''|'')') j, dress_jobs(i)
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
    
    !$OMP PARALLEL DEFAULT(shared) NUM_THREADS(nproc+1)              &
        !$OMP  PRIVATE(i)
    i = omp_get_thread_num()
    if (i==0) then
      call dress_collector(zmq_socket_pull,E, relative_error, delta, delta_s2, dress,&
          dress_stoch_istate)
    else
      call dress_slave_inproc(i)
    endif
    !$OMP END PARALLEL
    delta_out(dress_stoch_istate,1:N_det) = delta(dress_stoch_istate,1:N_det)
    delta_s2_out(dress_stoch_istate,1:N_det) = delta_s2_out(dress_stoch_istate,1:N_det)
    call end_parallel_job(zmq_to_qp_run_socket, zmq_socket_pull, 'dress')
    
    print *, '========== ================= ================= ================='
  enddo
  FREE dress_stoch_istate
  state_average_weight(:) = state_average_weight_save(:)
  TOUCH state_average_weight
  deallocate(delta,delta_s2)
  
end subroutine


subroutine dress_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i
  
  call run_dress_slave(1,i,dress_e0_denominator)
end



subroutine dress_collector(zmq_socket_pull, E, relative_error, delta, delta_s2, dress, istate)
  use f77_zmq
  use bitmasks
  implicit none

  

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  integer, intent(in)            :: istate

  double precision, intent(in)   :: relative_error, E(N_states)
  double precision, intent(out)  :: dress(N_states)
  double precision, allocatable  :: cp(:,:,:,:)

  double precision, intent(out)  :: delta(N_states, N_det)
  double precision, intent(out)  :: delta_s2(N_states, N_det)
  double precision, allocatable  :: delta_loc(:,:,:), delta_det(:,:,:,:)
  double precision, allocatable  :: dress_detail(:,:)
  double precision               :: dress_mwen(N_states)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_pull_socket

  integer :: more
  integer :: i, j, k, i_state, N
  integer :: task_id, ind
  double precision, save :: time0 = -1.d0
  double precision :: time, timeLast, old_tooth
  double precision, external :: omp_get_wtime
  integer :: cur_cp, old_cur_cp
  integer, allocatable :: parts_to_get(:)
  logical, allocatable :: actually_computed(:)
  integer :: total_computed
  
  delta = 0d0
  delta_s2 = 0d0
  allocate(delta_det(N_states, N_det, 0:comb_teeth+1, 2))
  allocate(cp(N_states, N_det, N_cp, 2), dress_detail(N_states, N_det))
  allocate(delta_loc(N_states, N_det, 2))
  dress_detail = 0d0
  delta_det = 0d0
  cp = 0d0
  total_computed = 0
  character*(512) :: task
  
  allocate(actually_computed(N_det_generators), parts_to_get(N_det_generators))
    
  dress_mwen =0.d0
  
  parts_to_get(:) = 1
  if(fragment_first > 0) then
    do i=1,fragment_first
      parts_to_get(i) = fragment_count
    enddo
  endif

  actually_computed = .false.

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  more = 1
  if (time0 < 0.d0) then
      call wall_time(time0)
  endif
  timeLast = time0
  cur_cp = 0
  old_cur_cp = 0
  logical :: loop
  loop = .true.

  pullLoop : do while (loop)
    call pull_dress_results(zmq_socket_pull, ind, delta_loc, task_id)
    dress_mwen(:) = 0d0 
    
    !!!!! A VERIFIER !!!!!
    do i_state=1,N_states
      do i=1, N_det
        dress_mwen(i_state) += delta_loc(i_state, i, 1) * psi_coef(i, i_state)
      end do
    end do
      
    dress_detail(:, ind) += dress_mwen(:)
    do j=1,N_cp !! optimizable
      if(cps(ind, j) > 0d0) then
        if(tooth_of_det(ind) < cp_first_tooth(j)) stop "coef on supposedely deterministic det"
        double precision :: fac
        integer :: toothMwen
        logical :: fracted
        fac = cps(ind, j) / cps_N(j) * dress_weight_inv(ind) * comb_step
        cp(1:N_states,1:N_det,j,1) += delta_loc(1:N_states,1:N_det,1) * fac
        cp(1:N_states,1:N_det,j,2) += delta_loc(1:N_states,1:N_det,2) * fac
      end if
    end do
    toothMwen = tooth_of_det(ind)
    fracted = (toothMwen /= 0)
    if(fracted) fracted = (ind == first_det_of_teeth(toothMwen))
    
    if(fracted) then
      delta_det(1:N_states,1:N_det,toothMwen-1, 1) = delta_det(1:N_states,1:N_det,toothMwen-1, 1) + delta_loc(1:N_states,1:N_det,1) * (1d0-fractage(toothMwen))
      delta_det(1:N_states,1:N_det,toothMwen-1, 2) = delta_det(1:N_states,1:N_det,toothMwen-1, 2) + delta_loc(1:N_states,1:N_det,2) * (1d0-fractage(toothMwen))
      delta_det(1:N_states,1:N_det,toothMwen  , 1) = delta_det(1:N_states,1:N_det,toothMwen  , 1) + delta_loc(1:N_states,1:N_det,1) * (fractage(toothMwen))
      delta_det(1:N_states,1:N_det,toothMwen  , 2) = delta_det(1:N_states,1:N_det,toothMwen  , 2) + delta_loc(1:N_states,1:N_det,2) * (fractage(toothMwen))
    else
      delta_det(1:N_states,1:N_det,toothMwen  , 1) = delta_det(1:N_states,1:N_det,toothMwen  , 1) + delta_loc(1:N_states,1:N_det,1)
      delta_det(1:N_states,1:N_det,toothMwen  , 2) = delta_det(1:N_states,1:N_det,toothMwen  , 2) + delta_loc(1:N_states,1:N_det,2)
    end if

    parts_to_get(ind) -= 1
    if(parts_to_get(ind) == 0) then
      actually_computed(ind) = .true.
      total_computed += 1
    end if


    integer, external :: zmq_delete_tasks
    if (zmq_delete_tasks(zmq_to_qp_run_socket,zmq_socket_pull,task_id,1,more) == -1) then
        stop 'Unable to delete tasks'
    endif
    if(more == 0) loop = .false.

    time = omp_get_wtime()
    
    if((time - timeLast > 2d0) .or. (.not. loop)) then
      timeLast = time
      cur_cp = N_cp
      
      do i=1,N_det_generators
        if(.not. actually_computed(dress_jobs(i))) then
          if(i /= 1) then
            cur_cp = done_cp_at(i-1)
          else
            cur_cp = 0
          end if
          exit
        end if
      end do
      if(cur_cp == 0) cycle pullLoop
      
      double precision :: su, su2, eqt, avg, E0, val
      integer, external :: zmq_abort

      su  = 0d0
      su2 = 0d0
      
      do i=1, int(cps_N(cur_cp))
        call get_comb_val(comb(i), dress_detail, cur_cp, val, istate)
        su  += val
        su2 += val*val
      end do
      avg = su / cps_N(cur_cp)
      eqt = dsqrt( ((su2 / cps_N(cur_cp)) - avg*avg) / cps_N(cur_cp) )
      E0 = sum(dress_detail(istate, :first_det_of_teeth(cp_first_tooth(cur_cp))-1))
      if(cp_first_tooth(cur_cp) <= comb_teeth) then
        E0 = E0 + dress_detail(istate, first_det_of_teeth(cp_first_tooth(cur_cp))) * (1d0-fractage(cp_first_tooth(cur_cp)))
      end if


      call wall_time(time)
      if ((dabs(eqt) < relative_error .and. cps_N(cur_cp) >= 30)  .or. total_computed == N_det_generators) then
        ! Termination
        print '(2X, F16.7, 2X, G16.3, 2X, F16.4, A20)', avg+E(istate)+E0, eqt, time-time0, ''
        if (zmq_abort(zmq_to_qp_run_socket) == -1) then
          call sleep(1)
          if (zmq_abort(zmq_to_qp_run_socket) == -1) then
            print *, irp_here, ': Error in sending abort signal (2)'
          endif
        endif
      else
        if (cur_cp > old_cur_cp) then
          old_cur_cp = cur_cp
          print '(2X, F16.7, 2X, G16.3, 2X, F16.4, A20)', avg+E(istate)+E0, eqt, time-time0, ''
        endif
      endif
    end if
  end do pullLoop

  if(total_computed == N_det_generators) then
    delta   (1:N_states,1:N_det) = 0d0
    delta_s2(1:N_states,1:N_det) = 0d0
    do i=comb_teeth+1,0,-1
      delta   (1:N_states,1:N_det) = delta   (1:N_states,1:N_det) + delta_det(1:N_states,1:N_det,i,1)
      delta_s2(1:N_states,1:N_det) = delta_s2(1:N_states,1:N_det) + delta_det(1:N_states,1:N_det,i,2)
    end do
  else

    delta   (1:N_states,1:N_det) = cp(1:N_states,1:N_det,cur_cp,1)
    delta_s2(1:N_states,1:N_det) = cp(1:N_states,1:N_det,cur_cp,2)
    do i=cp_first_tooth(cur_cp)-1,0,-1
      delta   (1:N_states,1:N_det) = delta   (1:N_states,1:N_det) + delta_det(1:N_states,1:N_det,i,1)
      delta_s2(1:N_states,1:N_det) = delta_s2(1:N_states,1:N_det) + delta_det(1:N_states,1:N_det,i,2)
    end do

  end if
  dress(istate) = E(istate)+E0
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
end subroutine


integer function dress_find(v, w, sze, imin, imax)
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
  do dress_find=l,h
    if(w(dress_find) >= v) then
      exit
    end if
  end do
end function


 BEGIN_PROVIDER [ integer, gen_per_cp ]
&BEGIN_PROVIDER [ integer, comb_teeth ]
&BEGIN_PROVIDER [ integer, N_cps_max ]
  implicit none
  BEGIN_DOC
! N_cps_max : max number of checkpoints
!
! gen_per_cp : number of generators per checkpoint
  END_DOC
  comb_teeth = 64
  N_cps_max = 256
  gen_per_cp = (N_det_generators / N_cps_max) + 1
END_PROVIDER


 BEGIN_PROVIDER [ integer, N_cp ]
&BEGIN_PROVIDER [ double precision, cps_N, (N_cps_max) ]
&BEGIN_PROVIDER [ integer, cp_first_tooth, (N_cps_max) ]
&BEGIN_PROVIDER [ integer, done_cp_at, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, cps, (N_det_generators, N_cps_max) ]
&BEGIN_PROVIDER [ integer, N_dress_jobs ]
&BEGIN_PROVIDER [ integer, dress_jobs, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, comb, (N_det_generators) ]
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
  
  N_dress_jobs = first_det_of_comb - 1
  do i=1, N_dress_jobs
    dress_jobs(i) = i
    computed(i) = .true.
  end do
  
  l=first_det_of_comb
  call RANDOM_NUMBER(comb)
  do i=1,N_det_generators
    comb(i) = comb(i) * comb_step
    !DIR$ FORCEINLINE
    call add_comb(comb(i), computed, cps(1, cur_cp), N_dress_jobs, dress_jobs)
    
    if(N_dress_jobs / gen_per_cp > (cur_cp-1) .or. N_dress_jobs == N_det_generators) then
      first_cp(cur_cp+1) = N_dress_jobs
      done_cp_at(N_dress_jobs) = cur_cp
      cps_N(cur_cp) = dfloat(i)
      if(N_dress_jobs /= N_det_generators) then
        cps(:, cur_cp+1) = cps(:,  cur_cp)
        cur_cp += 1
      end if
      
      if (N_dress_jobs == N_det_generators) exit
    end if
    do while (computed(l))
      l=l+1
    enddo
    k=N_dress_jobs+1
    dress_jobs(k) = l
    computed(l) = .True.
    N_dress_jobs = k
  enddo
  N_cp = cur_cp
  if(N_dress_jobs /= N_det_generators .or. N_cp > N_cps_max) then
    print *, N_dress_jobs, N_det_generators, N_cp, N_cps_max
    stop "error in jobs creation"
  end if

  cur_cp = 0
  do i=1,N_dress_jobs
    if(done_cp_at(i) /= 0) cur_cp = done_cp_at(i)
    done_cp_at(i) = cur_cp
  end do
  

  under_det = 0
  cp_first_tooth = 0
  do i=1,N_dress_jobs
    do j=comb_teeth+1,1,-1
      if(dress_jobs(i) <= first_det_of_teeth(j)) then
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

  iorder = -1
  do i=1,N_cp-1
    call isort(dress_jobs(first_cp(i)+1:first_cp(i+1)),iorder,first_cp(i+1)-first_cp(i))
  end do
END_PROVIDER


subroutine get_comb_val(stato, detail, cur_cp, val, istate)
  implicit none
  integer, intent(in)           :: cur_cp, istate
  integer                       :: first
  double precision, intent(in)  :: stato, detail(N_states, N_det_generators)
  double precision, intent(out) :: val
  double precision :: curs
  integer :: j, k
  integer, external :: dress_find

  curs = 1d0 - stato
  val = 0d0
  first = cp_first_tooth(cur_cp) 

  do j = comb_teeth, first, -1
    !DIR$ FORCEINLINE
    k = dress_find(curs, dress_cweight,size(dress_cweight), first_det_of_teeth(j), first_det_of_teeth(j+1))
    if(k == first_det_of_teeth(first)) then
     val += detail(istate, k) * dress_weight_inv(k) * comb_step * fractage(first)
    else
     val += detail(istate, k) * dress_weight_inv(k) * comb_step
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
  integer, external :: dress_find

  curs = 1d0 - stato
  do j = comb_teeth, 1, -1
    !DIR$ FORCEINLINE
    dets(j) = dress_find(curs, dress_cweight,size(dress_cweight), first_det_of_teeth(j), first_det_of_teeth(j+1))
    curs -= comb_step
  end do
end subroutine


subroutine add_comb(com, computed, cp, N, tbc)
  implicit none
  double precision, intent(in) :: com
  integer, intent(inout) :: N
  double precision, intent(inout) :: cp(N_det)
  logical, intent(inout) :: computed(N_det_generators)
  integer, intent(inout) :: tbc(N_det_generators)
  integer :: i, k, l, dets(comb_teeth)
  
  !DIR$ FORCEINLINE
  call get_comb(com, dets)
  
  k=N+1
  do i = 1, comb_teeth
    l = dets(i)
    cp(l) += 1d0
    if(.not.(computed(l))) then
      tbc(k) = l
      k = k+1
      computed(l) = .true.
    end if
  end do
  N = k-1
end subroutine


BEGIN_PROVIDER [ integer, dress_stoch_istate ]
  implicit none
  dress_stoch_istate = 1
END_PROVIDER

 
 BEGIN_PROVIDER [ double precision, dress_weight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, dress_weight_inv, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, dress_cweight, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, dress_cweight_cache, (N_det_generators) ]
&BEGIN_PROVIDER [ double precision, fractage, (comb_teeth) ]
&BEGIN_PROVIDER [ double precision, comb_step ]
&BEGIN_PROVIDER [ integer, first_det_of_teeth, (comb_teeth+1) ]
&BEGIN_PROVIDER [ integer, first_det_of_comb ]
&BEGIN_PROVIDER [ integer, tooth_of_det, (N_det_generators) ]
  implicit none
  integer :: i
  double precision :: norm_left, stato
  integer, external :: dress_find  

  dress_weight(1)  = psi_coef_generators(1,dress_stoch_istate)**2
  dress_cweight(1) = psi_coef_generators(1,dress_stoch_istate)**2
  
  do i=1,N_det_generators
    dress_weight(i) = psi_coef_generators(i,dress_stoch_istate)**2
  enddo

  ! Important to loop backwards for numerical precision
  dress_cweight(N_det_generators) = dress_weight(N_det_generators)
  do i=N_det_generators-1,1,-1
    dress_cweight(i) = dress_weight(i) + dress_cweight(i+1) 
  end do
  
  do i=1,N_det_generators
    dress_weight(i)  = dress_weight(i) / dress_cweight(1)
    dress_cweight(i) = dress_cweight(i) / dress_cweight(1)
  enddo

  do i=1,N_det_generators-1
    dress_cweight(i) = 1.d0 - dress_cweight(i+1) 
  end do
  dress_cweight(N_det_generators) = 1.d0
  
  norm_left = 1d0
  
  comb_step = 1d0/dfloat(comb_teeth)
  first_det_of_comb = 1
  do i=1,min(100,N_det_generators)
    if(dress_weight(i)/norm_left < comb_step) then
      first_det_of_comb = i
      exit
    end if
    norm_left -= dress_weight(i)
  end do
  first_det_of_comb = max(2,first_det_of_comb)
  call write_int(6, first_det_of_comb-1, 'Size of deterministic set')
  

  comb_step =  (1d0 - dress_cweight(first_det_of_comb-1)) * comb_step
  
  stato = 1d0 - comb_step
  iloc = N_det_generators
  do i=comb_teeth, 1, -1
    integer :: iloc
    iloc = dress_find(stato, dress_cweight, N_det_generators, 1, iloc)
    first_det_of_teeth(i) = iloc
    fractage(i) = (dress_cweight(iloc) - stato) / dress_weight(iloc)
    stato -= comb_step
  end do
  first_det_of_teeth(comb_teeth+1) = N_det_generators + 1
  first_det_of_teeth(1) = first_det_of_comb
  

  if(first_det_of_teeth(1) /= first_det_of_comb) then
     print *, 'Error in ', irp_here
     stop "comb provider"
  endif
  
  do i=1,N_det_generators
    dress_weight_inv(i) = 1.d0/dress_weight(i)
  enddo

  tooth_of_det(:first_det_of_teeth(1)-1) = 0
  do i=1,comb_teeth
    tooth_of_det(first_det_of_teeth(i):first_det_of_teeth(i+1)-1) = i
  end do
END_PROVIDER







