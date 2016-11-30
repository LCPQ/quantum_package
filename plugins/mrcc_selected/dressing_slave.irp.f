subroutine mrsc2_dressing_slave_tcp(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
! Task for parallel MR-SC2
  END_DOC
  call mrsc2_dressing_slave(0,i)
end


subroutine mrsc2_dressing_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
! Task for parallel MR-SC2
  END_DOC
  call mrsc2_dressing_slave(1,i)
end

subroutine mrsc2_dressing_slave(thread,iproc)
  use f77_zmq

  implicit none
  BEGIN_DOC
! Task for parallel MR-SC2
  END_DOC
  integer,  intent(in)            :: thread, iproc
!   integer                        :: j,l
  integer                        :: rc 

  integer                        :: worker_id, task_id 
  character*(512)                :: task 
 
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
 
  integer(ZMQ_PTR), external     :: new_zmq_push_socket 
  integer(ZMQ_PTR)               :: zmq_socket_push 

  double precision, allocatable  :: delta(:,:,:), delta_s2(:,:,:)
  


  integer                         :: i_state, i, i_I, J, k, k2, k1, kk, ll, degree, degree2, m, l, deg, ni, m2
  integer                         :: n(2)
  integer                         :: p1,p2,h1,h2,s1,s2, blok, I_s, J_s, kn
  logical                         :: ok
  double precision                :: phase_iI, phase_Ik, phase_Jl, phase_Ji, phase_al
  double precision                :: diI, hIi, hJi, delta_JI, dkI, HkI, ci_inv(N_states), cj_inv(N_states)
  double precision                :: contrib, contrib_s2, wall, iwall
  double precision, allocatable   :: dleat(:,:,:), dleat_s2(:,:,:)
  integer, dimension(0:2,2,2)     :: exc_iI, exc_Ik, exc_IJ
  integer(bit_kind)               :: det_tmp(N_int, 2), det_tmp2(N_int, 2), inac, virt
  integer, external               :: get_index_in_psi_det_sorted_bit, searchDet, detCmp
  logical, external               :: is_in_wavefunction, isInCassd, detEq
  integer,allocatable :: komon(:)
  logical :: komoned
  !double precision, external :: get_dij
     
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)

  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)

  allocate (dleat(N_states, N_det_non_ref, 2), delta(N_states,0:N_det_non_ref, 2))
  allocate (dleat_s2(N_states, N_det_non_ref, 2), delta_s2(N_states,0:N_det_non_ref, 2))
  allocate(komon(0:N_det_non_ref))

  do 
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task)
    if (task_id == 0) exit
    read (task,*) i_I, J, k1, k2
    do i_state=1, N_states
      ci_inv(i_state) = 1.d0 / psi_ref_coef(i_I,i_state)
      cj_inv(i_state) = 1.d0 / psi_ref_coef(J,i_state)
    end do
    n = 0
    delta(:,0,:) = 0d0
    delta(:,:nlink(J),1) = 0d0
    delta(:,:nlink(i_I),2) = 0d0
    delta_s2(:,0,:) = 0d0
    delta_s2(:,:nlink(J),1) = 0d0
    delta_s2(:,:nlink(i_I),2) = 0d0
    komon(0) = 0
    komoned = .false.
    
    
    
    
    do kk = k1, k2
      k = det_cepa0_idx(linked(kk, i_I))
      blok = blokMwen(kk, i_I)
      
      call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,k),exc_Ik,degree,phase_Ik,N_int)
    
      if(J /= i_I) then
        call apply_excitation(psi_ref(1,1,J),exc_Ik,det_tmp2,ok,N_int)
        if(.not. ok) cycle
          
        l = searchDet(det_cepa0(1,1,cepa0_shortcut(blok)), det_tmp2, cepa0_shortcut(blok+1)-cepa0_shortcut(blok), N_int)
        if(l == -1) cycle
        ll = cepa0_shortcut(blok)-1+l
        l = det_cepa0_idx(ll)
        ll = child_num(ll, J)
      else
        l = k
        ll = kk
      end if
      
        
      if(.not. komoned) then
        m = 0
        m2 = 0
          
        do while(m < nlink(i_I) .and. m2 < nlink(J))
          m += 1
          m2 += 1
          if(linked(m, i_I) < linked(m2, J)) then
            m2 -= 1
            cycle
          else if(linked(m, i_I) > linked(m2, J)) then
            m -= 1
            cycle
          end if
          i = det_cepa0_idx(linked(m, i_I))
          
          if(h_cache(J,i) == 0.d0) cycle
          if(h_cache(i_I,i) == 0.d0) cycle
          
          komon(0) += 1
          kn = komon(0)
          komon(kn) = i
          
          do i_state = 1,N_states
            dkI = h_cache(J,i) * dij(i_I, i, i_state)
            dleat(i_state, kn, 1) = dkI
            dleat(i_state, kn, 2) = dkI

            dkI = s2_cache(J,i) * dij(i_I, i, i_state)
            dleat_s2(i_state, kn, 1) = dkI
            dleat_s2(i_state, kn, 2) = dkI
          end do

        end do
          
        komoned = .true.
      end if
      
      integer :: hpmin(2)
      hpmin(1) = 2 - HP(1,k)
      hpmin(2) = 2 - HP(2,k)

      do m = 1, komon(0)
        
        i = komon(m)
        if(HP(1,i) <= hpmin(1) .and. HP(2,i) <= hpmin(2) ) then
          cycle
        end if
        
        call apply_excitation(psi_non_ref(1,1,i),exc_Ik,det_tmp,ok,N_int)
        if(.not. ok) cycle
          
        do i_state = 1, N_states 
          contrib =  dij(i_I, k, i_state) * dleat(i_state, m, 2)
          contrib_s2 =  dij(i_I, k, i_state) * dleat_s2(i_state, m, 2)
          delta(i_state,ll,1) += contrib
          delta_s2(i_state,ll,1) += contrib_s2
          if(dabs(psi_ref_coef(i_I,i_state)).ge.5.d-5) then
            delta(i_state,0,1) -= contrib * ci_inv(i_state) * psi_non_ref_coef(l,i_state)
            delta_s2(i_state,0,1) -= contrib_s2 * ci_inv(i_state) * psi_non_ref_coef(l,i_state)
          endif
          
          if(I_i == J) cycle
          contrib =  dij(J, l, i_state) * dleat(i_state, m, 1)
          contrib_s2 =  dij(J, l, i_state) * dleat_s2(i_state, m, 1)
          delta(i_state,kk,2) += contrib
          delta_s2(i_state,kk,2) += contrib_s2
          if(dabs(psi_ref_coef(J,i_state)).ge.5.d-5) then
            delta(i_state,0,2) -= contrib * cj_inv(i_state) * psi_non_ref_coef(k,i_state)
            delta_s2(i_state,0,2) -= contrib_s2 * cj_inv(i_state) * psi_non_ref_coef(k,i_state)
          end if
        enddo !i_state
      end do ! while
    end do ! kk

      
      call push_mrsc2_results(zmq_socket_push, I_i, J, delta, delta_s2, task_id) 
      call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id)
 
!     end if
    
  enddo

  deallocate(delta)

  call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)

end


subroutine push_mrsc2_results(zmq_socket_push, I_i, J, delta, delta_s2, task_id) 
  use f77_zmq 
  implicit none 
  BEGIN_DOC 
! Push integrals in the push socket 
  END_DOC 

  integer, intent(in)            :: i_I, J
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push 
  double precision,intent(inout) :: delta(N_states, 0:N_det_non_ref, 2)
  double precision,intent(inout) :: delta_s2(N_states, 0:N_det_non_ref, 2)
  integer, intent(in)            :: task_id 
  integer                        :: rc , i_state,  i, kk, li
  integer,allocatable            :: idx(:,:)
  integer                        :: n(2)
  logical :: ok
  
  allocate(idx(N_det_non_ref,2))
  rc = f77_zmq_send( zmq_socket_push, i_I, 4, ZMQ_SNDMORE) 
  if (rc /= 4) then 
    print *, irp_here,  'f77_zmq_send( zmq_socket_push, i_I, 4, ZMQ_SNDMORE)' 
    stop 'error' 
  endif 

  rc = f77_zmq_send( zmq_socket_push, J, 4, ZMQ_SNDMORE) 
  if (rc /= 4) then 
    print *, irp_here,  'f77_zmq_send( zmq_socket_push, J, 4, ZMQ_SNDMORE)' 
    stop 'error' 
  endif 
  
  
  do kk=1,2
    n(kk)=0
    if(kk == 1) li = nlink(j)
    if(kk == 2) li = nlink(i_I)
    do i=1, li
      ok = .false.
      do i_state=1,N_states
        if(delta(i_state, i, kk) /= 0d0) then
          ok = .true.
          exit
        end if
      end do
      
      if(ok) then
        n(kk) += 1
!         idx(n,kk) = i
        if(kk == 1) then
          idx(n(1),1) = det_cepa0_idx(linked(i, J))
        else
          idx(n(2),2) = det_cepa0_idx(linked(i, i_I))
        end if
        
        do i_state=1, N_states
            delta(i_state, n(kk), kk) = delta(i_state, i, kk)
        end do
      end if
    end do
    
    rc = f77_zmq_send( zmq_socket_push, n(kk), 4, ZMQ_SNDMORE) 
    if (rc /= 4) then 
      print *, irp_here,  'f77_zmq_send( zmq_socket_push, n, 4, ZMQ_SNDMORE)' 
      stop 'error' 
    endif 
    
    if(n(kk) /= 0) then
      rc = f77_zmq_send( zmq_socket_push, delta(1,0,kk), (n(kk)+1)*8*N_states, ZMQ_SNDMORE)  ! delta(1,0,1) = delta_I   delta(1,0,2) = delta_J 
      if (rc /=  (n(kk)+1)*8*N_states) then 
        print *, irp_here,  'f77_zmq_send( zmq_socket_push, delta, (n(kk)+1)*8*N_states, ZMQ_SNDMORE)'
        stop 'error' 
      endif 

      rc = f77_zmq_send( zmq_socket_push, delta_s2(1,0,kk), (n(kk)+1)*8*N_states, ZMQ_SNDMORE)  ! delta_s2(1,0,1) = delta_I   delta_s2(1,0,2) = delta_J 
      if (rc /=  (n(kk)+1)*8*N_states) then 
        print *, irp_here,  'f77_zmq_send( zmq_socket_push, delta_s2, (n(kk)+1)*8*N_states, ZMQ_SNDMORE)'
        stop 'error' 
      endif 
      
      rc = f77_zmq_send( zmq_socket_push, idx(1,kk), n(kk)*4, ZMQ_SNDMORE) 
      if (rc /=  n(kk)*4) then 
        print *, irp_here,  'f77_zmq_send( zmq_socket_push, delta, 8*n(kk), ZMQ_SNDMORE)'
        stop 'error' 
      endif 
    end if
  end do
  
  
  rc = f77_zmq_send( zmq_socket_push, task_id, 4, 0) 
  if (rc /= 4) then 
    print *, irp_here,  'f77_zmq_send( zmq_socket_push, task_id, 4, 0)' 
    stop 'error' 
  endif 
 
! ! Activate is zmq_socket_push is a REQ 
!   integer :: idummy 
!   rc = f77_zmq_recv( zmq_socket_push, idummy, 4, 0) 
!   if (rc /= 4) then 
!     print *, irp_here, 'f77_zmq_send( zmq_socket_push, idummy, 4, 0)' 
!     stop 'error' 
!   endif 
end



subroutine pull_mrsc2_results(zmq_socket_pull, I_i, J, n, idx, delta, delta_s2, task_id) 
  use f77_zmq 
  implicit none 
  BEGIN_DOC 
! Push integrals in the push socket 
  END_DOC 

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull 
  integer, intent(out)           :: i_I, J, n(2)
  double precision, intent(inout) :: delta(N_states, 0:N_det_non_ref, 2)
  double precision, intent(inout) :: delta_s2(N_states, 0:N_det_non_ref, 2)
  integer, intent(out)           :: task_id 
  integer                        :: rc , i, kk
  integer,intent(inout) :: idx(N_det_non_ref,2)
  logical :: ok
  
  rc = f77_zmq_recv( zmq_socket_pull, i_I, 4, ZMQ_SNDMORE) 
  if (rc /= 4) then 
    print *, irp_here,  'f77_zmq_recv( zmq_socket_pull, i_I, 4, ZMQ_SNDMORE)' 
    stop 'error' 
  endif 
  
  rc = f77_zmq_recv( zmq_socket_pull, J, 4, ZMQ_SNDMORE) 
  if (rc /= 4) then 
    print *, irp_here,  'f77_zmq_recv( zmq_socket_pull, J, 4, ZMQ_SNDMORE)' 
    stop 'error' 
  endif 
  
  do kk = 1, 2
    rc = f77_zmq_recv( zmq_socket_pull, n(kk), 4, ZMQ_SNDMORE) 
    if (rc /= 4) then 
      print *, irp_here,  'f77_zmq_recv( zmq_socket_pull, n, 4, ZMQ_SNDMORE)' 
      stop 'error' 
    endif 
    
    if(n(kk) /= 0) then
      rc = f77_zmq_recv( zmq_socket_pull, delta(1,0,kk), (n(kk)+1)*8*N_states, ZMQ_SNDMORE) 
      if (rc /= (n(kk)+1)*8*N_states) then 
        print *, irp_here,  'f77_zmq_recv( zmq_socket_pull, delta, (n(kk)+1)*8*N_states, ZMQ_SNDMORE)'
        stop 'error' 
      endif 
      
      rc = f77_zmq_recv( zmq_socket_pull, delta_s2(1,0,kk), (n(kk)+1)*8*N_states, ZMQ_SNDMORE) 
      if (rc /= (n(kk)+1)*8*N_states) then 
        print *, irp_here,  'f77_zmq_recv( zmq_socket_pull, delta_s2, (n(kk)+1)*8*N_states, ZMQ_SNDMORE)'
        stop 'error' 
      endif 
      
      rc = f77_zmq_recv( zmq_socket_pull, idx(1,kk), n(kk)*4, ZMQ_SNDMORE) 
      if (rc /= n(kk)*4) then 
        print *, irp_here,  'f77_zmq_recv( zmq_socket_pull, idx(1,kk), n(kk)*4, ZMQ_SNDMORE)'
        stop 'error' 
      endif 
    end if
  end do
  
  rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0) 
  if (rc /= 4) then 
    print *, irp_here,  'f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)' 
    stop 'error' 
  endif 

 
! ! Activate is zmq_socket_pull is a REP 
!   integer :: idummy 
!   rc = f77_zmq_send( zmq_socket_pull, idummy, 4, 0) 
!   if (rc /= 4) then 
!     print *, irp_here, 'f77_zmq_send( zmq_socket_pull, idummy, 4, 0)' 
!     stop 'error' 
!   endif 
end



subroutine mrsc2_dressing_collector(delta_ii_,delta_ij_,delta_ii_s2_,delta_ij_s2_)
  use f77_zmq
  implicit none
  BEGIN_DOC 
! Collects results from the AO integral calculation 
  END_DOC 
 
  double precision,intent(inout)               :: delta_ij_(N_states,N_det_non_ref,N_det_ref) 
  double precision,intent(inout)               :: delta_ii_(N_states,N_det_ref)
  double precision,intent(inout)               :: delta_ij_s2_(N_states,N_det_non_ref,N_det_ref) 
  double precision,intent(inout)               :: delta_ii_s2_(N_states,N_det_ref)

!   integer                        :: j,l
  integer                        :: rc 
   
  double precision, allocatable  :: delta(:,:,:), delta_s2(:,:,:) 
   
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
   
  integer(ZMQ_PTR), external     :: new_zmq_pull_socket 
  integer(ZMQ_PTR)               :: zmq_socket_pull 
   
  integer*8                      :: control, accu 
  integer                        :: task_id, more 
 
  integer                        :: I_i, J, l, i_state, n(2), kk
  integer,allocatable :: idx(:,:)
  
  delta_ii_(:,:) = 0d0
  delta_ij_(:,:,:) = 0d0
  delta_ii_s2_(:,:) = 0d0
  delta_ij_s2_(:,:,:) = 0d0

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket() 
  zmq_socket_pull = new_zmq_pull_socket() 
 
  allocate ( delta(N_states,0:N_det_non_ref,2), delta_s2(N_states,0:N_det_non_ref,2) ) 
  
  allocate(idx(N_det_non_ref,2))
  more = 1 
  do while (more == 1) 
         
    call pull_mrsc2_results(zmq_socket_pull, I_i, J, n, idx, delta, delta_s2, task_id)
    

      do l=1, n(1)
        do i_state=1,N_states
          delta_ij_(i_state,idx(l,1),i_I) += delta(i_state,l,1)
          delta_ij_s2_(i_state,idx(l,1),i_I) += delta_s2(i_state,l,1)
        end do
      end do
      
      do l=1, n(2)
        do i_state=1,N_states
          delta_ij_(i_state,idx(l,2),J) += delta(i_state,l,2)
          delta_ij_s2_(i_state,idx(l,2),J) += delta_s2(i_state,l,2)
        end do
      end do

    
       if(n(1) /= 0) then 
       do i_state=1,N_states
         delta_ii_(i_state,i_I) += delta(i_state,0,1)
         delta_ii_s2_(i_state,i_I) += delta_s2(i_state,0,1)
       end do
       end if

      if(n(2) /= 0) then 
       do i_state=1,N_states
         delta_ii_(i_state,J) += delta(i_state,0,2)
         delta_ii_s2_(i_state,J) += delta_s2(i_state,0,2)
       end do
       end if


    if (task_id /= 0) then 
      call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id,more) 
    endif 

 
  enddo 
  deallocate( delta, delta_s2 )
 
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_pull_socket(zmq_socket_pull)

end




 BEGIN_PROVIDER [ double precision, delta_ij_old, (N_states,N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ii_old, (N_states,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ij_s2_old, (N_states,N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ii_s2_old, (N_states,N_det_ref) ]
  implicit none
  
  integer                         :: i_state, i, i_I, J, k, kk, degree, degree2, m, l, deg, ni, m2
  integer                         :: p1,p2,h1,h2,s1,s2, blok, I_s, J_s, nex, nzer, ntot
!   integer, allocatable            :: linked(:,:), blokMwen(:, :), nlink(:)
  logical                         :: ok
  double precision                :: phase_iI, phase_Ik, phase_Jl, phase_Ji, phase_al, diI, hIi, hJi, delta_JI, dkI(N_states), HkI, ci_inv(N_states), dia_hla(N_states)
  double precision                :: contrib, wall, iwall ! , searchance(N_det_ref)
  integer, dimension(0:2,2,2)     :: exc_iI, exc_Ik, exc_IJ
  integer(bit_kind)               :: det_tmp(N_int, 2), det_tmp2(N_int, 2), inac, virt
  integer, external               :: get_index_in_psi_det_sorted_bit, searchDet, detCmp
  logical, external               :: is_in_wavefunction, isInCassd, detEq
  character*(512)                :: task 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
  
  integer :: KKsize = 1000000
  
  
  call new_parallel_job(zmq_to_qp_run_socket,'mrsc2')


  call wall_time(iwall)
!   allocate(linked(N_det_non_ref, N_det_ref), blokMwen(N_det_non_ref, N_det_ref), nlink(N_det_ref))
  

!   searchance = 0d0
!   do J = 1, N_det_ref
!     nlink(J) = 0
!     do blok=1,cepa0_shortcut(0)
!     do k=cepa0_shortcut(blok), cepa0_shortcut(blok+1)-1
!       call get_excitation_degree(psi_ref(1,1,J),det_cepa0(1,1,k),degree,N_int)
!       if(degree <= 2) then
!         nlink(J) += 1
!         linked(nlink(J),J) = k
!         blokMwen(nlink(J),J) = blok
!         searchance(J) += 1d0 + log(dfloat(cepa0_shortcut(blok+1) - cepa0_shortcut(blok)))
!       end if
!     end do
!     end do
!   end do

     
     
!   stop
  nzer = 0
  ntot = 0
  do nex = 3, 0, -1
    print *, "los ",nex
    do I_s = N_det_ref, 1, -1
!         if(mod(I_s,1) == 0) then
!           call wall_time(wall)
!           wall = wall-iwall
!           print *, I_s, "/", N_det_ref, wall * (dfloat(N_det_ref) / dfloat(I_s)), wall, wall * (dfloat(N_det_ref) / dfloat(I_s))-wall
!         end if


      do J_s = 1, I_s
      
        call get_excitation_degree(psi_ref(1,1,J_s), psi_ref(1,1,I_s), degree, N_int)
        if(degree /= nex) cycle
        if(nex == 3) nzer = nzer + 1
        ntot += 1
!           if(degree > 3) then
!             deg += 1
!             cycle
!           else if(degree == -10) then
!             KKsize = 100000
!           else
!             KKsize = 1000000
!           end if
        
        
        
        if(searchance(I_s) < searchance(J_s)) then
          i_I = I_s
          J = J_s
        else
          i_I = J_s
          J = I_s
        end if
        
        KKsize = nlink(1)
        if(nex == 0) KKsize = int(float(nlink(1)) / float(nlink(i_I)) * (float(nlink(1)) / 64d0))
        
        !if(KKsize == 0) stop "ZZEO"
        
        do kk = 1 , nlink(i_I), KKsize
          write(task,*) I_i, J, kk, int(min(kk+KKsize-1, nlink(i_I)))
          call add_task_to_taskserver(zmq_to_qp_run_socket,task)
        end do
        
  !       do kk = 1 , nlink(i_I)
  !         k = linked(kk,i_I)
  !         blok = blokMwen(kk,i_I)
  !         write(task,*) I_i, J, k, blok
  !         call add_task_to_taskserver(zmq_to_qp_run_socket,task)
  !         
  !       enddo !kk
      enddo !J
          
    enddo !I
  end do ! nex
  print *, "tasked"
!  integer(ZMQ_PTR)               ∷ collector_thread
!  external                       ∷ ao_bielec_integrals_in_map_collector
!  rc = pthread_create(collector_thread, mrsc2_dressing_collector)
  print *, nzer, ntot, float(nzer) / float(ntot)
  provide nproc
  !$OMP PARALLEL DEFAULT(none)  SHARED(delta_ii_old,delta_ij_old,delta_ii_s2_old,delta_ij_s2_old)  PRIVATE(i) NUM_THREADS(nproc+1)
      i = omp_get_thread_num()
      if (i==0) then
        call mrsc2_dressing_collector(delta_ii_old,delta_ij_old,delta_ii_s2_old,delta_ij_s2_old)
      else
        call mrsc2_dressing_slave_inproc(i)
      endif
  !$OMP END PARALLEL

!  rc = pthread_join(collector_thread)
  call end_parallel_job(zmq_to_qp_run_socket, 'mrsc2')
    

END_PROVIDER



