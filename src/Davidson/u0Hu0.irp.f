subroutine u_0_H_u_0(e_0,u_0,n,keys_tmp,Nint,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes e_0 = <u_0|H|u_0>/<u_0|u_0>
  !
  ! n : number of determinants
  !
  END_DOC
  integer, intent(in)            :: n,Nint, N_st, sze_8
  double precision, intent(out)  :: e_0(N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  
  double precision, allocatable  :: H_jj(:), v_0(:,:)
  double precision               :: u_dot_u,u_dot_v,diag_H_mat_elem
  integer :: i,j
  allocate (H_jj(n), v_0(sze_8,N_st))
  do i = 1, n
   H_jj(i) = diag_H_mat_elem(keys_tmp(1,1,i),Nint)
  enddo
  
  call H_u_0_nstates(v_0,u_0,H_jj,n,keys_tmp,Nint,N_st,sze_8)
  do i=1,N_st
    e_0(i) = u_dot_v(v_0(1,i),u_0(1,i),n)/u_dot_u(u_0(1,i),n)
  enddo
  deallocate (H_jj, v_0)
end


subroutine H_u_0_nstates(v_0,u_0,H_jj,n,keys_tmp,Nint,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> 
  !
  ! n : number of determinants
  !
  ! H_jj : array of <j|H|j>
  !
  END_DOC
  integer, intent(in)            :: N_st,n,Nint, sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  double precision               :: hij,s2 
  double precision, allocatable  :: vt(:,:), ut(:,:), st(:,:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  
  integer, allocatable           :: shortcut(:,:), sort_idx(:,:)
  integer(bit_kind), allocatable :: sorted(:,:,:), version(:,:,:)
  integer(bit_kind)              :: sorted_i(Nint)
  
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, istate
  integer                        :: N_st_8
  
  integer, external              :: align_double
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: vt, ut, st

  N_st_8 = align_double(N_st)

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy

  allocate (shortcut(0:n+1,2), sort_idx(n,2), sorted(Nint,n,2), version(Nint,n,2))
  allocate( ut(N_st_8,n))

  v_0 = 0.d0

  call sort_dets_ab_v(keys_tmp, sorted(1,1,1), sort_idx(1,1), shortcut(0,1), version(1,1,1), n, Nint)
  call sort_dets_ba_v(keys_tmp, sorted(1,1,2), sort_idx(1,2), shortcut(0,2), version(1,1,2), n, Nint)

  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,s2,j,k,jj,vt,st,ii,sh,sh2,ni,exa,ext,org_i,org_j,endi,sorted_i,istate)&
      !$OMP SHARED(n,keys_tmp,ut,Nint,u_0,v_0,sorted,shortcut,sort_idx,version,N_st,N_st_8) 
  allocate(vt(N_st_8,n),st(N_st_8,n))
  Vt = 0.d0
  St = 0.d0

  !$OMP DO
  do i=1,n
    do istate=1,N_st
      ut(istate,i) = u_0(sort_idx(i,2),istate)
    enddo
  enddo
  !$OMP END DO

  !$OMP DO SCHEDULE(static,1)
  do sh=1,shortcut(0,2)
    do i=shortcut(sh,2),shortcut(sh+1,2)-1
      org_i = sort_idx(i,2)
      do j=shortcut(sh,2),shortcut(sh+1,2)-1
        org_j = sort_idx(j,2)
        ext = popcnt(xor(sorted(1,i,2), sorted(1,j,2)))
        if (ext > 4) cycle
        do ni=2,Nint
          ext = ext + popcnt(xor(sorted(ni,i,2), sorted(ni,j,2)))
          if (ext > 4) exit
        end do
        if(ext == 4) then
            call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
              st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
            enddo
        end if
      end do
    end do
  enddo
  !$OMP END DO

  !$OMP DO
  do i=1,n
    do istate=1,N_st
      ut(istate,i) = u_0(sort_idx(i,1),istate)
    enddo
  enddo
  !$OMP END DO

  !$OMP DO SCHEDULE(static,1)
  do sh=1,shortcut(0,1)
    do sh2=1,shortcut(0,1)
      if (sh==sh2) cycle

      exa = 0
      do ni=1,Nint
        exa = exa + popcnt(xor(version(ni,sh,1), version(ni,sh2,1)))
      end do
      if(exa > 2) then
        cycle
      end if

      do i=shortcut(sh,1),shortcut(sh+1,1)-1
        org_i = sort_idx(i,1)
        do ni=1,Nint
          sorted_i(ni) = sorted(ni,i,1)
        enddo

        do j=shortcut(sh2,1),shortcut(sh2+1,1)-1
          ext = exa + popcnt(xor(sorted_i(1), sorted(1,j,1)))
          if (ext > 4) cycle
          do ni=2,Nint
            ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
            if (ext > 4) exit
          end do
          if(ext <= 4) then
            org_j = sort_idx(j,1)
            call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
            if (hij /= 0.d0) then
              do istate=1,n_st
                vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
              enddo
            endif
            if (ext /= 2) then
              call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
              if (s2 /= 0.d0) then
                do istate=1,n_st
                  st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
                enddo
              endif
            endif
          endif
        enddo
        
      enddo
    enddo

    exa = 0

    do i=shortcut(sh,1),shortcut(sh+1,1)-1
      org_i = sort_idx(i,1)
      do ni=1,Nint
        sorted_i(ni) = sorted(ni,i,1)
      enddo

      do j=shortcut(sh,1),i-1
        ext = exa + popcnt(xor(sorted_i(1), sorted(1,j,1)))
        if (ext > 4) cycle
        do ni=2,Nint
          ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
          if (ext > 4) exit
        end do
        if(ext <= 4) then
          org_j = sort_idx(j,1)
          call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
          if (hij /= 0.d0) then
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
            enddo
          endif
          if (ext /= 2) then
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            if (s2 /= 0.d0) then
              do istate=1,n_st
                st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
              enddo
            endif
          endif
        endif
      enddo
      
      do j=i+1,shortcut(sh+1,1)-1
        if (i==j) cycle
        ext = exa + popcnt(xor(sorted_i(1), sorted(1,j,1)))
        if (ext > 4) cycle
        do ni=2,Nint
          ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
          if (ext > 4) exit
        end do
        if(ext <= 4) then
          org_j = sort_idx(j,1)
          call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
          if (hij /= 0.d0) then
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
            enddo
          endif
          if (ext /= 2) then
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            if (s2 /= 0.d0) then
              do istate=1,n_st
                st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
              enddo
            endif
          endif
        endif
      enddo
    enddo
  enddo
  !$OMP END DO 

  do istate=1,N_st
    do i=1,n
      !$OMP ATOMIC
      v_0(i,istate) = v_0(i,istate) + vt(istate,i)
    enddo
  enddo

  deallocate(vt,st)
  !$OMP END PARALLEL

  do istate=1,N_st
    do i=1,n
      v_0(i,istate) = v_0(i,istate) + H_jj(i) * u_0(i,istate)
    enddo
  enddo
  deallocate (shortcut, sort_idx, sorted, version, ut)
end


BEGIN_PROVIDER [ double precision, psi_energy, (N_states) ]
  implicit none
  BEGIN_DOC
! Energy of the current wave function
  END_DOC
  call u_0_H_u_0(psi_energy,psi_coef,N_det,psi_det,N_int,N_states,psi_det_size)
END_PROVIDER


subroutine H_S2_u_0_nstates_zmq(v_0,s_0,u_0,H_jj,S2_jj,n,keys_tmp,Nint,N_st,sze_8,update_dets)
  use omp_lib
  use bitmasks
  use f77_zmq
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! n : number of determinants
  !
  ! H_jj : array of <j|H|j>
  !
  ! S2_jj : array of <j|S^2|j>
  END_DOC
  integer, intent(in)            :: N_st,n,Nint, sze_8, update_dets
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n), S2_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  double precision               :: hij,s2 
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0, ithread
  
  integer(bit_kind)              :: sorted_i(Nint)
  
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, istate
  integer                        :: N_st_8
  
  integer, external              :: align_double
  integer :: blockb2, istep
  double precision :: ave_workload, workload, target_workload_inv
  
  integer(ZMQ_PTR) :: handler
  
  if(N_st /= N_states_diag .or. sze_8 < N_det) stop "assert fail in H_S2_u_0_nstates"
  N_st_8 = N_st ! align_double(N_st)

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy 

  v_0 = 0.d0
  s_0 = 0.d0
  
  call davidson_init(handler,keys_tmp,u_0,size(u_0,1),n,N_st,update_dets)

  ave_workload = 0.d0
  do sh=1,shortcut_(0,1)
    ave_workload += shortcut_(0,1)
    ave_workload += (shortcut_(sh+1,1) - shortcut_(sh,1))**2
    do i=sh, shortcut_(0,2), shortcut_(0,1)
      do j=i, min(i, shortcut_(0,2))
        ave_workload += (shortcut_(j+1,2) - shortcut_(j, 2))**2
      end do
    end do
  enddo
  ave_workload = ave_workload/dble(shortcut_(0,1))
  target_workload_inv = 0.01d0/ave_workload

  PROVIDE nproc


  character(len=:), allocatable :: task
  task = repeat(' ', iposmax)
  character(32) :: tmp_task
  integer :: ipos, iposmax
  iposmax = shortcut_(0,1)+32
  ipos = 1
  do sh=1,shortcut_(0,1),1
    workload = shortcut_(0,1)+dble(shortcut_(sh+1,1) - shortcut_(sh,1))**2
    do i=sh, shortcut_(0,2), shortcut_(0,1)
      do j=i, min(i, shortcut_(0,2))
        workload += (shortcut_(j+1,2) - shortcut_(j, 2))**2
      end do
    end do
!    istep = 1+ int(workload*target_workload_inv)
    istep = 1
    do blockb2=0, istep-1
      write(tmp_task,'(3(I9,1X),''|'',1X)') sh, blockb2, istep
      task = task//tmp_task
      ipos += 32
      if (ipos+32 > iposmax) then
        call add_task_to_taskserver(handler, trim(task))
        ipos=1
        task = ''
      endif
    enddo
  enddo
  if (ipos>1)  then
     call add_task_to_taskserver(handler, trim(task))
  endif

  !$OMP PARALLEL NUM_THREADS(nproc+2) PRIVATE(ithread)
  ithread = omp_get_thread_num()
  if (ithread == 0 ) then
    call zmq_set_running(handler)
    call davidson_run(handler, v_0, s_0, size(v_0,1))
  else if (ithread == 1 ) then
    call davidson_miniserver_run (update_dets)
  else
    call davidson_slave_inproc(ithread)
  endif
  !$OMP END PARALLEL

  call end_parallel_job(handler, 'davidson')

  do istate=1,N_st
    do i=1,n
      v_0(i,istate) = v_0(i,istate) + H_jj(i) * u_0(i,istate)
      s_0(i,istate) = s_0(i,istate) + s2_jj(i)* u_0(i,istate)
    enddo
  enddo
end



subroutine H_S2_u_0_nstates(v_0,s_0,u_0,H_jj,S2_jj,n,keys_tmp,Nint,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! n : number of determinants
  !
  ! H_jj : array of <j|H|j>
  !
  ! S2_jj : array of <j|S^2|j>
  END_DOC
  integer, intent(in)            :: N_st,n,Nint, sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n), S2_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  double precision               :: hij,s2 
  double precision, allocatable  :: vt(:,:), ut(:,:), st(:,:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  
  integer, allocatable           :: shortcut(:,:), sort_idx(:,:)
  integer(bit_kind), allocatable :: sorted(:,:,:), version(:,:,:)
  integer(bit_kind)              :: sorted_i(Nint)
  
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, istate
  integer                        :: N_st_8
  
  integer, external              :: align_double
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: vt, ut, st

  N_st_8 = align_double(N_st)

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy

  allocate (shortcut(0:n+1,2), sort_idx(n,2), sorted(Nint,n,2), version(Nint,n,2))
  allocate( ut(N_st_8,n))

  v_0 = 0.d0
  s_0 = 0.d0

  call sort_dets_ab_v(keys_tmp, sorted(1,1,1), sort_idx(1,1), shortcut(0,1), version(1,1,1), n, Nint)
  call sort_dets_ba_v(keys_tmp, sorted(1,1,2), sort_idx(1,2), shortcut(0,2), version(1,1,2), n, Nint)

  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,s2,j,k,jj,vt,st,ii,sh,sh2,ni,exa,ext,org_i,org_j,endi,sorted_i,istate)&
      !$OMP SHARED(n,keys_tmp,ut,Nint,u_0,v_0,s_0,sorted,shortcut,sort_idx,version,N_st,N_st_8) 
  allocate(vt(N_st_8,n),st(N_st_8,n))
  Vt = 0.d0
  St = 0.d0

  !$OMP DO
  do i=1,n
    do istate=1,N_st
      ut(istate,i) = u_0(sort_idx(i,2),istate)
    enddo
  enddo
  !$OMP END DO

  !$OMP DO SCHEDULE(static,4)
  do sh=1,shortcut(0,2)
    do i=shortcut(sh,2),shortcut(sh+1,2)-1
      org_i = sort_idx(i,2)
      do j=shortcut(sh,2),shortcut(sh+1,2)-1
        org_j = sort_idx(j,2)
        ext = popcnt(xor(sorted(1,i,2), sorted(1,j,2)))
        if (ext > 4) cycle
        do ni=2,Nint
          ext = ext + popcnt(xor(sorted(ni,i,2), sorted(ni,j,2)))
          if (ext > 4) exit
        end do
        if(ext == 4) then
            call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
              st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
            enddo
        end if
      end do
    end do
  enddo
  !$OMP END DO

  !$OMP DO
  do i=1,n
    do istate=1,N_st
      ut(istate,i) = u_0(sort_idx(i,1),istate)
    enddo
  enddo
  !$OMP END DO

  !$OMP DO SCHEDULE(static,4)
  do sh=1,shortcut(0,1)
    do sh2=1,shortcut(0,1)
      if (sh==sh2) cycle

      exa = 0
      do ni=1,Nint
        exa = exa + popcnt(xor(version(ni,sh,1), version(ni,sh2,1)))
      end do
      if(exa > 2) then
        cycle
      end if

      do i=shortcut(sh,1),shortcut(sh+1,1)-1
        org_i = sort_idx(i,1)
        do ni=1,Nint
          sorted_i(ni) = sorted(ni,i,1)
        enddo

        do j=shortcut(sh2,1),shortcut(sh2+1,1)-1
          ext = exa + popcnt(xor(sorted_i(1), sorted(1,j,1)))
          if (ext > 4) cycle
          do ni=2,Nint
            ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
            if (ext > 4) exit
          end do
          if(ext <= 4) then
            org_j = sort_idx(j,1)
            call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
            if (hij /= 0.d0) then
              do istate=1,n_st
                vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
              enddo
            endif
            if (ext /= 2) then
              call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
              if (s2 /= 0.d0) then
                do istate=1,n_st
                  st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
                enddo
              endif
            endif
          endif
        enddo
        
      enddo
    enddo

    exa = 0

    do i=shortcut(sh,1),shortcut(sh+1,1)-1
      org_i = sort_idx(i,1)
      do ni=1,Nint
        sorted_i(ni) = sorted(ni,i,1)
      enddo

      do j=shortcut(sh,1),i-1
        ext = exa + popcnt(xor(sorted_i(1), sorted(1,j,1)))
        if (ext > 4) cycle
        do ni=2,Nint
          ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
          if (ext > 4) exit
        end do
        if(ext <= 4) then
          org_j = sort_idx(j,1)
          call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
          if (hij /= 0.d0) then
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
            enddo
          endif
          if (ext /= 2) then
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            if (s2 /= 0.d0) then
              do istate=1,n_st
                st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
              enddo
            endif
          endif
        endif
      enddo
      
      do j=i+1,shortcut(sh+1,1)-1
        ext = exa + popcnt(xor(sorted_i(1), sorted(1,j,1)))
        if (ext > 4) cycle
        do ni=2,Nint
          ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
          if (ext > 4) exit
        end do
        if(ext <= 4) then
          org_j = sort_idx(j,1)
          call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
          if (hij /= 0.d0) then
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,j)
            enddo
          endif
          if (ext /= 2) then
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            if (s2 /= 0.d0) then
              do istate=1,n_st
                st (istate,org_i) = st (istate,org_i) + s2*ut(istate,j)
              enddo
            endif
          endif
        endif
      enddo
    enddo
  enddo
  !$OMP END DO 

  do istate=1,N_st
    do i=1,n
      !$OMP ATOMIC
      v_0(i,istate) = v_0(i,istate) + vt(istate,i)
      !$OMP ATOMIC
      s_0(i,istate) = s_0(i,istate) + st(istate,i)
    enddo
  enddo

  deallocate(vt,st)
  !$OMP END PARALLEL

  do istate=1,N_st
    do i=1,n
      v_0(i,istate) = v_0(i,istate) + H_jj(i) * u_0(i,istate)
      s_0(i,istate) = s_0(i,istate) + s2_jj(i)* u_0(i,istate)
    enddo
  enddo
  deallocate (shortcut, sort_idx, sorted, version, ut)
end





subroutine H_S2_u_0_nstates_new(v_0,s_0,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! n : number of determinants
  !
  ! H_jj : array of <j|H|j>
  !
  ! S2_jj : array of <j|S^2|j>
  END_DOC
  integer, intent(in)            :: N_st,sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st)

  
  PROVIDE ref_bitmask_energy

  double precision               :: hij, s2 
  integer                        :: i,j
  integer                        :: k_a, k_b, l_a, l_b, m_a, m_b
  integer                        :: degree, istate
  integer                        :: krow, kcol, krow_b, kcol_b
  integer                        :: lrow, lcol
  integer                        :: mrow, mcol
  integer(bit_kind)              :: spindet(N_int)
  integer(bit_kind)              :: tmp_det(N_int,2)
  integer(bit_kind)              :: tmp_det2(N_int,2)
  integer(bit_kind)              :: tmp_det3(N_int,2)
  integer(bit_kind), allocatable :: buffer(:,:)
  double precision               :: ck(N_st), cl(N_st), cm(N_st)
  integer                        :: n_singles, n_doubles
  integer, allocatable           :: singles(:), doubles(:)
  integer, allocatable           :: idx(:), idx0(:)
  logical, allocatable           :: is_single_a(:)

  allocate( buffer(N_int,N_det_alpha_unique),                        &
      singles(N_det_alpha_unique), doubles(N_det_alpha_unique),      &
      is_single_a(N_det_alpha_unique),                               &
      idx(N_det_alpha_unique), idx0(N_det_alpha_unique) )
  
  v_0 = 0.d0

  do k_a=1,N_det-1
    
    ! Initial determinant is at k_a in alpha-major representation
    ! -----------------------------------------------------------------------
    
    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    ! Initial determinant is at k_b in beta-major representation
    ! ----------------------------------------------------------------------
    
    k_b = psi_bilinear_matrix_order_reverse(k_a)

    ! Diagonal contribution
    ! ---------------------

    double precision, external :: diag_H_mat_elem
  
    v_0(k_a,1:N_st) = v_0(k_a,1:N_st) + diag_H_mat_elem(tmp_det,N_int) * &
        psi_bilinear_matrix_values(k_a,1:N_st)

    
    ! Get all single and double alpha excitations
    ! ===========================================
    
    spindet(1:N_int) = tmp_det(1:N_int,1)
    
    ! Loop inside the beta column to gather all the connected alphas
    i=1
    l_a = k_a+1
    lcol = psi_bilinear_matrix_columns(l_a)
    do while ( (lcol == kcol).and.(l_a <= N_det) )
      lrow = psi_bilinear_matrix_rows(l_a)
      buffer(1:N_int,i) = psi_det_alpha_unique(1:N_int, lrow)
      idx(i) = lrow
      i=i+1
      l_a = l_a + 1
      lcol = psi_bilinear_matrix_columns(l_a)
    enddo
    i = i-1
    
    call get_all_spin_singles_and_doubles(                           &
        buffer, idx, spindet, N_int, i,                              &
        singles, doubles, n_singles, n_doubles )

    ! Compute Hij for all alpha singles
    ! ----------------------------------

    l_a = k_a
    lrow = psi_bilinear_matrix_rows(l_a)
    tmp_det2(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    do i=1,n_singles
      do while ( lrow < singles(i) )
        l_a = l_a+1
        lrow = psi_bilinear_matrix_rows(l_a)
      enddo
      tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, lrow)
      call i_H_j_mono_spin( tmp_det, tmp_det2, N_int, 1, hij)
      v_0(l_a, 1:N_st) += hij * psi_bilinear_matrix_values(k_a,1:N_st)
      v_0(k_a, 1:N_st) += hij * psi_bilinear_matrix_values(l_a,1:N_st)
    enddo
    
    ! Compute Hij for all alpha doubles
    ! ----------------------------------
    
    l_a = k_a
    lrow = psi_bilinear_matrix_rows(l_a)
    do i=1,n_doubles
      do while (lrow < doubles(i))
        l_a = l_a+1
        lrow = psi_bilinear_matrix_rows(l_a)
      enddo
      call i_H_j_double_spin( tmp_det(1,1), psi_det_alpha_unique(1, doubles(i)), N_int, hij)
      v_0(l_a, 1:N_st) += hij * psi_bilinear_matrix_values(k_a,1:N_st)
      v_0(k_a, 1:N_st) += hij * psi_bilinear_matrix_values(l_a,1:N_st)
    enddo
    
    
    
    ! Get all single and double beta excitations
    ! ===========================================
    
    spindet(1:N_int) = tmp_det(1:N_int,2)
    
    ! Loop inside the alpha row to gather all the connected betas
    i=1
    l_b = k_b+1
    lrow = psi_bilinear_matrix_transp_rows(l_b)
    do while ( (lrow == krow).and.(l_b <= N_det) )
      lcol = psi_bilinear_matrix_transp_columns(l_b)
      buffer(1:N_int,i) = psi_det_beta_unique(1:N_int, lcol)
      idx(i) = lcol
      i=i+1
      l_b = l_b + 1
      lrow = psi_bilinear_matrix_transp_rows(l_b)
    enddo
    i = i-1
    
    call get_all_spin_singles_and_doubles(                           &
        buffer, idx, spindet, N_int, i,                              &
        singles, doubles, n_singles, n_doubles )
    
    ! Compute Hij for all beta singles
    ! ----------------------------------
    
    l_b = k_b
    lcol = psi_bilinear_matrix_transp_columns(l_b)
    tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    do i=1,n_singles
      do while ( lcol < singles(i) )
        l_b = l_b+1
        lcol = psi_bilinear_matrix_transp_columns(l_b)
      enddo
      tmp_det2(1:N_int,2) = psi_det_beta_unique (1:N_int, lcol)
      l_a = psi_bilinear_matrix_transp_order(l_b)
      call i_H_j_mono_spin( tmp_det, tmp_det2, N_int, 2, hij)
      v_0(l_a, 1:N_st) += hij * psi_bilinear_matrix_values(k_a,1:N_st)
      v_0(k_a, 1:N_st) += hij * psi_bilinear_matrix_values(l_a,1:N_st)
    enddo
    
    ! Compute Hij for all beta doubles
    ! ----------------------------------
    
    l_b = k_b
    lcol = psi_bilinear_matrix_transp_columns(l_b)
    do i=1,n_doubles
      do while (lcol < doubles(i))
        l_b = l_b+1
        lcol = psi_bilinear_matrix_transp_columns(l_b)
      enddo
      l_a = psi_bilinear_matrix_transp_order(l_b)
      call i_H_j_double_spin( tmp_det(1,2), psi_det_beta_unique(1, doubles(i)), N_int, hij)
      v_0(l_a, 1:N_st) += hij * psi_bilinear_matrix_values(k_a,1:N_st)
      v_0(k_a, 1:N_st) += hij * psi_bilinear_matrix_values(l_a,1:N_st)
    enddo
    
  end do


  ! Alpha/Beta double excitations
  ! =============================
    
  do i=1,N_det_beta_unique
    idx0(i) = i
  enddo
  is_single_a(:) = .False.

  k_a=1
  do i=1,N_det_beta_unique

    ! Select a beta determinant
    ! -------------------------

    spindet(1:N_int) = psi_det_beta_unique(1:N_int, i)
    tmp_det(1:N_int,2) = spindet(1:N_int)

    call get_all_spin_singles(                                       &
        psi_det_beta_unique, idx0, spindet, N_int, N_det_beta_unique, &
        singles, n_singles )

    do j=1,n_singles
      is_single_a( singles(j) ) = .True.
    enddo

    ! For all alpha.beta pairs with the selected beta
    ! -----------------------------------------------

    kcol = psi_bilinear_matrix_columns(k_a)
    do while (kcol < i)
      k_a = k_a+1
      if (k_a > N_det) exit
      kcol = psi_bilinear_matrix_columns(k_a)
    enddo

    do while (kcol == i)

      krow = psi_bilinear_matrix_rows(k_a)
      tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int,krow)

      ! Loop over all alpha.beta pairs with a single exc alpha
      ! ------------------------------------------------------

      l_a = k_a+1
      if (l_a > N_det) exit
      lrow = psi_bilinear_matrix_rows(l_a)
      lcol = psi_bilinear_matrix_columns(l_a)

      do while (lrow == krow)

        ! Loop over all alpha.beta pairs with a single exc alpha
        ! ------------------------------------------------------
        if (is_single_a(lrow)) then

           tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int,lrow)
          
           ! Build list of singly excited beta
           ! ---------------------------------

           m_b = psi_bilinear_matrix_order_reverse(l_a)
           m_b = m_b+1
           j=1
           do while ( (mrow == lrow) )
             mcol = psi_bilinear_matrix_transp_columns(m_b)
             buffer(1:N_int,j) = psi_det_beta_unique(1:N_int,mcol)
             idx(j) = mcol
             j = j+1
             m_b = m_b+1
             if (m_b <= N_det) exit
             mrow = psi_bilinear_matrix_transp_rows(m_b)
           enddo
           j=j-1

           call get_all_spin_singles(                                &
               buffer, idx, tmp_det(1,2), N_int, j,                  &
               doubles, n_doubles)

           ! Compute Hij for all doubles 
           ! ---------------------------

           m_b = psi_bilinear_matrix_order(l_a)+1
           mcol = psi_bilinear_matrix_transp_columns(m_b)
           do j=1,n_doubles
             tmp_det2(1:N_int,2) = psi_det_beta_unique(1:N_int, doubles(j) )
             call i_H_j_double_alpha_beta(tmp_det,tmp_det2,N_int,hij)
             do while (mcol /= doubles(j))
               m_b = m_b+1
               if (m_b > N_det) exit
               mcol = psi_bilinear_matrix_transp_columns(m_b)
             enddo
             m_a = psi_bilinear_matrix_order_reverse(m_b)
!             v_0(m_a, 1:N_st) += hij * psi_bilinear_matrix_values(k_a,1:N_st)
!             v_0(k_a, 1:N_st) += hij * psi_bilinear_matrix_values(m_a,1:N_st)
           enddo

        endif
        l_a = l_a+1
        if (l_a > N_det) exit
        lrow = psi_bilinear_matrix_rows(l_a)
        lcol = psi_bilinear_matrix_columns(l_a)
      enddo

      k_b = k_b+1
      if (k_b > N_det) exit
      kcol = psi_bilinear_matrix_transp_columns(k_b)
    enddo
    
    do j=1,n_singles
      is_single_a( singles(j) ) = .False.
    enddo

  enddo  

    
end


subroutine H_S2_u_0_nstates_test(v_0,s_0,u_0,H_jj,S2_jj,n,keys_tmp,Nint,N_st,sze_8)
  use bitmasks
  implicit none
  integer, intent(in)            :: N_st,n,Nint, sze_8
  integer(bit_kind), intent(in)  :: keys_tmp(Nint,2,n)
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n), S2_jj(n)
  
  PROVIDE ref_bitmask_energy

  double precision, allocatable :: vt(:,:)
  integer, allocatable :: idx(:)
  integer                        :: i,j, jj
  double precision               :: hij

  do i=1,n
    v_0(i,:) = H_jj(i) * u_0(i,:)
  enddo

  allocate(idx(0:n), vt(N_st,n))
  Vt = 0.d0
  do i=2,n
    idx(0) = i
    call filter_connected(keys_tmp,keys_tmp(1,1,i),Nint,i-1,idx)
    do jj=1,idx(0)
      j = idx(jj)
      double precision :: phase
      integer :: degree
      integer :: exc(0:2,2,2)
      call get_excitation(keys_tmp(1,1,j),keys_tmp(1,1,i),exc,degree,phase,Nint)
      if ((degree == 2).and.(exc(0,1,1)==1)) cycle
!      if (exc(0,1,2) /= 0) cycle
      call i_H_j(keys_tmp(1,1,j),keys_tmp(1,1,i),Nint,hij)
      vt (:,i) = vt (:,i) + hij*u_0(j,:)
      vt (:,j) = vt (:,j) + hij*u_0(i,:)
    enddo
  enddo
  do i=1,n
    v_0(i,:) = v_0(i,:) + vt(:,i)
  enddo
end

