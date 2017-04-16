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





subroutine H_S2_u_0_nstates_openmp(v_0,s_0,u_0,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! Assumes that the determinants are in psi_det
  END_DOC
  integer, intent(in)            :: N_st,sze_8
  double precision, intent(inout)  :: v_0(sze_8,N_st), s_0(sze_8,N_st), u_0(sze_8,N_st)
  integer :: k
  do k=1,N_st
    call dset_order(u_0(1,k),psi_bilinear_matrix_order,N_det)
  enddo
  call H_S2_u_0_nstates_bilinear_order(v_0,s_0,u_0,N_st,sze_8)
  do k=1,N_st
    call dset_order(v_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
    call dset_order(s_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
    call dset_order(u_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
  enddo

end

subroutine H_S2_u_0_nstates_bilinear_order(v_0,s_0,u_0,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  END_DOC
  integer, intent(in)            :: N_st,sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st), u_0(sze_8,N_st)

  
  PROVIDE ref_bitmask_energy

  double precision               :: hij, sij
  integer                        :: i,j,k,l
  integer                        :: k_a, k_b, l_a, l_b, m_a, m_b
  integer                        :: istate
  integer                        :: krow, kcol, krow_b, kcol_b
  integer                        :: lrow, lcol
  integer                        :: mrow, mcol
  integer(bit_kind)              :: spindet(N_int)
  integer(bit_kind)              :: tmp_det(N_int,2)
  integer(bit_kind)              :: tmp_det2(N_int,2)
  integer(bit_kind)              :: tmp_det3(N_int,2)
  integer(bit_kind), allocatable :: buffer(:,:)
  integer                        :: n_singles, n_doubles
  integer, allocatable           :: singles(:), doubles(:)
  integer, allocatable           :: singles_b(:,:)
  integer, allocatable           :: idx(:), idx0(:)
  logical, allocatable           :: is_single_a(:)
  integer                        :: maxab, n_singles_max, kcol_prev, nmax
  double precision, allocatable  :: u_t(:,:), v_t(:,:), s_t(:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: v_t, s_t, u_t

  maxab = max(N_det_alpha_unique, N_det_beta_unique)+1
  allocate(idx0(maxab), u_t(N_st,N_det) )
  
  do i=1,maxab
    idx0(i) = i
  enddo

  call dtranspose(                                                   &
      u_0,                                                           &
      size(u_0, 1),                                                  &
      u_t,                                                           &
      size(u_t, 1),                                                  &
      N_det, N_st)

  v_0 = 0.d0
  s_0 = 0.d0

  ! Prepare the array of all alpha single excitations
  ! -------------------------------------------------

  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP   SHARED(psi_bilinear_matrix_rows, N_det,                &
      !$OMP          psi_bilinear_matrix_columns,                    &
      !$OMP          psi_det_alpha_unique, psi_det_beta_unique,      &
      !$OMP          n_det_alpha_unique, n_det_beta_unique, N_int,   &
      !$OMP          psi_bilinear_matrix_transp_rows,                &
      !$OMP          psi_bilinear_matrix_transp_columns,             &
      !$OMP          psi_bilinear_matrix_transp_order, N_st,         &
      !$OMP          psi_bilinear_matrix_order_transp_reverse,       &
      !$OMP          singles_alpha, psi_bilinear_matrix_columns_loc, &
      !$OMP          singles_alpha_size, sze_8,                      &
      !$OMP          idx0, u_t, maxab, v_0, s_0)                     &
      !$OMP   PRIVATE(krow, kcol, tmp_det, spindet, k_a, k_b, i,     &
      !$OMP          lcol, lrow, is_single_a,l_a, l_b, nmax,         &
      !$OMP          buffer, singles, doubles, n_singles, n_doubles, &
      !$OMP          tmp_det2, hij, sij, idx, l, kcol_prev, v_t, s_t)
  
  ! Alpha/Beta double excitations
  ! =============================
    
  allocate( buffer(N_int,maxab),                                     &
      singles(maxab),                                   &
      doubles(maxab),                                                &
      idx(maxab),                                                    &
      v_t(N_st,N_det), s_t(N_st,N_det),                              &
      is_single_a(N_det_alpha_unique))
  is_single_a = .False.
  kcol_prev=-1
  krow=1

  v_t = 0.d0
  s_t = 0.d0


  !$OMP DO SCHEDULE(static,1)
  do k_a=1,N_det
    do k=1,singles_alpha(0,krow)
      is_single_a( singles_alpha(k,krow) ) = .False.
    enddo

    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    do k=1,singles_alpha(0,krow)
      is_single_a( singles_alpha(k,krow) ) = .True.
    enddo

    if (kcol /= kcol_prev) then
      call get_all_spin_singles(                                   &
          psi_det_beta_unique, idx0, tmp_det(1,2), N_int, N_det_beta_unique,&
          singles, n_singles)
    endif
    kcol_prev = kcol

    ! Loop over singly excited beta columns
    ! -------------------------------------

    do i=1,n_singles
      lcol = singles(i)
      if (lcol <= kcol) cycle

      tmp_det2(1:N_int,2) = psi_det_beta_unique(1:N_int, lcol)

      l_a = psi_bilinear_matrix_columns_loc(lcol)

      ! Loop over alpha singles
      ! -----------------------

      do while ( l_a < psi_bilinear_matrix_columns_loc(lcol+1) )
        do l=l_a,psi_bilinear_matrix_columns_loc(lcol+1)-1
          lrow = psi_bilinear_matrix_rows(l)
          if (is_single_a(lrow)) exit
        enddo
        if (l >= psi_bilinear_matrix_columns_loc(lcol+1)) exit
        l_a = l
        tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, lrow)
        call i_H_j_double_alpha_beta(tmp_det,tmp_det2,N_int,hij)
        call get_s2(tmp_det,tmp_det2,N_int,sij)
        do l=1,N_st
          v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
          v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
          s_t(l,k_a) = s_t(l,k_a) + sij * u_t(l,l_a)
          s_t(l,l_a) = s_t(l,l_a) + sij * u_t(l,k_a)
        enddo
        l_a = l_a+1
      enddo
    enddo

  enddo
  !$OMP END DO NOWAIT


  ! Single and double alpha excitations
  ! ===================================
    
  !$OMP DO SCHEDULE(static,1)
  do k_a=1,N_det
    
    ! Initial determinant is at k_a in alpha-major representation
    ! -----------------------------------------------------------------------
    
    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    ! Initial determinant is at k_b in beta-major representation
    ! ----------------------------------------------------------------------
    
    k_b = psi_bilinear_matrix_order_transp_reverse(k_a)

    spindet(1:N_int) = tmp_det(1:N_int,1)
    
    ! Loop inside the beta column to gather all the connected alphas
    l_a = k_a+1
    nmax = min(N_det_alpha_unique, N_det - l_a)
    do i=1,nmax
      lcol = psi_bilinear_matrix_columns(l_a)
      if (lcol /= kcol) exit
      lrow = psi_bilinear_matrix_rows(l_a)
      buffer(1:N_int,i) = psi_det_alpha_unique(1:N_int, lrow)
      idx(i) = l_a
      l_a = l_a+1
    enddo
    i = i-1
    
    call get_all_spin_singles_and_doubles(                           &
        buffer, idx, spindet, N_int, i,                              &
        singles, doubles, n_singles, n_doubles )

    ! Compute Hij for all alpha singles
    ! ----------------------------------

    tmp_det2(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    do i=1,n_singles
      l_a = singles(i)
      lrow = psi_bilinear_matrix_rows(l_a)
      tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, lrow)
      call i_H_j_mono_spin( tmp_det, tmp_det2, N_int, 1, hij)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! single => sij = 0 
      enddo
    enddo

    
    ! Compute Hij for all alpha doubles
    ! ----------------------------------
    
    do i=1,n_doubles
      l_a = doubles(i)
      lrow = psi_bilinear_matrix_rows(l_a)
      call i_H_j_double_spin( tmp_det(1,1), psi_det_alpha_unique(1, lrow), N_int, hij)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! same spin => sij = 0
      enddo
    enddo
    
  end do
  !$OMP END DO NOWAIT


  ! Single and double beta excitations
  ! ==================================

  !$OMP DO SCHEDULE(static,1)
  do k_b=1,N_det

    ! Initial determinant is at k_b in beta-major representation
    ! -----------------------------------------------------------------------
    
    krow = psi_bilinear_matrix_transp_rows(k_b)
    kcol = psi_bilinear_matrix_transp_columns(k_b)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    spindet(1:N_int) = tmp_det(1:N_int,2)
    k_a = psi_bilinear_matrix_transp_order(k_b)
    
    ! Loop inside the alpha row to gather all the connected betas
    l_b = k_b+1
    nmax = min(N_det_beta_unique, N_det - l_b)
    do i=1,nmax
      lrow = psi_bilinear_matrix_transp_rows(l_b)
      if (lrow /= krow) exit
      lcol = psi_bilinear_matrix_transp_columns(l_b)
      buffer(1:N_int,i) = psi_det_beta_unique(1:N_int, lcol)
      idx(i) = l_b
      l_b = l_b+1
    enddo
    i = i-1
  
    call get_all_spin_singles_and_doubles(                           &
        buffer, idx, spindet, N_int, i,                              &
        singles, doubles, n_singles, n_doubles )
    
    ! Compute Hij for all beta singles
    ! ----------------------------------
    
    tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    do i=1,n_singles
      l_b = singles(i)
      lcol = psi_bilinear_matrix_transp_columns(l_b)
      tmp_det2(1:N_int,2) = psi_det_beta_unique (1:N_int, lcol)
      call i_H_j_mono_spin( tmp_det, tmp_det2, N_int, 2, hij)
      l_a = psi_bilinear_matrix_transp_order(l_b)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! single => sij = 0 
      enddo
    enddo
    
    ! Compute Hij for all beta doubles
    ! ----------------------------------
    
    do i=1,n_doubles
      l_b = doubles(i)
      lcol = psi_bilinear_matrix_transp_columns(l_b)
      call i_H_j_double_spin( tmp_det(1,2), psi_det_beta_unique(1, lcol), N_int, hij)
      l_a = psi_bilinear_matrix_transp_order(l_b)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! same spin => sij = 0 
      enddo
    enddo

  end do
  !$OMP END DO NOWAIT


  ! Diagonal contribution
  ! =====================

  !$OMP DO SCHEDULE(static,1)
  do k_a=1,N_det
    
    ! Initial determinant is at k_a in alpha-major representation
    ! -----------------------------------------------------------------------
    
    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    double precision, external :: diag_H_mat_elem, diag_S_mat_elem
  
    hij = diag_H_mat_elem(tmp_det,N_int) 
    sij = diag_S_mat_elem(tmp_det,N_int)
    do l=1,N_st
      v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,k_a)
      s_t(l,k_a) = s_t(l,k_a) + sij * u_t(l,k_a)
    enddo

  end do
  !$OMP END DO NOWAIT

  !$OMP CRITICAL
  do l=1,N_st
    do i=1, N_det
      v_0(i,l) = v_0(i,l) + v_t(l,i)
      s_0(i,l) = s_0(i,l) + s_t(l,i)
    enddo
  enddo
  !$OMP END CRITICAL

  !$OMP BARRIER
  !$OMP END PARALLEL

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
  integer                        :: i,j, jj, l
  double precision               :: hij

  do i=1,n
    v_0(i,:) = H_jj(i) * u_0(i,:)
  enddo

  allocate(idx(0:n), vt(N_st,n))
  Vt = 0.d0
  !$OMP PARALLEL DO DEFAULT(shared) PRIVATE(i,idx,jj,j,degree,exc,phase,hij,l) SCHEDULE(static,1)
  do i=2,n
    idx(0) = i
    call filter_connected(keys_tmp,keys_tmp(1,1,i),Nint,i-1,idx)
    do jj=1,idx(0)
      j = idx(jj)
      double precision :: phase
      integer :: degree
      integer :: exc(0:2,2,2)
      call get_excitation(keys_tmp(1,1,j),keys_tmp(1,1,i),exc,degree,phase,Nint)
!       if ((degree == 2).and.(exc(0,1,1)==1)) then
!         continue
!       else
!         cycle
!       endif
!       if ((degree == 2).and.(exc(0,1,1)==1)) cycle
!      if ((degree > 1)) cycle
!      if (exc(0,1,2) /= 0) cycle
!      if (exc(0,1,1) == 2) cycle
!      if (exc(0,1,2) == 2) cycle
!      if ((degree==1).and.(exc(0,1,2) == 1)) cycle
      call i_H_j(keys_tmp(1,1,j),keys_tmp(1,1,i),Nint,hij)
      do l=1,N_st
        !$OMP ATOMIC
        vt (l,i) = vt (l,i) + hij*u_0(j,l)
        !$OMP ATOMIC
        vt (l,j) = vt (l,j) + hij*u_0(i,l)
      enddo
    enddo
  enddo
  !$OMP END PARALLEL DO
  do i=1,n
    v_0(i,:) = v_0(i,:) + vt(:,i)
  enddo
end

