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
  END_DOC
  integer, intent(in)            :: N_st,n,Nint, sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  double precision               :: hij
  double precision, allocatable  :: vt(:,:)
  double precision, allocatable  :: ut(:,:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  
  integer, allocatable           :: shortcut(:,:), sort_idx(:,:)
  integer(bit_kind), allocatable :: sorted(:,:,:), version(:,:,:)
  integer(bit_kind)              :: sorted_i(Nint)
  
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, istate
  integer                        :: N_st_8
  
  integer, external              :: align_double
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: vt, ut

  N_st_8 = align_double(N_st)

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy 

  allocate (shortcut(0:n+1,2), sort_idx(n,2), sorted(Nint,n,2), version(Nint,n,2))
  allocate(ut(N_st_8,n))

  v_0 = 0.d0

  do i=1,n
    do istate=1,N_st
      ut(istate,i) = u_0(i,istate)
    enddo
  enddo

  call sort_dets_ab_v(keys_tmp, sorted(1,1,1), sort_idx(1,1), shortcut(0,1), version(1,1,1), n, Nint)
  call sort_dets_ba_v(keys_tmp, sorted(1,1,2), sort_idx(1,2), shortcut(0,2), version(1,1,2), n, Nint)
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,j,k,jj,vt,ii,sh,sh2,ni,exa,ext,org_i,org_j,endi,sorted_i,istate)&
      !$OMP SHARED(n,H_jj,keys_tmp,ut,Nint,v_0,sorted,shortcut,sort_idx,version,N_st,N_st_8)
  allocate(vt(N_st_8,n))
  Vt = 0.d0
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0,1)
    do sh2=sh,shortcut(0,1)
      exa = 0
      do ni=1,Nint
        exa = exa + popcnt(xor(version(ni,sh,1), version(ni,sh2,1)))
      end do
      if(exa > 2) then
        cycle
      end if
      
      do i=shortcut(sh,1),shortcut(sh+1,1)-1
        org_i = sort_idx(i,1)
        if(sh==sh2) then
          endi = i-1
        else
          endi = shortcut(sh2+1,1)-1
        end if
        do ni=1,Nint
          sorted_i(ni) = sorted(ni,i,1)
        enddo
        
        do j=shortcut(sh2,1),endi
          org_j = sort_idx(j,1)
          ext = exa
          do ni=1,Nint
            ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
          end do
          if(ext <= 4) then
            call i_H_j(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),Nint,hij)
            do istate=1,N_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,org_j)
              vt (istate,org_j) = vt (istate,org_j) + hij*ut(istate,org_i)
            enddo
          endif
        enddo
      enddo
    enddo
  enddo
  !$OMP END DO NOWAIT
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0,2)
    do i=shortcut(sh,2),shortcut(sh+1,2)-1
      org_i = sort_idx(i,2)
      do j=shortcut(sh,2),i-1
        org_j = sort_idx(j,2)
        ext = 0
        do ni=1,Nint
          ext = ext + popcnt(xor(sorted(ni,i,2), sorted(ni,j,2)))
        end do
        if(ext == 4) then
          call i_H_j(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),Nint,hij)
          do istate=1,N_st
            vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,org_j)
            vt (istate,org_j) = vt (istate,org_j) + hij*ut(istate,org_i)
          enddo
        end if
      end do
    end do
  enddo
  !$OMP END DO NOWAIT
  
  !$OMP CRITICAL
  do istate=1,N_st
    do i=n,1,-1
      v_0(i,istate) = v_0(i,istate) + vt(istate,i)
    enddo
  enddo
  !$OMP END CRITICAL

  deallocate(vt)
  !$OMP END PARALLEL
  
  do istate=1,N_st
    do i=1,n
      v_0(i,istate) += H_jj(i) * u_0(i,istate)
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


subroutine H_S2_u_0_nstates(v_0,s_0,u_0,H_jj,S2_jj,n,keys_tmp,Nint,N_st,sze_8)
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
  integer, intent(in)            :: N_st,n,Nint, sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n), S2_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  double precision               :: hij,s2 
  double precision, allocatable  :: ut(:,:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  
  integer, allocatable           :: shortcut(:,:), sort_idx(:)
  integer(bit_kind), allocatable :: sorted(:,:), version(:,:)
  integer(bit_kind)              :: sorted_i(Nint)
  
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, istate
  integer                        :: N_st_8
  
  integer, external              :: align_double
  integer :: blockb, blockb2, istep
  double precision :: ave_workload, workload
  
  integer(ZMQ_PTR) :: handler
  
  if(N_st /= N_states_diag .or. sze_8 < N_det) stop "assert fail in H_S2_u_0_nstates"
  N_st_8 = N_st ! align_double(N_st)

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy 

  allocate (shortcut(0:n+1,2), sort_idx(n), sorted(Nint,n), version(Nint,n))
  allocate(ut(N_st_8,n))

  v_0 = 0.d0
  s_0 = 0.d0
  
  do i=1,n
    do istate=1,N_st
      ut(istate,i) =  u_0(i,istate)
    enddo
  enddo
   call sort_dets_ab_v(keys_tmp, sorted, sort_idx, shortcut(0,1), version, n, Nint)
   call sort_dets_ba_v(keys_tmp, sorted, sort_idx, shortcut(0,2), version, n, Nint)
    
  blockb = shortcut(0,1)
  call davidson_init(handler,n,N_st_8,ut)


  ave_workload = 0.d0
  do sh=1,shortcut(0,1)
    ave_workload += shortcut(0,1)
    ave_workload += (shortcut(sh+1,1) - shortcut(sh,1))**2
    do i=sh, shortcut(0,2), shortcut(0,1)
      do j=i, min(i, shortcut(0,2))
        ave_workload += (shortcut(j+1,2) - shortcut(j, 2))**2
      end do
    end do
  enddo
  ave_workload = ave_workload/dble(shortcut(0,1))


  print *,  'Ave workload :', ave_workload

  do sh=shortcut(0,1),1,-1
    workload = shortcut(0,1)+dble(shortcut(sh+1,1) - shortcut(sh,1))**2
    do i=sh, shortcut(0,2), shortcut(0,1)
      do j=i, min(i, shortcut(0,2))
        workload += (shortcut(j+1,2) - shortcut(j, 2))**2
      end do
    end do
    istep = 1+ int(0.5d0*workload/ave_workload)
    do blockb2=0, istep-1
      call davidson_add_task(handler, sh, blockb2, istep)
    enddo
  enddo
  
  call davidson_run(handler, v_0, s_0, size(v_0,1))

  do istate=1,N_st
    do i=1,n
      v_0(i,istate) = v_0(i,istate) + H_jj(i) * u_0(i,istate)
      s_0(i,istate) = s_0(i,istate) + s2_jj(i)* u_0(i,istate)
    enddo
  enddo
  deallocate(shortcut, sort_idx, sorted, version)
  deallocate(ut)
end



