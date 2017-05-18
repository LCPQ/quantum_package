
subroutine H_u_0_nstates(v_0,u_0,H_jj,n,keys_tmp,Nint,N_st,sze)
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
  integer, intent(in)            :: N_st,n,Nint, sze
  double precision, intent(out)  :: v_0(sze,N_st)
  double precision, intent(in)   :: u_0(sze,N_st)
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





subroutine H_S2_u_0_nstates(v_0,s_0,u_0,H_jj,S2_jj,n,keys_tmp,Nint,N_st,sze)
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
  integer, intent(in)            :: N_st,n,Nint, sze
  double precision, intent(out)  :: v_0(sze,N_st), s_0(sze,N_st)
  double precision, intent(in)   :: u_0(sze,N_st)
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

subroutine H_S2_u_0_nstates_test(v_0,s_0,u_0,H_jj,S2_jj,n,keys_tmp,Nint,N_st,sze)
  use bitmasks
  implicit none
  integer, intent(in)            :: N_st,n,Nint, sze
  integer(bit_kind), intent(in)  :: keys_tmp(Nint,2,n)
  double precision, intent(inout)   :: v_0(sze,N_st), s_0(sze,N_st)
  double precision, intent(in)   :: u_0(sze,N_st)
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
!      if ((degree == 1)) cycle
!      if (exc(0,1,2) /= 0) cycle
!      if (exc(0,1,1) == 2) cycle
!      if (exc(0,1,2) == 2) cycle
!      if ((degree==1).and.(exc(0,1,1) == 1)) cycle
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

