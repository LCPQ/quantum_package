


subroutine davidson_diag_mrcc(dets_in,u_in,energies,dim_in,sze,N_st,Nint,iunit,istate)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization.
  !
  ! dets_in : bitmasks corresponding to determinants
  !
  ! u_in : guess coefficients on the various states. Overwritten
  !   on exit
  !
  ! dim_in : leftmost dimension of u_in
  !
  ! sze : Number of determinants
  !
  ! N_st : Number of eigenstates
  !
  ! iunit : Unit number for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, Nint, iunit, istate
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision, intent(inout) :: u_in(dim_in,N_st)
  double precision, intent(out)  :: energies(N_st)
  double precision, allocatable  :: H_jj(:)
  
  double precision               :: diag_h_mat_elem
  integer                        :: i
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  PROVIDE mo_bielec_integrals_in_map
  allocate(H_jj(sze))
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,H_jj,N_det_ref,dets_in,Nint,istate,delta_ii,idx_ref)           &
      !$OMP  PRIVATE(i)
  !$OMP DO SCHEDULE(guided)
  do i=1,sze
    H_jj(i) = diag_h_mat_elem(dets_in(1,1,i),Nint) 
  enddo
  !$OMP END DO 
  !$OMP DO SCHEDULE(guided)
  do i=1,N_det_ref
    H_jj(idx_ref(i)) +=  delta_ii(i,istate)
  enddo
  !$OMP END DO 
  !$OMP END PARALLEL

  call davidson_diag_hjj_mrcc(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,Nint,iunit,istate)
  deallocate (H_jj)
end

subroutine davidson_diag_hjj_mrcc(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,Nint,iunit,istate)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization with specific diagonal elements of the H matrix
  !
  ! H_jj : specific diagonal H matrix elements to diagonalize de Davidson
  !
  ! dets_in : bitmasks corresponding to determinants
  !
  ! u_in : guess coefficients on the various states. Overwritten
  !   on exit
  !
  ! dim_in : leftmost dimension of u_in
  !
  ! sze : Number of determinants
  !
  ! N_st : Number of eigenstates
  !
  ! iunit : Unit for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, Nint, istate
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(sze)
  integer,  intent(in)  :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st)
  double precision, intent(out)  :: energies(N_st)
  
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision               :: overlap(N_st,N_st)
  double precision               :: u_dot_v, u_dot_u
  
  integer, allocatable           :: kl_pairs(:,:)
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, allocatable  :: W(:,:,:),  U(:,:,:), R(:,:)
  double precision, allocatable  :: y(:,:,:,:), h(:,:,:,:), lambda(:)
  double precision               :: diag_h_mat_elem
  double precision               :: residual_norm(N_st)
  character*(16384)              :: write_buffer
  double precision               :: to_print(2,N_st)
  double precision               :: cpu, wall
  
  integer(bit_kind)              :: dets_in_sorted(Nint,2,sze)
  integer                        :: idx(sze), shortcut(0:sze+1),sh,ii,tmp
  
  !PROVIDE det_connections

  call write_time(iunit)
  call wall_time(wall)
  call cpu_time(cpu)
  write(iunit,'(A)') ''
  write(iunit,'(A)') 'Davidson Diagonalization'
  write(iunit,'(A)') '------------------------'
  write(iunit,'(A)') ''
  call write_int(iunit,N_st,'Number of states')
  call write_int(iunit,sze,'Number of determinants')
  write(iunit,'(A)') ''
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = ' Iter'
  do i=1,N_st
    write_buffer = trim(write_buffer)//'           Energy         Residual'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)

  allocate(                                                          &
      kl_pairs(2,N_st*(N_st+1)/2),                                   &
      W(sze,N_st,davidson_sze_max),                                                   &
      U(sze,N_st,davidson_sze_max),                                  &
      R(sze,N_st),                                                   &
      h(N_st,davidson_sze_max,N_st,davidson_sze_max),                &
      y(N_st,davidson_sze_max,N_st,davidson_sze_max),                &
      lambda(N_st*davidson_sze_max))
  
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Initialization
  ! ==============
  
  
  dets_in_sorted(:,:,:) = dets_in(:,:,:)
  call sort_dets_ab(dets_in_sorted, idx, shortcut, sze, Nint)
  
  k_pairs=0
  do l=1,N_st
    do k=1,l
      k_pairs+=1
      kl_pairs(1,k_pairs) = k
      kl_pairs(2,k_pairs) = l
    enddo
  enddo
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(U,sze,N_st,overlap,kl_pairs,k_pairs,             &
      !$OMP  Nint,dets_in,u_in)                                 &
      !$OMP  PRIVATE(k,l,kl,i)
  
  
  ! Orthonormalize initial guess
  ! ============================
  
  !$OMP DO
  do kl=1,k_pairs
    k = kl_pairs(1,kl)
    l = kl_pairs(2,kl)
    if (k/=l) then
      overlap(k,l) = u_dot_v(U_in(1,k),U_in(1,l),sze)
      overlap(l,k) = overlap(k,l)
    else
      overlap(k,k) = u_dot_u(U_in(1,k),sze)
    endif
  enddo
  !$OMP END DO
  !$OMP END PARALLEL

  call ortho_lowdin(overlap,size(overlap,1),N_st,U_in,size(U_in,1),sze)
  
  ! Davidson iterations
  ! ===================
  
  converged = .False.
  
  do while (.not.converged)
    
    !$OMP PARALLEL DEFAULT(NONE)                                     &
        !$OMP PRIVATE(k,i) SHARED(U,u_in,sze,N_st)
    do k=1,N_st
      !$OMP DO
      do i=1,sze
        U(i,k,1) = u_in(i,k)
      enddo
      !$OMP END DO 
    enddo
    !$OMP END PARALLEL
    
    do iter=1,davidson_sze_max-1
      
      ! Compute W_k = H |u_k>
      ! ----------------------
      
      do k=1,N_st
        !call H_u_0_mrcc(W(1,k,iter),U(1,k,iter),H_jj,sze,dets_in_sorted,shortcut,idx,Nint,istate)
        !call H_u_0_mrcc_org(W(1,k,iter),U(1,k,iter),H_jj,sze,dets_in,Nint,istate)
        call H_u_0_mrcc(W(1,k,iter),U(1,k,iter),H_jj,sze,dets_in,Nint,istate)
      enddo
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------

      do l=1,N_st
        do k=1,N_st
          do iter2=1,iter-1
            h(k,iter2,l,iter) = u_dot_v(U(1,k,iter2),W(1,l,iter),sze)
            h(k,iter,l,iter2) = h(k,iter2,l,iter)
          enddo
        enddo
        do k=1,l
          h(k,iter,l,iter) = u_dot_v(U(1,k,iter),W(1,l,iter),sze)
          h(l,iter,k,iter) = h(k,iter,l,iter)
        enddo
      enddo

      !DEBUG H MATRIX
      !do i=1,iter
      !  print '(10(x,F16.10))',  h(1,i,1,1:i)
      !enddo
      !print *,  ''
      !END
      
      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,N_st*davidson_sze_max,N_st*iter)
      
      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      do k=1,N_st
        do i=1,sze
          U(i,k,iter+1) = 0.d0
          W(i,k,iter+1) = 0.d0
          do l=1,N_st
            do iter2=1,iter
              U(i,k,iter+1) = U(i,k,iter+1) + U(i,l,iter2)*y(l,iter2,k,1)
              W(i,k,iter+1) = W(i,k,iter+1) + W(i,l,iter2)*y(l,iter2,k,1)
            enddo
          enddo
        enddo
      enddo
      
      ! Compute residual vector
      ! -----------------------
      
      do k=1,N_st
        do i=1,sze
          R(i,k) = lambda(k) * U(i,k,iter+1) - W(i,k,iter+1)
        enddo
        residual_norm(k) = u_dot_u(R(1,k),sze)
        to_print(1,k) = lambda(k) + nuclear_repulsion
        to_print(2,k) = residual_norm(k)
      enddo

      write(iunit,'(X,I3,X,100(X,F16.10,X,E16.6))'), iter, to_print(:,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      if (converged) then
        exit
      endif
      
      
      ! Davidson step
      ! -------------
      
      do k=1,N_st
        do i=1,sze
          U(i,k,iter+1) = -1.d0/max(H_jj(i) - lambda(k),1.d-2) * R(i,k)
        enddo
      enddo
      
      ! Gram-Schmidt
      ! ------------
      
      double precision               :: c
      do k=1,N_st
        do iter2=1,iter
          do l=1,N_st
            c = u_dot_v(U(1,k,iter+1),U(1,l,iter2),sze)
            do i=1,sze
              U(i,k,iter+1) -= c * U(i,l,iter2)
            enddo
          enddo
        enddo
        do l=1,k-1
          c = u_dot_v(U(1,k,iter+1),U(1,l,iter+1),sze)
          do i=1,sze
            U(i,k,iter+1) -= c * U(i,l,iter+1)
          enddo
        enddo
        call normalize( U(1,k,iter+1), sze )
      enddo
    
      !DEBUG : CHECK OVERLAP
      !print *,  '==='
      !do k=1,iter+1
      !  do l=1,k
      !  c = u_dot_v(U(1,1,k),U(1,1,l),sze)
      !  print *,  k,l, c
      !  enddo
      !enddo
      !print *,  '==='
      !pause
      !END DEBUG


    enddo

    if (.not.converged) then
      iter = davidson_sze_max-1
    endif
    
    ! Re-contract to u_in
    ! -----------
    
    do k=1,N_st
      energies(k) = lambda(k)
      do i=1,sze
        u_in(i,k) = 0.d0
        do iter2=1,iter
          do l=1,N_st
            u_in(i,k) += U(i,l,iter2)*y(l,iter2,k,1)
          enddo
        enddo
      enddo
    enddo

  enddo

  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write(iunit,'(A)') ''
  call write_time(iunit)

  deallocate (                                                       &
      kl_pairs,                                                      &
      W,                                                             &
      U,                                                             &
      R,                                                             &
      h,                                                             &
      y,                                                             &
      lambda                                                         &
      )
  abort_here = abort_all
end



subroutine H_u_0_mrcc_myold(v_0,u_0,H_jj,n,keys_tmp,shortcut,sort_idx,Nint,istate)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0>
  !   
  ! n : number of determinants
  !     
  ! H_jj : array of <j|H|j>
  END_DOC
  integer, intent(in)            :: n,Nint,istate
  double precision, intent(out)  :: v_0(n)
  double precision, intent(in)   :: u_0(n)
  double precision, intent(in)   :: H_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  integer, allocatable           :: idx(:)
  double precision               :: hij
  double precision, allocatable  :: vt(:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  
  integer,intent(in)             :: shortcut(0:n+1), sort_idx(n)
  integer                        :: tmp, warp(2,0:n+1), sh, ni
!   
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy delta_ij
  integer, parameter             :: block_size = 157
  
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,j,k,idx,jj,vt,ii,warp,tmp,sh)                             &
      !$OMP SHARED(n_det_ref,n_det_non_ref,idx_ref,idx_non_ref,n,H_jj,u_0,keys_tmp,Nint,v_0,istate,delta_ij,shortcut,sort_idx)
      
  !$OMP DO SCHEDULE(static)
  do i=1,n  
    v_0(i) = H_jj(i) * u_0(i)
  enddo 
  !$OMP END DO

  allocate(idx(0:n), vt(n))
  Vt = 0.d0
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0)
    warp(1,0) = 0
    do ii=1,sh!shortcut(0)
      tmp = 0
      do ni=1,Nint
        tmp = popcnt(xor(keys_tmp(ni,1, shortcut(ii)), keys_tmp(ni,1,shortcut(sh))))
      end do
      if(tmp <= 4) then
        tmp = warp(1,0) + 1
        warp(1,0) = tmp
        warp(1,tmp) = shortcut(ii)
        warp(2,tmp) = shortcut(ii+1)-1
      end if
    end do
    
    do ii=shortcut(sh),shortcut(sh+1)-1
      idx(0) = ii
      !call filter_connected_davidson_mwen(keys_tmp,shortcut,keys_tmp(1,1,ii),Nint,ii-1,idx)
      call filter_connected_davidson_warp(keys_tmp,warp,keys_tmp(1,1,ii),Nint,ii-1,idx)
      i = sort_idx(ii)
      
      do jj=1,idx(0)
        j = sort_idx(idx(jj))
        !j = idx(jj)
        if ( (dabs(u_0(j)) > 1.d-7).or.((dabs(u_0(i)) > 1.d-7)) ) then
          call i_H_j(keys_tmp(1,1,idx(jj)),keys_tmp(1,1,ii),Nint,hij)
          vt (i) = vt (i) + hij*u_0(j)
          vt (j) = vt (j) + hij*u_0(i)
        endif
      enddo
    enddo
  enddo
  !$OMP END DO
  
  
  
  !$OMP DO SCHEDULE(guided)
  do ii=1,n_det_ref
    i = idx_ref(ii)
    do jj = 1, n_det_non_ref
        j = idx_non_ref(jj)
        vt (i) = vt (i) + delta_ij(ii,jj,istate)*u_0(j)
        vt (j) = vt (j) + delta_ij(ii,jj,istate)*u_0(i)
    enddo
  enddo
  !$OMP END DO
  !$OMP CRITICAL
  do i=1,n
    v_0(i) = v_0(i) + vt(i)
  enddo
  !$OMP END CRITICAL
  deallocate(idx,vt)
  !$OMP END PARALLEL
end


subroutine H_u_0_mrcc(v_0,u_0,H_jj,n,keys_tmp,Nint,istate)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0>
  !   
  ! n : number of determinants
  !     
  ! H_jj : array of <j|H|j>
  END_DOC
  integer, intent(in)            :: n,Nint,istate
  double precision, intent(out)  :: v_0(n)
  double precision, intent(in)   :: u_0(n)
  double precision, intent(in)   :: H_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  integer, allocatable           :: idx(:)
  double precision               :: hij
  double precision, allocatable  :: vt(:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  
  integer                        :: shortcut(0:n+1), sort_idx(n)
  integer(bit_kind)              :: sorted(Nint,n), version(Nint,n)
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, pass
!   
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy delta_ij
  integer, parameter             :: block_size = 157
  
 
   !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,j,k,idx,jj,vt,ii,sh, sh2, ni, exa, ext, org_i, org_j, endi, pass)                             &
      !$OMP SHARED(n_det_ref,n_det_non_ref,idx_ref,idx_non_ref,n,H_jj,u_0,keys_tmp,Nint,v_0,istate,delta_ij,sorted,shortcut,sort_idx,version)
      
      
      
 !$OMP DO SCHEDULE(static)
  do i=1,n  
    v_0(i) = H_jj(i) * u_0(i)
  enddo 
  !$OMP END DO

  allocate(idx(0:n), vt(n))
  Vt = 0.d0
  
  do pass=1,2
    if(pass == 1) then
      call sort_dets_ab_v(keys_tmp, sorted, sort_idx, shortcut, version, n, Nint)
    else
      call sort_dets_ba_v(keys_tmp, sorted, sort_idx, shortcut, version, n, Nint)
    end if
    

    !$OMP DO SCHEDULE(dynamic)
    do sh=1,shortcut(0)
    
    if(pass == 2) then
      endi = sh
    else
      endi = 1
    end if
    
    do sh2=endi,sh
      exa = 0
      do ni=1,Nint
        exa += popcnt(xor(version(ni,sh), version(ni,sh2)))
      end do
      if(exa > 2) then
        cycle
      end if
      
      do i=shortcut(sh),shortcut(sh+1)-1
        if(sh==sh2) then
          endi = i-1
        else
          endi = shortcut(sh2+1)-1
        end if
        
        do j=shortcut(sh2),endi
          ext = exa
          do ni=1,Nint
            ext += popcnt(xor(sorted(ni,i), sorted(ni,j)))
          end do
          if(ext <= 4) then
            org_i = sort_idx(i)
            org_j = sort_idx(j)
            if ( (dabs(u_0(org_j)) > 1.d-7).or.((dabs(u_0(org_i)) > 1.d-7)) ) then
              call i_H_j(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),Nint,hij)
              vt (org_i) = vt (org_i) + hij*u_0(org_j)
              vt (org_j) = vt (org_j) + hij*u_0(org_i)
            endif
          end if
        end do
      end do
    end do
    enddo
   !$OMP END DO
  end do
 
  
  
  
  !$OMP DO SCHEDULE(guided)
  do ii=1,n_det_ref
    i = idx_ref(ii)
    do jj = 1, n_det_non_ref
        j = idx_non_ref(jj)
        vt (i) = vt (i) + delta_ij(ii,jj,istate)*u_0(j)
        vt (j) = vt (j) + delta_ij(ii,jj,istate)*u_0(i)
    enddo
  enddo
  !$OMP END DO
  !$OMP CRITICAL
  do i=1,n
    v_0(i) = v_0(i) + vt(i)
  enddo
  !$OMP END CRITICAL
  deallocate(idx,vt)
  !$OMP END PARALLEL
end


! 
! subroutine H_u_0_mrcc_org(v_0,u_0,H_jj,n,keys_tmp,Nint,istate)
!   use bitmasks
!   implicit none
!   BEGIN_DOC
!   ! Computes v_0 = H|u_0>
!   !   
!   ! n : number of determinants
!   !     
!   ! H_jj : array of <j|H|j>
!   END_DOC
!   integer, intent(in)            :: n,Nint,istate
!   double precision, intent(out)  :: v_0(n)
!   double precision, intent(in)   :: u_0(n)
!   double precision, intent(in)   :: H_jj(n)
!   integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
!   integer, allocatable           :: idx(:)
!   double precision               :: hij
!   double precision, allocatable  :: vt(:)
!   integer                        :: i,j,k,l, jj,ii
!   integer                        :: i0, j0
!   
! 
!   
! 
!   
!   ASSERT (Nint > 0)
!   ASSERT (Nint == N_int)
!   ASSERT (n>0)
!   PROVIDE ref_bitmask_energy delta_ij 
!   integer, parameter             :: block_size = 157
!   !$OMP PARALLEL DEFAULT(NONE)                                       &
!       !$OMP PRIVATE(i,hij,j,k,idx,jj,ii,vt)                             &
!       !$OMP SHARED(n_det_ref,n_det_non_ref,idx_ref,idx_non_ref,n,H_jj,u_0,keys_tmp,Nint,v_0,istate,delta_ij)
!   !$OMP DO SCHEDULE(static)
!   do i=1,n  
!     v_0(i) = H_jj(i) * u_0(i)
!   enddo 
!   !$OMP END DO
!   allocate(idx(0:n), vt(n))
!   Vt = 0.d0
!   !$OMP DO SCHEDULE(guided)
!   do i=1,n
!     idx(0) = i
!     call filter_connected_davidson(keys_tmp,keys_tmp(1,1,i),Nint,i-1,idx)
!     do jj=1,idx(0)
!       j = idx(jj)
! !     if ( (dabs(u_0(j)) > 1.d-7).or.((dabs(u_0(i)) > 1.d-7)) ) then
!         call i_H_j(keys_tmp(1,1,j),keys_tmp(1,1,i),Nint,hij)
!         hij = hij 
!         vt (i) = vt (i) + hij*u_0(j)
!         vt (j) = vt (j) + hij*u_0(i)
! !     endif
!     enddo
!   enddo
!   !$OMP END DO
! 
!   !$OMP DO SCHEDULE(guided)
!   do ii=1,n_det_ref
!     i = idx_ref(ii)
!     do jj = 1, n_det_non_ref
!         j = idx_non_ref(jj)
!         vt (i) = vt (i) + delta_ij(ii,jj,istate)*u_0(j)
!         vt (j) = vt (j) + delta_ij(ii,jj,istate)*u_0(i)
!     enddo
!   enddo
!   !$OMP END DO
!   !$OMP CRITICAL
!   do i=1,n
!     v_0(i) = v_0(i) + vt(i)
!   enddo
!   !$OMP END CRITICAL
!   deallocate(idx,vt)
!   !$OMP END PARALLEL
! end

