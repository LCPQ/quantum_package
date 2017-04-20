subroutine davidson_diag(dets_in,u_in,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
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
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint, iunit
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
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
      !$OMP  SHARED(sze,H_jj,dets_in,Nint)                           &
      !$OMP  PRIVATE(i)
  !$OMP DO SCHEDULE(guided)
  do i=1,sze
    H_jj(i) = diag_h_mat_elem(dets_in(1,1,i),Nint)
  enddo
  !$OMP END DO 
  !$OMP END PARALLEL

  call davidson_diag_hjj(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
  deallocate (H_jj)
end


logical function det_inf(key1, key2, Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
! Ordering function for determinants
  END_DOC
  integer,intent(in)                 :: Nint
  integer(bit_kind),intent(in)       :: key1(Nint, 2), key2(Nint, 2)
  integer                            :: i,j
  
  det_inf = .false.
  
  do i=1,2
    do j=Nint,1,-1
      if(key1(j,i) < key2(j,i)) then
        det_inf = .true.
        return
      else if(key1(j,i) > key2(j,i)) then
        return
      end if
    end do
  end do
end function


subroutine tamiser(key, idx, no, n, Nint, N_key)
  use bitmasks
  implicit none
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer,intent(in)                    :: no, n, Nint, N_key
  integer(bit_kind),intent(inout)       :: key(Nint, 2, N_key)
  integer,intent(inout)                 :: idx(N_key)
  integer                               :: k,j,tmpidx
  integer(bit_kind)                     :: tmp(Nint, 2)
  logical                               :: det_inf
  integer                               :: ni
  
  k = no
  j = 2*k
  do while(j <= n)
    if(j < n) then
      if (det_inf(key(1,1,j), key(1,1,j+1), Nint)) then
        j = j+1
      endif
    endif
    if(det_inf(key(1,1,k), key(1,1,j), Nint)) then
      do ni=1,Nint
        tmp(ni,1)   = key(ni,1,k)
        tmp(ni,2)   = key(ni,2,k)
        key(ni,1,k) = key(ni,1,j)
        key(ni,2,k) = key(ni,2,j)
        key(ni,1,j) = tmp(ni,1)
        key(ni,2,j) = tmp(ni,2)
      enddo
      tmpidx = idx(k)
      idx(k) = idx(j)
      idx(j) = tmpidx
      k = j
      j = k+k
    else
      return
    endif
  enddo
end subroutine


subroutine sort_dets_ba_v(key_in, key_out, idx, shortcut, version, N_key, Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer, intent(in)            :: Nint, N_key
  integer(bit_kind),intent(in)   :: key_in(Nint,2,N_key)
  integer(bit_kind),intent(out)  :: key_out(Nint,N_key)
  integer,intent(out)            :: idx(N_key)
  integer,intent(out)            :: shortcut(0:N_key+1)
  integer(bit_kind),intent(out)  :: version(Nint,N_key+1)
  integer(bit_kind), allocatable :: key(:,:,:)
  integer                        :: i,ni
  
  allocate ( key(Nint,2,N_key) )
  do i=1,N_key
    do ni=1,Nint
      key(ni,1,i) = key_in(ni,2,i)
      key(ni,2,i) = key_in(ni,1,i)
    enddo
  enddo
  
  call sort_dets_ab_v(key, key_out, idx, shortcut, version, N_key, Nint)
  deallocate ( key )
end subroutine



subroutine sort_dets_ab_v(key_in, key_out, idx, shortcut, version, N_key, Nint)
  use bitmasks
  implicit none
  
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer, intent(in)                   :: Nint, N_key
  integer(bit_kind),intent(in)          :: key_in(Nint,2,N_key)
  integer(bit_kind),intent(out)         :: key_out(Nint,N_key)
  integer,intent(out)                   :: idx(N_key)
  integer,intent(out)                   :: shortcut(0:N_key+1)
  integer(bit_kind),intent(out)         :: version(Nint,N_key+1)
  integer(bit_kind), allocatable        :: key(:,:,:)
  integer(bit_kind)                     :: tmp(Nint, 2)
  integer                               :: tmpidx,i,ni
  
  allocate (key(Nint,2,N_key))
  do i=1,N_key
    do ni=1,Nint
      key(ni,1,i) = key_in(ni,1,i)
      key(ni,2,i) = key_in(ni,2,i)
    enddo
    idx(i) = i
  end do
  
  do i=N_key/2,1,-1
    call tamiser(key, idx, i, N_key, Nint, N_key)
  end do
  
  do i=N_key,2,-1
    do ni=1,Nint
      tmp(ni,1) = key(ni,1,i)
      tmp(ni,2) = key(ni,2,i)
      key(ni,1,i) = key(ni,1,1)
      key(ni,2,i) = key(ni,2,1)
      key(ni,1,1) = tmp(ni,1)
      key(ni,2,1) = tmp(ni,2)
    enddo
    tmpidx = idx(i)
    idx(i) = idx(1)
    idx(1) = tmpidx
    call tamiser(key, idx, 1, i-1, Nint, N_key)
  end do
  
  shortcut(0) = 1
  shortcut(1) = 1
  do ni=1,Nint
    version(ni,1) = key(ni,1,1)
  enddo
  do i=2,N_key
    do ni=1,nint
      if(key(ni,1,i) /= key(ni,1,i-1)) then
        shortcut(0) = shortcut(0) + 1
        shortcut(shortcut(0)) = i
        version(:,shortcut(0)) = key(:,1,i)
        exit
      end if
    end do
  end do
  shortcut(shortcut(0)+1) = N_key+1
  do i=1,N_key
    do ni=1,Nint
      key_out(ni,i) = key(ni,2,i)
    enddo
  enddo
  deallocate (key)
end subroutine


subroutine sort_dets_ab(key, idx, shortcut, N_key, Nint)
  use bitmasks
  implicit none
  
  
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer, intent(in)                   :: Nint, N_key
  integer(bit_kind),intent(inout)       :: key(Nint,2,N_key)
  integer,intent(inout)                   :: idx(N_key)
  integer,intent(inout)                   :: shortcut(0:N_key+1)
  integer(bit_kind)                     :: tmp(Nint, 2)
  integer                               :: tmpidx,i,ni
  
  do i=1,N_key
    idx(i) = i
  end do
  
  do i=N_key/2,1,-1
    call tamiser(key, idx, i, N_key, Nint, N_key)
  end do
  
  do i=N_key,2,-1
    do ni=1,Nint
      tmp(ni,1) = key(ni,1,i)
      tmp(ni,2) = key(ni,2,i)
      key(ni,1,i) = key(ni,1,1)
      key(ni,2,i) = key(ni,2,1)
      key(ni,1,1) = tmp(ni,1)
      key(ni,2,1) = tmp(ni,2)
    enddo

    tmpidx = idx(i)
    idx(i) = idx(1)
    idx(1) = tmpidx
    call tamiser(key, idx, 1, i-1, Nint, N_key)
  end do
  
  shortcut(0) = 1
  shortcut(1) = 1
  do i=2,N_key
    do ni=1,nint
      if(key(ni,1,i) /= key(ni,1,i-1)) then
        shortcut(0) = shortcut(0) + 1
        shortcut(shortcut(0)) = i
        exit
      end if
    end do
  end do
  shortcut(shortcut(0)+1) = N_key+1
end subroutine


subroutine davidson_diag_hjj(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
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
  ! N_st_diag : Number of states in which H is diagonalized
  !
  ! iunit : Unit for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(sze)
  integer,  intent(in)  :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision, allocatable  :: overlap(:,:)
  double precision               :: u_dot_v, u_dot_u
  
  integer, allocatable           :: kl_pairs(:,:)
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, allocatable  :: W(:,:,:),  U(:,:,:), R(:,:)
  double precision, allocatable  :: y(:,:,:,:), h(:,:,:,:), lambda(:)
  double precision, allocatable  :: c(:), H_small(:,:)
  double precision               :: diag_h_mat_elem
  double precision, allocatable  :: residual_norm(:)
  character*(16384)              :: write_buffer
  double precision               :: to_print(2,N_st)
  double precision               :: cpu, wall
  include 'constants.include.F'
  

  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: U, W, R, y, h, lambda

  if(store_full_H_mat) then
   stop  'TODO : put S^2 in stor_full_H_mat'
  endif

  if(store_full_H_mat.and.sze.le.n_det_max_stored)then
   provide H_matrix_all_dets
  endif

  PROVIDE nuclear_repulsion

  call write_time(iunit)
  call wall_time(wall)
  call cpu_time(cpu)
  write(iunit,'(A)') ''
  write(iunit,'(A)') 'Davidson Diagonalization'
  write(iunit,'(A)') '------------------------'
  write(iunit,'(A)') ''
  call write_int(iunit,N_st,'Number of states')
  call write_int(iunit,N_st_diag,'Number of states in diagonalization')
  call write_int(iunit,sze,'Number of determinants')
  write(iunit,'(A)') ''
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = ' Iter'
  do i=1,N_st
    write_buffer = trim(write_buffer)//'            Energy           Residual'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)

  integer, external :: align_double

  allocate(                                                          &
      kl_pairs(2,N_st_diag*(N_st_diag+1)/2),                         &
      W(sze,N_st_diag,davidson_sze_max),                           &
      U(sze,N_st_diag,davidson_sze_max),                           &
      R(sze,N_st_diag),                                            &
      h(N_st_diag,davidson_sze_max,N_st_diag,davidson_sze_max),      &
      y(N_st_diag,davidson_sze_max,N_st_diag,davidson_sze_max),      &
      residual_norm(N_st_diag),                                      &
      overlap(N_st_diag,N_st_diag),                                  &
      c(N_st_diag*davidson_sze_max),                                 &
      H_small(N_st_diag,N_st_diag),                                  &
      lambda(N_st_diag*davidson_sze_max))
  
  ASSERT (N_st > 0)
  ASSERT (N_st_diag >= N_st)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Davidson iterations
  ! ===================
  
  converged = .False.
  
  do k=1,N_st_diag

    if (k > N_st) then
      do i=1,sze
        double precision               :: r1, r2
        call random_number(r1)
        call random_number(r2)
        u_in(i,k) = dsqrt(-2.d0*dlog(r1))*dcos(dtwo_pi*r2)
      enddo
    endif
    
    ! Gram-Schmidt
    ! ------------
    call dgemv('T',sze,k-1,1.d0,u_in,size(u_in,1),                   &
        u_in(1,k),1,0.d0,c,1)
    call dgemv('N',sze,k-1,-1.d0,u_in,size(u_in,1),                  &
        c,1,1.d0,u_in(1,k),1)
    call normalize(u_in(1,k),sze)
  enddo


  
  do while (.not.converged)
    
    do k=1,N_st_diag
      do i=1,sze
        U(i,k,1) = u_in(i,k)
      enddo
    enddo

    do iter=1,davidson_sze_max-1
      
      ! Compute |W_k> = \sum_i |i><i|H|u_k>
      ! -----------------------------------------
      
      call H_u_0_nstates(W(1,1,iter),U(1,1,iter),H_jj,sze,dets_in,Nint,N_st_diag,sze)
!      do k=1,N_st
!          if(store_full_H_mat.and.sze.le.n_det_max_stored)then
!           call H_u_0_stored(W(1,k,iter),U(1,k,iter),H_matrix_all_dets,sze)
!          else
!           call H_u_0(W(1,k,iter),U(1,k,iter),H_jj,sze,dets_in,Nint)
!          endif
!      enddo
      
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------


!      do l=1,N_st_diag
!        do k=1,N_st_diag
!          do iter2=1,iter-1
!            h(k,iter2,l,iter) = u_dot_v(U(1,k,iter2),W(1,l,iter),sze)
!            h(k,iter,l,iter2) = h(k,iter2,l,iter)
!          enddo
!        enddo
!        do k=1,l
!          h(k,iter,l,iter) = u_dot_v(U(1,k,iter),W(1,l,iter),sze)
!          h(l,iter,k,iter) = h(k,iter,l,iter)
!        enddo
!      enddo

      call dgemm('T','N', N_st_diag*iter, N_st_diag, sze,            &
          1.d0, U, size(U,1), W(1,1,iter), size(W,1),                &
          0.d0, h(1,1,1,iter), size(h,1)*size(h,2))

      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,N_st_diag*davidson_sze_max,N_st_diag*iter)
      
      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      do k=1,N_st_diag
        do i=1,sze
          U(i,k,iter+1) = 0.d0
          W(i,k,iter+1) = 0.d0
        enddo
      enddo
!      do k=1,N_st_diag
!         do iter2=1,iter
!          do l=1,N_st_diag
!            do i=1,sze
!              U(i,k,iter+1) = U(i,k,iter+1) + U(i,l,iter2)*y(l,iter2,k,1)
!              W(i,k,iter+1) = W(i,k,iter+1) + W(i,l,iter2)*y(l,iter2,k,1)
!            enddo
!          enddo
!        enddo
!      enddo
!
!
      call dgemm('N','N', sze, N_st_diag, N_st_diag*iter,            &
          1.d0, U, size(U,1), y, size(y,1)*size(y,2), 0.d0, U(1,1,iter+1), size(U,1))
      call dgemm('N','N',sze,N_st_diag,N_st_diag*iter,               &
          1.d0, W, size(W,1), y, size(y,1)*size(y,2), 0.d0, W(1,1,iter+1), size(W,1))


      ! Compute residual vector
      ! -----------------------
      
      do k=1,N_st_diag
        do i=1,sze
          R(i,k) = lambda(k) * U(i,k,iter+1) - W(i,k,iter+1)
        enddo
        if (k <= N_st) then
          residual_norm(k) = u_dot_u(R(1,k),sze)
          to_print(1,k) = lambda(k) + nuclear_repulsion
          to_print(2,k) = residual_norm(k)
        endif
      enddo
      
      write(iunit,'(1X,I3,1X,100(1X,F16.10,1X,E16.6))')  iter, to_print(:,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      if (converged) then
        exit
      endif
      
      ! Davidson step
      ! -------------
      
      do k=1,N_st_diag
        do i=1,sze
          U(i,k,iter+1) = -1.d0/max(H_jj(i) - lambda(k),1.d-2) * R(i,k)
        enddo
      enddo
      
      ! Gram-Schmidt
      ! ------------
      
      do k=1,N_st_diag

!        do iter2=1,iter
!          do l=1,N_st_diag
!            c(1) = u_dot_v(U(1,k,iter+1),U(1,l,iter2),sze)
!            do i=1,sze
!              U(i,k,iter+1) = U(i,k,iter+1) - c(1) * U(i,l,iter2)
!            enddo
!          enddo
!        enddo
!
        call dgemv('T',sze,N_st_diag*iter,1.d0,U,size(U,1),  &
              U(1,k,iter+1),1,0.d0,c,1)
        call dgemv('N',sze,N_st_diag*iter,-1.d0,U,size(U,1), &
              c,1,1.d0,U(1,k,iter+1),1)
!
!        do l=1,k-1
!          c(1) = u_dot_v(U(1,k,iter+1),U(1,l,iter+1),sze)
!          do i=1,sze
!            U(i,k,iter+1) = U(i,k,iter+1) - c(1) * U(i,l,iter+1)
!          enddo
!        enddo
!
        call dgemv('T',sze,k-1,1.d0,U(1,1,iter+1),size(U,1),   &
            U(1,k,iter+1),1,0.d0,c,1)
        call dgemv('N',sze,k-1,-1.d0,U(1,1,iter+1),size(U,1),        &
            c,1,1.d0,U(1,k,iter+1),1)

        call normalize( U(1,k,iter+1), sze )
      enddo

    enddo

    if (.not.converged) then
      iter = davidson_sze_max-1
    endif
    
    ! Re-contract to u_in
    ! -----------
    
    do k=1,N_st_diag
      energies(k) = lambda(k)
      do i=1,sze
        u_in(i,k) = 0.d0
      enddo
    enddo
!    do k=1,N_st_diag
!      do i=1,sze
!        do iter2=1,iter
!          do l=1,N_st_diag
!            u_in(i,k) += U(i,l,iter2)*y(l,iter2,k,1)
!          enddo
!        enddo
!      enddo
!    enddo

    call dgemm('N','N', sze, N_st_diag, N_st_diag*iter, 1.d0,      &
        U, size(U,1), y, N_st_diag*davidson_sze_max, &
        0.d0, u_in, size(u_in,1))

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
      W, residual_norm,                                              &
      U, overlap,                                                    &
      R, c,                                                          &
      h,                                                             &
      y,                                                             &
      lambda                                                         &
      )
end

