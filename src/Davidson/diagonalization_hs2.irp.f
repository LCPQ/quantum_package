subroutine davidson_diag_hs2(dets_in,u_in,dim_in,energies,sze,N_st,N_st_diag,Nint,iunit)
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
  double precision, allocatable  :: H_jj(:), S2_jj(:)
  
  double precision               :: diag_h_mat_elem
  integer                        :: i
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  PROVIDE mo_bielec_integrals_in_map
  allocate(H_jj(sze), S2_jj(sze))
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,H_jj,S2_jj, dets_in,Nint)                    &
      !$OMP  PRIVATE(i)
  !$OMP DO SCHEDULE(guided)
  do i=1,sze
    H_jj(i) = diag_h_mat_elem(dets_in(1,1,i),Nint)
    call get_s2(dets_in(1,1,i),dets_in(1,1,i),Nint,S2_jj(i))
  enddo
  !$OMP END DO 
  !$OMP END PARALLEL

  call davidson_diag_hjj_sjj(dets_in,u_in,H_jj,S2_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
  deallocate (H_jj,S2_jj)
end


subroutine davidson_diag_hjj_sjj(dets_in,u_in,H_jj,S2_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization with specific diagonal elements of the H matrix
  !
  ! H_jj : specific diagonal H matrix elements to diagonalize de Davidson
  !
  ! S2_jj : specific diagonal S^2 matrix elements 
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
  ! N_st_diag : Number of states in which H is diagonalized. Assumed > sze
  !
  ! iunit : Unit for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(sze), S2_jj(sze)
  integer,  intent(in)  :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  
  integer                        :: sze_8
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision, allocatable  :: overlap(:,:)
  double precision               :: u_dot_v, u_dot_u
  
  integer, allocatable           :: kl_pairs(:,:)
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, allocatable  :: W(:,:,:),  U(:,:,:), R(:,:), S(:,:,:)
  double precision, allocatable  :: y(:,:,:,:), h(:,:,:,:), lambda(:), s2(:)
  double precision, allocatable  :: c(:), H_small(:,:)
  double precision               :: diag_h_mat_elem
  double precision, allocatable  :: residual_norm(:)
  character*(16384)              :: write_buffer
  double precision               :: to_print(3,N_st)
  double precision               :: cpu, wall
  include 'constants.include.F'
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: U, W, R, S, y, h, lambda
  if (N_st_diag > sze) then
     stop 'error in Davidson : N_st_diag > sze'
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
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = ' Iter'
  do i=1,N_st
    write_buffer = trim(write_buffer)//'      Energy          S^2      Residual'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)

  integer, external :: align_double
  sze_8 = align_double(sze)

  double precision :: delta

  if (s2_eig) then
    delta = 1.d0
  else
    delta = 0.d0
  endif

  allocate(                                                          &
      kl_pairs(2,N_st_diag*(N_st_diag+1)/2),                         &
      W(sze_8,N_st_diag,davidson_sze_max),                           &
      U(sze_8,N_st_diag,davidson_sze_max),                           &
      R(sze_8,N_st_diag),                                            &
      S(sze_8,N_st_diag,davidson_sze_max),                           &
      h(N_st_diag,davidson_sze_max,N_st_diag,davidson_sze_max),      &
      y(N_st_diag,davidson_sze_max,N_st_diag,davidson_sze_max),      &
      residual_norm(N_st_diag),                                      &
      overlap(N_st_diag,N_st_diag),                                  &
      c(N_st_diag*davidson_sze_max),                                 &
      H_small(N_st_diag,N_st_diag),                                  &
      s2(N_st_diag),                                                 &
      lambda(N_st_diag*davidson_sze_max))
  
  ASSERT (N_st > 0)
  ASSERT (N_st_diag >= N_st)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Davidson iterations
  ! ===================
  
  converged = .False.
  
  do k=N_st+1,N_st_diag
      do i=1,sze
        double precision               :: r1, r2
        call random_number(r1)
        call random_number(r2)
        u_in(i,k) = dsqrt(-2.d0*dlog(r1))*dcos(dtwo_pi*r2)
      enddo
    
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
      
      call H_S2_u_0_nstates(W(1,1,iter),S(1,1,iter),U(1,1,iter),H_jj,S2_jj,sze,dets_in,Nint,N_st_diag,sze_8)
      
      
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
          S(i,k,iter+1) = 0.d0
        enddo
      enddo

!      do k=1,N_st_diag
!         do iter2=1,iter
!          do l=1,N_st_diag
!            do i=1,sze
!              U(i,k,iter+1) = U(i,k,iter+1) + U(i,l,iter2)*y(l,iter2,k,1)
!              W(i,k,iter+1) = W(i,k,iter+1) + W(i,l,iter2)*y(l,iter2,k,1)
!              S(i,k,iter+1) = W(i,k,iter+1) + S(i,l,iter2)*y(l,iter2,k,1)
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
      call dgemm('N','N',sze,N_st_diag,1,                            &
          1.d0, S, size(S,1), y, size(y,1)*size(y,2), 0.d0, S(1,1,iter+1), size(S,1))

      ! Compute residual vector
      ! -----------------------
      
      do k=1,N_st_diag
        s2(k) = u_dot_v(U(1,k,iter+1), S(1,k,iter+1), sze) + S_z2_Sz
      enddo

      do k=1,N_st_diag
        do i=1,sze
          R(i,k) = (lambda(k) * U(i,k,iter+1) - W(i,k,iter+1) ) &
            * (1.d0 + s2(k) * U(i,k,iter+1) - S(i,k,iter+1) - S_z2_Sz)
        enddo
        if (k <= N_st) then
          residual_norm(k) = u_dot_u(R(1,k),sze)
          to_print(1,k) = lambda(k) + nuclear_repulsion
          to_print(2,k) = s2(k)
          to_print(3,k) = residual_norm(k)
          if (residual_norm(k) > 1.e9) then
            stop 'Davidson failed'
          endif
        endif
      enddo
      
      write(iunit,'(X,I3,X,100(X,F16.10,X,F11.6,X,E11.3))')  iter, to_print(:,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      if (converged) then
        exit
      endif
      
      ! Davidson step
      ! -------------
      
      do k=1,N_st_diag
        do i=1,sze
          U(i,k,iter+1) =  - R(i,k)/max(H_jj(i) - lambda(k),1.d-2) 
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
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
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

