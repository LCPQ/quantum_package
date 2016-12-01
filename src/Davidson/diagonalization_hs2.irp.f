subroutine davidson_diag_hs2(dets_in,u_in,s2_out,dim_in,energies,sze,N_st,N_st_diag,Nint,iunit)
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
  double precision, intent(out)  :: energies(N_st_diag), s2_out(N_st_diag)
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

  if (disk_based_davidson) then
    call davidson_diag_hjj_sjj_mmap(dets_in,u_in,H_jj,S2_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
  else
    call davidson_diag_hjj_sjj(dets_in,u_in,H_jj,S2_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
  endif
  do i=1,N_st_diag
    s2_out(i) = S2_jj(i)
  enddo
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
  double precision,  intent(in)  :: H_jj(sze)
  double precision,  intent(inout) :: S2_jj(sze)
  integer,  intent(in)           :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  
  integer                        :: sze_8
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision               :: u_dot_v, u_dot_u
  
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, allocatable  :: W(:,:),  U(:,:), S(:,:), overlap(:,:)
  double precision, allocatable  :: y(:,:), h(:,:), lambda(:), s2(:)
  double precision, allocatable  :: c(:), s_(:,:), s_tmp(:,:)
  double precision               :: diag_h_mat_elem
  double precision, allocatable  :: residual_norm(:)
  character*(16384)              :: write_buffer
  double precision               :: to_print(3,N_st)
  double precision               :: cpu, wall
  integer                        :: shift, shift2, itermax
  double precision               :: r1, r2
  logical                        :: state_ok(N_st_diag*davidson_sze_max)
  include 'constants.include.F'
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: U, W, S, y, h, lambda
  if (N_st_diag*3 > sze) then
    print *,  'error in Davidson :'
    print *,  'Increase n_det_max_jacobi to ', N_st_diag*3
    stop -1
  endif
  
  PROVIDE nuclear_repulsion expected_s2
  
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
    write_buffer = trim(write_buffer)//'      Energy          S^2      Residual  '
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)
  
  integer, external              :: align_double
  sze_8 = align_double(sze)
  
  itermax = max(3,min(davidson_sze_max, sze/N_st_diag))
  allocate(                                                          &
      W(sze_8,N_st_diag*itermax),                                    &
      U(sze_8,N_st_diag*itermax),                                    &
      S(sze_8,N_st_diag*itermax),                                    &
      h(N_st_diag*itermax,N_st_diag*itermax),                        &
      y(N_st_diag*itermax,N_st_diag*itermax),                        &
      s_(N_st_diag*itermax,N_st_diag*itermax),                       &
      s_tmp(N_st_diag*itermax,N_st_diag*itermax),                    &
      residual_norm(N_st_diag),                                      &
      c(N_st_diag*itermax),                                          &
      s2(N_st_diag*itermax),                                         &
      overlap(N_st_diag*itermax, N_st_diag*itermax),                 &
      lambda(N_st_diag*itermax))
  
  h = 0.d0
  U = 0.d0
  W = 0.d0
  S = 0.d0
  y = 0.d0
  s_ = 0.d0
  s_tmp = 0.d0


  ASSERT (N_st > 0)
  ASSERT (N_st_diag >= N_st)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Davidson iterations
  ! ===================
  
  converged = .False.
  
  do k=N_st+1,N_st_diag
    u_in(k,k) = 10.d0
    do i=1,sze
      call random_number(r1)
      call random_number(r2)
      r1 = dsqrt(-2.d0*dlog(r1))
      r2 = dtwo_pi*r2
      u_in(i,k) = r1*dcos(r2)
    enddo
  enddo
  do k=1,N_st_diag
    call normalize(u_in(1,k),sze)
  enddo
  
  
  do while (.not.converged)
    
    do k=1,N_st_diag
      do i=1,sze
        U(i,k) = u_in(i,k)
      enddo
    enddo
    
    do iter=1,itermax-1
      
      shift  = N_st_diag*(iter-1)
      shift2 = N_st_diag*iter
      
      call ortho_qr(U,size(U,1),sze,shift2)

      ! Compute |W_k> = \sum_i |i><i|H|u_k>
      ! -----------------------------------------
      
       
!      call H_S2_u_0_nstates_zmq(W(1,shift+1),S(1,shift+1),U(1,shift+1),H_jj,S2_jj,sze,dets_in,Nint,N_st_diag,sze_8)
      call H_S2_u_0_nstates(W(1,shift+1),S(1,shift+1),U(1,shift+1),H_jj,S2_jj,sze,dets_in,Nint,N_st_diag,sze_8)
      
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------

      call dgemm('T','N', shift2, shift2, sze,                       &
          1.d0, U, size(U,1), W, size(W,1),                          &
          0.d0, h, size(h,1))
      
      call dgemm('T','N', shift2, shift2, sze,                       &
          1.d0, U, size(U,1), S, size(S,1),                          &
          0.d0, s_, size(s_,1))


!      ! Diagonalize S^2
!      ! ---------------
!
!      call lapack_diag(s2,y,s_,size(s_,1),shift2)
!
!
!      ! Rotate H in the basis of eigenfunctions of s2
!      ! ---------------------------------------------
!
!      call dgemm('N','N',shift2,shift2,shift2,                       &
!          1.d0, h, size(h,1), y, size(y,1),                          &
!          0.d0, s_tmp, size(s_tmp,1))
!      
!      call dgemm('T','N',shift2,shift2,shift2,                       &
!          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
!          0.d0, h, size(h,1))
!
!      ! Damp interaction between different spin states
!      ! ------------------------------------------------
!
!      do k=1,shift2
!        do l=1,shift2
!          if (dabs(s2(k) - s2(l)) > 1.d0) then
!            h(k,l) = h(k,l)*(max(0.d0,1.d0 - dabs(s2(k) - s2(l))))
!          endif
!        enddo
!      enddo
!
!      ! Rotate back H 
!      ! -------------
!
!      call dgemm('N','T',shift2,shift2,shift2,                       &
!          1.d0, h, size(h,1), y, size(y,1),                          &
!          0.d0, s_tmp, size(s_tmp,1))
!      
!      call dgemm('N','N',shift2,shift2,shift2,                       &
!          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
!          0.d0, h, size(h,1))

      
      ! Diagonalize h
      ! -------------

      call lapack_diag(lambda,y,h,size(h,1),shift2)
      
      ! Compute S2 for each eigenvector
      ! -------------------------------

      call dgemm('N','N',shift2,shift2,shift2,                       &
          1.d0, s_, size(s_,1), y, size(y,1),                        &
          0.d0, s_tmp, size(s_tmp,1))
      
      call dgemm('T','N',shift2,shift2,shift2,                       &
          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
          0.d0, s_, size(s_,1))


      
      do k=1,shift2
        s2(k) = s_(k,k) + S_z2_Sz
      enddo

      if (s2_eig) then
          do k=1,shift2
            state_ok(k) = (dabs(s2(k)-expected_s2) < 0.6d0)
          enddo
      else
        do k=1,size(state_ok)
          state_ok(k) = .True.
        enddo
      endif

      do k=1,shift2
        if (.not. state_ok(k)) then
          do l=k+1,shift2
            if (state_ok(l)) then
              call dswap(shift2, y(1,k), 1, y(1,l), 1)
              call dswap(1, s2(k), 1, s2(l), 1)
              call dswap(1, lambda(k), 1, lambda(l), 1)
              state_ok(k) = .True.
              state_ok(l) = .False.
              exit
            endif
          enddo
        endif
      enddo

      if (state_following) then

        integer                        :: order(N_st_diag)
        double precision               :: cmax

        overlap = -1.d0
        do k=1,shift2
          do i=1,shift2
            overlap(k,i) = dabs(y(k,i))
          enddo
        enddo
        do k=1,N_st
          cmax = -1.d0
          do i=1,N_st
            if (overlap(i,k) > cmax) then
              cmax = overlap(i,k) 
              order(k) = i
            endif
          enddo
          do i=1,N_st_diag
            overlap(order(k),i) = -1.d0
          enddo
        enddo
        overlap = y
        do k=1,N_st
          l = order(k)
          if (k /= l) then
            y(1:shift2,k) = overlap(1:shift2,l)
          endif
        enddo
        do k=1,N_st
          overlap(k,1) = lambda(k)
          overlap(k,2) = s2(k)
        enddo
        do k=1,N_st
          l = order(k)
          if (k /= l) then
            lambda(k) = overlap(l,1)
            s2(k) = overlap(l,2)
          endif
        enddo
        
      endif


      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, U, size(U,1), y, size(y,1), 0.d0, U(1,shift2+1), size(U,1))
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, W, size(W,1), y, size(y,1), 0.d0, W(1,shift2+1), size(W,1))
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, S, size(S,1), y, size(y,1), 0.d0, S(1,shift2+1), size(S,1))

      ! Compute residual vector and davidson step
      ! -----------------------------------------
      
      do k=1,N_st_diag
!        if (state_ok(k)) then
          do i=1,sze
            U(i,shift2+k) =  &
              (lambda(k) * U(i,shift2+k) - W(i,shift2+k) )      &
                * (1.d0 + s2(k) * U(i,shift2+k) - S(i,shift2+k) - S_z2_Sz &
              )/max(H_jj(i) - lambda (k),1.d-2)
          enddo
!        else
!          ! Randomize components with bad <S2>
!            do i=1,sze-2,2
!              call random_number(r1)
!              call random_number(r2)
!              r1 = dsqrt(-2.d0*dlog(r1))
!              r2 = dtwo_pi*r2
!              U(i,shift2+k) = r1*dcos(r2)
!              U(i+1,shift2+k) = r1*dsin(r2)
!            enddo
!            do i=sze-2+1,sze
!              call random_number(r1)
!              call random_number(r2)
!              r1 = dsqrt(-2.d0*dlog(r1))
!              r2 = dtwo_pi*r2
!              U(i,shift2+k) = r1*dcos(r2)
!            enddo
!        endif

        if (k <= N_st) then
          residual_norm(k) = u_dot_u(U(1,shift2+k),sze)
          to_print(1,k) = lambda(k) + nuclear_repulsion
          to_print(2,k) = s2(k)
          to_print(3,k) = residual_norm(k)
        endif
      enddo
      
      write(iunit,'(X,I3,X,100(X,F16.10,X,F11.6,X,E11.3))')  iter, to_print(1:3,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      do k=1,N_st
        if (residual_norm(k) > 1.e8) then
        print *,  ''
          stop 'Davidson failed'
        endif
      enddo
      if (converged) then
        exit
      endif
      
    enddo

    ! Re-contract to u_in
    ! -----------
    
    call dgemm('N','N', sze, N_st_diag, shift2, 1.d0,      &
        U, size(U,1), y, size(y,1), 0.d0, u_in, size(u_in,1))

  enddo

  do k=1,N_st_diag
    energies(k) = lambda(k)
    S2_jj(k) = s2(k)
  enddo
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write(iunit,'(A)') ''
  call write_time(iunit)

  deallocate (                                                       &
      W, residual_norm,                                              &
      U, overlap,                                                    &
      c, S,                                                          &
      h,                                                             &
      y, s_, s_tmp,                                                  &
      lambda                                                         &
      )
end

subroutine davidson_diag_hjj_sjj_mmap(dets_in,u_in,H_jj,S2_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit)
  use bitmasks
  use mmap_module
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
  double precision,  intent(in)  :: H_jj(sze)
  double precision,  intent(inout) :: S2_jj(sze)
  integer,  intent(in)           :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  
  integer                        :: sze_8
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision               :: u_dot_v, u_dot_u
  
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, pointer      :: W(:,:),  U(:,:), S(:,:), overlap(:,:)
  double precision, allocatable  :: y(:,:), h(:,:), lambda(:), s2(:)
  double precision, allocatable  :: c(:), s_(:,:), s_tmp(:,:)
  double precision               :: diag_h_mat_elem
  double precision, allocatable  :: residual_norm(:)
  character*(16384)              :: write_buffer
  double precision               :: to_print(3,N_st)
  double precision               :: cpu, wall
  logical                        :: state_ok(N_st_diag*davidson_sze_max)
  integer                        :: shift, shift2, itermax
  include 'constants.include.F'
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: U, W, S, y, h, lambda
  if (N_st_diag*3 > sze) then
    print *,  'error in Davidson :'
    print *,  'Increase n_det_max_jacobi to ', N_st_diag*3
    stop -1
  endif
  
  PROVIDE nuclear_repulsion expected_s2
  
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
    write_buffer = trim(write_buffer)//'      Energy          S^2      Residual  '
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)
  
  integer, external              :: align_double
  integer                        :: fd(3)
  type(c_ptr)                    :: c_pointer(3)
  sze_8 = align_double(sze)
  
  itermax = min(davidson_sze_max, sze/N_st_diag)
  
  call mmap(                                                         &
      trim(ezfio_work_dir)//'U',                                     &
      (/ int(sze_8,8),int(N_st_diag*itermax,8) /),                   &
      8, fd(1), .False., c_pointer(1))
  call c_f_pointer(c_pointer(1), W, (/ sze_8,N_st_diag*itermax /) )
  
  call mmap(                                                         &
      trim(ezfio_work_dir)//'W',                                     &
      (/ int(sze_8,8),int(N_st_diag*itermax,8) /),                   &
      8, fd(2), .False., c_pointer(2))
  call c_f_pointer(c_pointer(2), U, (/ sze_8,N_st_diag*itermax /) )
  
  call mmap(                                                         &
      trim(ezfio_work_dir)//'S',                                     &
      (/ int(sze_8,8),int(N_st_diag*itermax,8) /),                   &
      8, fd(3), .False., c_pointer(3))
  call c_f_pointer(c_pointer(3), S, (/ sze_8,N_st_diag*itermax /) )
  
  allocate(                                                          &
      h(N_st_diag*itermax,N_st_diag*itermax),                        &
      y(N_st_diag*itermax,N_st_diag*itermax),                        &
      s_(N_st_diag*itermax,N_st_diag*itermax),                       &
      s_tmp(N_st_diag*itermax,N_st_diag*itermax),                    &
      overlap(N_st_diag*itermax, N_st_diag*itermax),                 &
      residual_norm(N_st_diag),                                      &
      c(N_st_diag*itermax),                                          &
      s2(N_st_diag*itermax),                                         &
      lambda(N_st_diag*itermax))
  
  h = 0.d0
  U = 0.d0
  W = 0.d0
  S = 0.d0
  y = 0.d0
  s_ = 0.d0
  s_tmp = 0.d0
  
  
  ASSERT (N_st > 0)
  ASSERT (N_st_diag >= N_st)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Davidson iterations
  ! ===================
  
  converged = .False.
  
  double precision               :: r1, r2
  do k=N_st+1,N_st_diag
      u_in(k,k) = 10.d0
      do i=1,sze
        call random_number(r1)
        r1 = dsqrt(-2.d0*dlog(r1))
        r2 = dtwo_pi*r2
        u_in(i,k) = r1*dcos(r2)
      enddo
  enddo
  do k=1,N_st_diag
    call normalize(u_in(1,k),sze)
  enddo


  do while (.not.converged)
    
    do k=1,N_st_diag
      do i=1,sze
        U(i,k) = u_in(i,k)
      enddo
    enddo

    do iter=1,itermax-1
      
      shift  = N_st_diag*(iter-1)
      shift2 = N_st_diag*iter

      call ortho_qr(U,size(U,1),sze,shift2)

      ! Compute |W_k> = \sum_i |i><i|H|u_k>
      ! -----------------------------------------
      
       
!      call H_S2_u_0_nstates_zmq(W(1,shift+1),S(1,shift+1),U(1,shift+1),H_jj,S2_jj,sze,dets_in,Nint,N_st_diag,sze_8)
      call H_S2_u_0_nstates(W(1,shift+1),S(1,shift+1),U(1,shift+1),H_jj,S2_jj,sze,dets_in,Nint,N_st_diag,sze_8)
      
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------

      do k=1,iter
        shift  = N_st_diag*(k-1)
        call dgemm('T','N', N_st_diag, shift2, sze,                       &
            1.d0, U(1,shift+1), size(U,1), W, size(W,1),                          &
            0.d0, h(shift+1,1), size(h,1))
        
        call dgemm('T','N', N_st_diag, shift2, sze,                       &
            1.d0, U(1,shift+1), size(U,1), S, size(S,1),                          &
            0.d0, s_(shift+1,1), size(s_,1))
      enddo

!      ! Diagonalize S^2
!      ! ---------------
!
!      call lapack_diag(s2,y,s_,size(s_,1),shift2)
!
!
!      ! Rotate H in the basis of eigenfunctions of s2
!      ! ---------------------------------------------
!
!      call dgemm('N','N',shift2,shift2,shift2,                       &
!          1.d0, h, size(h,1), y, size(y,1),                          &
!          0.d0, s_tmp, size(s_tmp,1))
!      
!      call dgemm('T','N',shift2,shift2,shift2,                       &
!          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
!          0.d0, h, size(h,1))
!
!      ! Damp interaction between different spin states
!      ! ------------------------------------------------
!
!      do k=1,shift2
!        do l=1,shift2
!          if (dabs(s2(k) - s2(l)) > 1.d0) then
!            h(k,l) = h(k,l)*(max(0.d0,1.d0 - dabs(s2(k) - s2(l))))
!          endif
!        enddo
!      enddo
!
!      ! Rotate back H 
!      ! -------------
!
!      call dgemm('N','T',shift2,shift2,shift2,                       &
!          1.d0, h, size(h,1), y, size(y,1),                          &
!          0.d0, s_tmp, size(s_tmp,1))
!      
!      call dgemm('N','N',shift2,shift2,shift2,                       &
!          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
!          0.d0, h, size(h,1))


      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,size(h,1),shift2)
      
      ! Compute S2 for each eigenvector
      ! -------------------------------

      call dgemm('N','N',shift2,shift2,shift2,                       &
          1.d0, s_, size(s_,1), y, size(y,1),                        &
          0.d0, s_tmp, size(s_tmp,1))
      
      call dgemm('T','N',shift2,shift2,shift2,                       &
          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
          0.d0, s_, size(s_,1))


      
      do k=1,shift2
        s2(k) = s_(k,k) + S_z2_Sz
      enddo


      if (s2_eig) then
          do k=1,shift2
            state_ok(k) = (dabs(s2(k)-expected_s2) < 0.6d0)
          enddo
      else
        state_ok(k) = .True.
      endif

      do k=1,shift2
        if (.not. state_ok(k)) then
          do l=k+1,shift2
            if (state_ok(l)) then
              call dswap(shift2, y(1,k), 1, y(1,l), 1)
              call dswap(1, s2(k), 1, s2(l), 1)
              call dswap(1, lambda(k), 1, lambda(l), 1)
              state_ok(k) = .True.
              state_ok(l) = .False.
              exit
            endif
          enddo
        endif
      enddo

      if (state_following) then

        ! Compute overlap with U_in
        ! -------------------------
        
        integer                        :: order(N_st_diag)
        double precision               :: cmax
        overlap = -1.d0
        do k=1,shift2
          do i=1,shift2
            overlap(k,i) = dabs(y(k,i))
          enddo
        enddo
        do k=1,N_st
          cmax = -1.d0
          do i=1,shift2
            if (overlap(i,k) > cmax) then
              cmax = overlap(i,k) 
              order(k) = i
            endif
          enddo
          do i=1,shift2
            overlap(order(k),i) = -1.d0
          enddo
        enddo
        overlap = y
        do k=1,N_st
          l = order(k)
          if (k /= l) then
            y(1:shift2,k) = overlap(1:shift2,l)
          endif
        enddo
        do k=1,N_st
          overlap(k,1) = lambda(k)
          overlap(k,2) = s2(k)
        enddo
        do k=1,N_st
          l = order(k)
          if (k /= l) then
            lambda(k) = overlap(l,1)
            s2(k) = overlap(l,2)
          endif
        enddo
        
      endif


      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, U, size(U,1), y, size(y,1), 0.d0, U(1,shift2+1), size(U,1))
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, W, size(W,1), y, size(y,1), 0.d0, W(1,shift2+1), size(W,1))
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, S, size(S,1), y, size(y,1), 0.d0, S(1,shift2+1), size(S,1))

      ! Compute residual vector and davidson step
      ! -----------------------------------------
      
      do k=1,N_st_diag
        if (state_ok(k)) then
          do i=1,sze
            U(i,shift2+k) = (lambda(k) * U(i,shift2+k) - W(i,shift2+k) )      &
                * (1.d0 + s2(k) * U(i,shift2+k) - S(i,shift2+k) - S_z2_Sz &
              )/max(H_jj(i) - lambda (k),1.d-2)
          enddo
        else
          ! Randomize components with bad <S2>
            do i=1,sze-2,2
              call random_number(r1)
              call random_number(r2)
              r1 = dsqrt(-2.d0*dlog(r1))
              r2 = dtwo_pi*r2
              U(i,shift2+k) = r1*dcos(r2)
              U(i+1,shift2+k) = r1*dsin(r2)
            enddo
            do i=sze-2+1,sze
              call random_number(r1)
              call random_number(r2)
              r1 = dsqrt(-2.d0*dlog(r1))
              r2 = dtwo_pi*r2
              U(i,shift2+k) = r1*dcos(r2)
            enddo
        endif

        if (k <= N_st) then
          residual_norm(k) = u_dot_u(U(1,shift2+k),sze)
          to_print(1,k) = lambda(k) + nuclear_repulsion
          to_print(2,k) = s2(k)
          to_print(3,k) = residual_norm(k)
        endif
      enddo
      
      write(iunit,'(X,I3,X,100(X,F16.10,X,F11.6,X,E11.3))')  iter, to_print(1:3,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      do k=1,N_st
        if (residual_norm(k) > 1.e8) then
        print *,  ''
          stop 'Davidson failed'
        endif
      enddo
      if (converged) then
        exit
      endif
      
    enddo

    ! Re-contract to u_in
    ! -----------
    
    call dgemm('N','N', sze, N_st_diag, shift2, 1.d0,      &
        U, size(U,1), y, size(y,1), 0.d0, u_in, size(u_in,1))

  enddo

  do k=1,N_st_diag
    energies(k) = lambda(k)
    S2_jj(k) = s2(k)
  enddo
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write(iunit,'(A)') ''
  call write_time(iunit)

  call munmap(                                                        &
      (/ int(sze_8,8),int(N_st_diag*itermax,8) /),                                &
      8, fd(1), c_pointer(1))
  
  call munmap(                                                        &
      (/ int(sze_8,8),int(N_st_diag*itermax,8) /),                                &
      8, fd(2), c_pointer(2))
  
  call munmap(                                                        &
      (/ int(sze_8,8),int(N_st_diag*itermax,8) /),                                &
      8, fd(3), c_pointer(3))

  deallocate (                                                       &
      residual_norm,                                                 &
      c, overlap,                                                    &
      h,                                                             &
      y, s_, s_tmp,                                                  &
      lambda                                                         &
      )
end

