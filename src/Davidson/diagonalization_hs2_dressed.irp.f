BEGIN_PROVIDER [ integer, dressed_column_idx, (N_states) ]
 implicit none
 BEGIN_DOC
 ! Index of the dressed columns
 END_DOC
 integer :: i
 double precision :: tmp
 integer, external :: idamax 
 do i=1,N_states
   dressed_column_idx(i) = idamax(size(psi_coef,1), psi_coef(1,i), 1)
 enddo
END_PROVIDER

subroutine davidson_diag_hs2(dets_in,u_in,s2_out,dim_in,energies,sze,N_st,N_st_diag,Nint,dressing_state)
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
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag), s2_out(N_st_diag)
  integer, intent(in)            :: dressing_state
  double precision, allocatable  :: H_jj(:), S2_jj(:)
  
  double precision, external     :: diag_H_mat_elem, diag_S_mat_elem
  integer                        :: i,k
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  PROVIDE mo_bielec_integrals_in_map
  allocate(H_jj(sze),S2_jj(sze))
  
  H_jj(1) = diag_h_mat_elem(dets_in(1,1,1),Nint)
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,H_jj, dets_in,Nint)                    &
      !$OMP  PRIVATE(i)
  !$OMP DO SCHEDULE(static)
  do i=2,sze
    H_jj(i)  = diag_H_mat_elem(dets_in(1,1,i),Nint)
  enddo
  !$OMP END DO 
  !$OMP END PARALLEL

  if (dressing_state > 0) then
    do k=1,N_st
      do i=1,sze
        H_jj(i) += u_in(i,k) * dressing_column_h(i,k)
      enddo
    enddo
  endif

  call davidson_diag_hjj_sjj(dets_in,u_in,H_jj,S2_out,energies,dim_in,sze,N_st,N_st_diag,Nint,dressing_state)
  deallocate (H_jj,S2_jj)
end


subroutine davidson_diag_hjj_sjj(dets_in,u_in,H_jj,s2_out,energies,dim_in,sze,N_st,N_st_diag,Nint,dressing_state)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization with specific diagonal elements of the H matrix
  !
  ! H_jj : specific diagonal H matrix elements to diagonalize de Davidson
  !
  ! S2_out : Output : s^2
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
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(sze)
  integer, intent(in)            :: dressing_state
  double precision,  intent(inout) :: s2_out(N_st_diag)
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision, external     :: u_dot_v, u_dot_u
  
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
  integer                        :: shift, shift2, itermax, istate
  double precision               :: r1, r2
  logical                        :: state_ok(N_st_diag*davidson_sze_max)
  include 'constants.include.F'
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: U, W, S, y, h, lambda
  if (N_st_diag*3 > sze) then
    print *,  'error in Davidson :'
    print *,  'Increase n_det_max_jacobi to ', N_st_diag*3
    stop -1
  endif
  
  itermax = max(3,min(davidson_sze_max, sze/N_st_diag))
  
  PROVIDE nuclear_repulsion expected_s2 psi_bilinear_matrix_order psi_bilinear_matrix_order_reverse
  
  call write_time(6)
  call wall_time(wall)
  call cpu_time(cpu)
  write(6,'(A)') ''
  write(6,'(A)') 'Davidson Diagonalization'
  write(6,'(A)') '------------------------'
  write(6,'(A)') ''
  call write_int(6,N_st,'Number of states')
  call write_int(6,N_st_diag,'Number of states in diagonalization')
  call write_int(6,sze,'Number of determinants')
  r1 = 8.d0*(3.d0*dble(sze*N_st_diag*itermax+5.d0*(N_st_diag*itermax)**2 & 
    + 4.d0*(N_st_diag*itermax)+nproc*(4.d0*N_det_alpha_unique+2.d0*N_st_diag*sze)))/(1024.d0**3)
  call write_double(6, r1, 'Memory(Gb)')
  write(6,'(A)') ''
  write_buffer = '====='
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(6,'(A)') write_buffer(1:6+41*N_st)
  write_buffer = 'Iter'
  do i=1,N_st
    write_buffer = trim(write_buffer)//'       Energy          S^2       Residual '
  enddo
  write(6,'(A)') write_buffer(1:6+41*N_st)
  write_buffer = '====='
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(6,'(A)') write_buffer(1:6+41*N_st)
  

  allocate(                                                          &
      ! Large
      W(sze,N_st_diag*itermax),                                    &
      U(sze,N_st_diag*itermax),                                    &
      S(sze,N_st_diag*itermax),                                    &

      ! Small
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
      
       
      if ((sze > 100000).and.distributed_davidson) then
          call H_S2_u_0_nstates_zmq   (W(1,shift+1),S(1,shift+1),U(1,shift+1),N_st_diag,sze)
      else
          call H_S2_u_0_nstates_openmp(W(1,shift+1),S(1,shift+1),U(1,shift+1),N_st_diag,sze)
      endif
      
      if (dressing_state > 0) then

        call dgemm('T','N', N_st, N_st_diag, sze, 1.d0, &
          psi_coef, size(psi_coef,1), &
          U(1,shift+1), size(U,1), 0.d0, s_tmp, size(s_tmp,1))

        call dgemm('N','N', sze, N_st_diag, N_st, 0.5d0, &
          dressing_column_h, size(dressing_column_h,1), s_tmp, size(s_tmp,1), &
          1.d0, W(1,shift+1), size(W,1))

        call dgemm('N','N', sze, N_st_diag, N_st, 0.5d0, &
          dressing_column_s, size(dressing_column_s,1), s_tmp, size(s_tmp,1), &
          1.d0, S(1,shift+1), size(S,1))


        call dgemm('T','N', N_st, N_st_diag, sze, 1.d0, &
          dressing_column_h, size(dressing_column_h,1), &
          U(1,shift+1), size(U,1), 0.d0, s_tmp, size(s_tmp,1))

        call dgemm('N','N', sze, N_st_diag, N_st, 0.5d0, &
          psi_coef, size(psi_coef,1), s_tmp, size(s_tmp,1), &
          1.d0, W(1,shift+1), size(W,1))

        call dgemm('T','N', N_st, N_st_diag, sze, 1.d0, &
          dressing_column_s, size(dressing_column_s,1), &
          U(1,shift+1), size(U,1), 0.d0, s_tmp, size(s_tmp,1))

        call dgemm('N','N', sze, N_st_diag, N_st, 0.5d0, &
          psi_coef, size(psi_coef,1), s_tmp, size(s_tmp,1), &
          1.d0, S(1,shift+1), size(S,1))

      endif

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
        do i=1,sze
          U(i,shift2+k) =  &
            (lambda(k) * U(i,shift2+k) - W(i,shift2+k) )      &
              * (1.d0 + s2(k) * U(i,shift2+k) - S(i,shift2+k) - S_z2_Sz &
            )/max(H_jj(i) - lambda (k),1.d-2)
        enddo

        if (k <= N_st) then
          residual_norm(k) = u_dot_u(U(1,shift2+k),sze)
          to_print(1,k) = lambda(k) + nuclear_repulsion
          to_print(2,k) = s2(k)
          to_print(3,k) = residual_norm(k)
        endif
      enddo
      
      write(6,'(1X,I3,1X,100(1X,F16.10,1X,F11.6,1X,E11.3))')  iter, to_print(1:3,1:N_st)
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
    s2_out(k) = s2(k)
  enddo
  write_buffer = '======'
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(6,'(A)') trim(write_buffer)
  write(6,'(A)') ''
  call write_time(6)

  deallocate (                                                       &
      W, residual_norm,                                              &
      U, overlap,                                                    &
      c, S,                                                          &
      h,                                                             &
      y, s_, s_tmp,                                                  &
      lambda                                                         &
      )
end







