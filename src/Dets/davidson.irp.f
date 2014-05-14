BEGIN_PROVIDER [ integer, davidson_iter_max]
  implicit none
  BEGIN_DOC
  ! Max number of Davidson iterations
  END_DOC
  davidson_iter_max = 100
END_PROVIDER

BEGIN_PROVIDER [ integer, davidson_sze_max]
  implicit none
  BEGIN_DOC
  ! Max number of Davidson sizes
  END_DOC
  ASSERT (davidson_sze_max <= davidson_iter_max)
  davidson_sze_max = 4
END_PROVIDER

subroutine davidson_diag(dets_in,u_in,energies,dim_in,sze,N_st,Nint)
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
  integer, intent(in)            :: dim_in, sze, N_st, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
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
  double precision, allocatable  :: W(:,:), H_jj(:), U(:,:,:), R(:,:)
  double precision, allocatable  :: y(:,:,:,:), h(:,:,:,:), lambda(:)
  double precision               :: diag_h_mat_elem
  double precision               :: residual_norm(N_st)
  
  allocate(                                                          &
      kl_pairs(2,N_st*(N_st+1)/2),                                   &
      H_jj(sze),                                                     &
      W(sze,N_st),                                                   &
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
      !$OMP  H_jj,Nint,dets_in,u_in)                                 &
      !$OMP  PRIVATE(k,l,kl,i)
  
  !$OMP DO
  do i=1,sze
    H_jj(i) = diag_h_mat_elem(dets_in(1,1,i),Nint)
  enddo
  !$OMP END DO NOWAIT
  
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
      print *,  'iter = ',iter
      
      !      print *,  '***************'
      !      do i=1,iter
      !       do k=1,N_st
      !         do j=1,iter
      !             do l=1,N_st
      !               print '(4(I4,X),F16.8)', i,j,k,l, u_dot_v(U(1,k,i),U(1,l,j),sze)
      !             enddo
      !           enddo
      !         enddo
      !       enddo
      !       print *,  '***************'
      
      ! Compute W_k = H |u_k>
      ! ----------------------
      
      do k=1,N_st
        call H_u_0(W(1,k),U(1,k,iter),H_jj,sze,dets_in,Nint)
      enddo
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------

      !$OMP PARALLEL 
      !$OMP SINGLE
      do l=1,N_st
        do iter2=1,iter-1
          do k=1,N_st
            !$OMP TASK FIRSTPRIVATE(k,iter,l,iter2)
            h(k,iter2,l,iter) = u_dot_v(U(1,k,iter2),W(1,l),sze)
            h(k,iter,l,iter2) = h(k,iter2,l,iter)
            !$OMP END TASK
          enddo
        enddo
        do k=1,l
          !$OMP TASK FIRSTPRIVATE(k,iter,l)
          h(k,iter,l,iter) = u_dot_v(U(1,k,iter),W(1,l),sze)
          h(l,iter,k,iter) = h(k,iter,l,iter)
          !$OMP END TASK
        enddo
      enddo
      !$OMP END SINGLE NOWAIT
      !$OMP TASKWAIT
      !$OMP END PARALLEL
      
      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,N_st*davidson_sze_max,N_st*iter)
      
      print *,  lambda(1:4)
      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      !TODO dgemm
      do k=1,N_st
        do i=1,sze
          U(i,k,iter+1) = 0.d0
          do iter2=1,iter
            do l=1,N_st
              U(i,k,iter+1) += U(i,l,iter2)*y(l,iter2,k,1)
            enddo
          enddo
        enddo
      enddo
      
      ! Compute residual vector
      ! -----------------------
      
      do k=1,N_st
        call H_u_0(W(1,k),U(1,k,iter+1),H_jj,sze,dets_in,Nint)
      enddo
      
      do k=1,N_st
        do i=1,sze
          R(i,k) = lambda(k) * U(i,k,iter+1) - W(i,k)
        enddo
        residual_norm(k) = u_dot_u(R(1,k),sze)
      enddo
      print *,  'Lambda'
      print *,  lambda(1:N_st) + nuclear_repulsion
      print *,  'Residual_norm'
      print *,  residual_norm(1:N_st)
      print *,  ''
      
      converged = maxval(residual_norm) < 1.d-10
      if (converged) then
        exit
      endif
      
      ! Davidson step
      ! -------------
      
      do k=1,N_st
        do i=1,sze
          U(i,k,iter+1) = 1.d0/(lambda(k) - H_jj(i)) * R(i,k)
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
    enddo
    
    if (.not.converged) then
      iter = davidson_sze_max
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

  deallocate (                                                       &
      kl_pairs,                                                      &
      H_jj,                                                          &
      W,                                                             &
      U,                                                             &
      R,                                                             &
      h,                                                             &
      y,                                                             &
      lambda                                                         &
      )
end

