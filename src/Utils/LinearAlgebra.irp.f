subroutine ortho_lowdin(overlap,lda,n,C,ldc,m)
  implicit none
  
  ! Compute U.S^-1/2 canonical orthogonalization
  
  integer, intent(in)            :: lda, ldc, n, m
  double precision, intent(in)   :: overlap(lda,n)
  double precision, intent(inout) :: C(ldc,n)
  double precision               :: U(ldc,n)
  double precision               :: Vt(lda,n)
  double precision               :: D(n)
  double precision               :: S_half(lda,n)
  double precision,allocatable   :: work(:)
  !DEC$ ATTRIBUTES ALIGN : 64    :: U, Vt, D, work
  integer                        :: info, lwork, i, j, k
  
  double precision,allocatable   :: overlap_tmp(:,:)
  allocate (overlap_tmp(lda,n))
  overlap_tmp = overlap
  
  allocate(work(1))
  lwork = -1
  call dgesvd('A','A', n, n, overlap_tmp, lda,                       &
      D, U, ldc, Vt, lda, work, lwork, info)
  lwork = work(1)
  deallocate(work)
  allocate(work(lwork))
  call dgesvd('A','A', n, n, overlap_tmp, lda,                       &
      D, U, ldc, Vt, lda, work, lwork, info)
  deallocate(work,overlap_tmp)
  if (info /= 0) then
    print *,  info, ': SVD failed'
    stop
  endif
  
  do i=1,n
    if ( D(i) < 1.d-6 ) then
      D(i) = 0.d0
    else
      D(i) = 1.d0/dsqrt(D(i))
    endif
  enddo
  
  S_half = 0.d0
  do k=1,n
    do j=1,n
      do i=1,n
        S_half(i,j) += U(i,k)*D(k)*Vt(k,j)
      enddo
    enddo
  enddo
  
  do j=1,n
    do i=1,m
      U(i,j) = C(i,j)
    enddo
  enddo
  
  C = 0.d0
  do j=1,n
    do i=1,m
      do k=1,n
        C(i,j) += U(i,k)*S_half(k,j)
      enddo
    enddo
  enddo
  
end



subroutine get_pseudo_inverse(A,m,n,C,LDA)
  ! Find C = A^-1
  implicit none
  integer, intent(in)            :: m,n, LDA
  double precision, intent(in)   :: A(LDA,n)
  double precision, intent(out)  :: C(n,m)
  
  double precision, allocatable  :: U(:,:), D(:), Vt(:,:), work(:), A_tmp(:,:)
  integer                        :: info, lwork
  integer                        :: i,j,k
  allocate (D(n),U(m,n),Vt(n,n),work(1),A_tmp(m,n))
  do j=1,n
    do i=1,m
      A_tmp(i,j) = A(i,j)
    enddo
  enddo
  lwork = -1
  call dgesvd('S','A', m, n, A_tmp, m,D,U,m,Vt,n,work,lwork,info)
  if (info /= 0) then
    print *,  info, ': SVD failed'
    stop
  endif
  lwork = work(1)
  deallocate(work)
  allocate(work(lwork))
  call dgesvd('S','A', m, n, A_tmp, m,D,U,m,Vt,n,work,lwork,info)
  if (info /= 0) then
    print *,  info, ': SVD failed'
    stop
  endif
  
  do i=1,n
    if (abs(D(i)) > 1.d-6) then
      D(i) = 1.d0/D(i)
    else
      D(i) = 0.d0
    endif
  enddo
  
  C = 0.d0
  do i=1,m
    do j=1,n
      do k=1,n
        C(j,i) += U(i,k) * D(k) * Vt(k,j)
      enddo
    enddo
  enddo
  
  deallocate(U,D,Vt,work,A_tmp)
  
end

subroutine find_rotation(A,LDA,B,m,C,n)
  ! Find A.C = B
  implicit none
  integer, intent(in)            :: m,n, LDA
  double precision, intent(in)   :: A(LDA,n), B(LDA,n)
  double precision, intent(out)  :: C(n,n)
  
  double precision, allocatable  :: A_inv(:,:)
  allocate(A_inv(LDA,n))
  call get_pseudo_inverse(A,m,n,A_inv,LDA)
  
  integer                        :: i,j,k
  call dgemm('N','N',n,n,m,1.d0,A_inv,n,B,LDA,0.d0,C,n)
  deallocate(A_inv)
end




subroutine apply_rotation(A,LDA,R,LDR,B,LDB,m,n)
  implicit none
  double precision, intent(in)   :: R(LDR,n)
  double precision, intent(in)   :: A(LDA,n)
  double precision, intent(out)  :: B(LDB,n)
  integer, intent(in)            :: m,n, LDA, LDB, LDR
  call dgemm('N','N',m,n,n,1.d0,A,LDA,R,LDR,0.d0,B,LDB)
end

subroutine jacobi_lapack(eigvalues,eigvectors,H,nmax,n)
  implicit none
  integer, intent(in)            :: n,nmax
  double precision, intent(out)  :: eigvectors(nmax,n)
  double precision, intent(out)  :: eigvalues(n)
  double precision, intent(in)   :: H(nmax,n)
  double precision,allocatable   :: eigenvalues(:)
  double precision,allocatable   :: work(:)
  double precision,allocatable   :: A(:,:)
  !eigvectors(i,j) = <d_i|psi_j> where d_i is the basis function and psi_j is the j th eigenvector
  print*,nmax,n
  allocate(A(nmax,n),eigenvalues(nmax),work(4*nmax))
  integer                        :: LWORK, info, i,j,l,k
  double precision               :: cpu_2, cpu_1
  A=H
  call cpu_time (cpu_1)
  LWORK = 4*nmax
  call dsyev( 'V',                                                   &
      'U',                                                           &
      n,                                                             &
      A,                                                             &
      nmax,                                                          &
      eigenvalues,                                                   &
      work,                                                          &
      LWORK,                                                         &
      info )
  if (info < 0) then
    print *, irp_here, ': the ',-info,'-th argument had an illegal value'
    stop
  else if (info > 0) then
    print *, irp_here, ': the algorithm failed to converge;  ',info,' off-diagonal'
    print *, 'elements of an intermediate tridiagonal form did not converge to zero.'
    stop
  endif
  call cpu_time (cpu_2)
  eigvectors = 0.d0
  eigvalues = 0.d0
  do j = 1, n
    eigvalues(j) = eigenvalues(j)
    do i = 1, n
      eigvectors(i,j) = A(i,j)
    enddo
  enddo
end
