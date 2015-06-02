subroutine write_pseudopotential
  implicit none
  BEGIN_DOC
!  Write the pseudo_potential into the EZFIO file
  END_DOC
!  call ezfio_set_pseudo_pseudo_matrix(pseudo_matrix)
!  call ezfio_set_pseudo_ao_pseudo_grid(ao_pseudo_grid)
  call ezfio_set_pseudo_mo_pseudo_grid(mo_pseudo_grid)
end


BEGIN_PROVIDER [ double precision, pseudo_matrix, (aux_basis_num_sqrt,aux_basis_num_sqrt) ]
 implicit none
 BEGIN_DOC
 ! Pseudo-potential expressed in the basis of ao products
 END_DOC

 integer :: i,j,k,l
 integer :: info, m,n, lwork, lda, ldu, ldvt
 integer, allocatable :: iwork(:)
 character :: jobz
 double precision, allocatable :: a(:,:),work(:)

 double precision,allocatable :: U(:,:)
 double precision,allocatable :: Vt(:,:)
 double precision,allocatable :: S(:), B(:)



 jobz = 'A'
 m = aux_basis_num
 n = aux_basis_num
 lda = size(aux_basis_overlap_matrix,1)
 ldu = lda
 ldvt = lda
 lwork = -1

! allocate (A(lda,n), U(ldu,n), Vt(ldvt,n), S(n), work(1), b(n), iwork(8*n))
 allocate (A(lda,n), U(ldu,n), Vt(ldvt,n), S(n), work(1), b(n),iwork(1))

 work(1) = 1
 do i=1,n
   do j=1,n
     A(i,j) = aux_basis_overlap_matrix(i,j)
   enddo
 enddo

! call dgesdd(jobz, m, n, A, lda, s, u, ldu, vt, ldvt, work, lwork, iwork, info)
 call dgesvd(jobz, jobz, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
 lwork = int(work(1))
 deallocate(work)

 print *,  'Fitting pseudo-potentials'

 allocate(work(lwork))
! call dgesdd(jobz, m, n, A, lda, s, u, ldu, vt, ldvt, work, lwork, iwork, info)
 call dgesvd(jobz, jobz, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
 deallocate(work)

 do i=1,n
 print *,  i, s(i)
 enddo

 do k=1,n
   if (s(k) < 1.d-1) then
     s(k) = 0.d0
   else
     s(k) = 1.d0/s(k)
   endif
   do m=1,n
     Vt(m,k) = S(m) * Vt(m,k) 
   enddo
 enddo
 call dgemm('N','N',n,n,n,1.d0,U,lda,Vt,ldvt,0.d0,A,lda)
! do k=1,n
!  do l=1,n
!   A(k,l) = 0.d0
!   do m=1,n
!    A(k,l) = A(k,l) + U(k,m) * Vt(m,l)
!   enddo
! enddo

 do k=1,n
   i = aux_basis_idx(1,k)
   j = aux_basis_idx(2,k)
   b(k) = aux_pseudo_integral(i,j)
 enddo

 do k=1,n
   S(k) = 0.d0
 enddo
 do l=1,n
   do k=1,n
     S(k) = S(k) + A(k,l) * b(l)
   enddo
 enddo

 do k=1,aux_basis_num
   i = aux_basis_idx(1,k)
   j = aux_basis_idx(2,k)
   pseudo_matrix(i,j) = S(k)
   pseudo_matrix(j,i) = S(k)
 enddo
 deallocate(a,b,s,iwork,u,vt)

print *,  'Done'
 if (info /= 0) then
   print *,  info
   stop 'pseudo fit failed'
 endif
END_PROVIDER





!BEGIN_PROVIDER [ double precision, pseudo_matrix, (ao_num,ao_num) ]
! implicit none
! BEGIN_DOC
! ! Pseudo-potential expressed in the basis of ao products
! END_DOC
!
! integer :: i,j,k
! integer :: info, n, lwork, lda, ldb, nrhs
! character :: uplo
! integer, allocatable :: ipiv(:)
! double precision, allocatable :: a(:,:),work(:), b(:)
!
! uplo = 'L'
! n = aux_basis_num
! nrhs = 1
! lda = size(aux_basis_overlap_matrix,1)
! ldb = n
! lwork = -1
!
! print *,  'Fitting pseudo-potentials'
! allocate(work(1),a(lda,n),ipiv(n),b(n))
! work(1) = 1
! do i=1,n
!   do j=1,n
!     a(i,j) = aux_basis_overlap_matrix(i,j)
!   enddo
! enddo
!
! do k=1,n
!   i = aux_basis_idx(1,k)
!   j = aux_basis_idx(2,k)
!   b(k) = ao_pseudo_integral(i,j)
! enddo
! call dsysv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work, lwork, info )
! lwork = int(work(1))
! deallocate(work)
!
! allocate(work(lwork))
! call dsysv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work, lwork, info )
! deallocate(work,ipiv)
! do k=1,aux_basis_num
!   i = aux_basis_idx(1,k)
!   j = aux_basis_idx(2,k)
!   pseudo_matrix(i,j) = b(k)
!   pseudo_matrix(j,i) = b(k)
! enddo
! deallocate(a,b)
!
!print *,  'Done'
! if (info /= 0) then
!   print *,  info
!   stop 'pseudo fit failed'
! endif
!END_PROVIDER

